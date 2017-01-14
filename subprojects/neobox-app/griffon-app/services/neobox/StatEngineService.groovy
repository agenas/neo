/*
 * Copyright 2014-2016 Stefano Gualdi, AGENAS.
 *
 * Licensed under the European Union Public Licence (EUPL), Version 1.1 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package neobox

import griffon.core.GriffonApplication
import griffon.core.artifact.GriffonService
import griffon.metadata.ArtifactProviderFor
import groovy.sql.Sql
import groovy.util.logging.Slf4j
import neobox.stat.DataPreparationException
import neobox.stat.Indicator
import neobox.stat.IndicatorsHolder
import neobox.utils.NeoboxUtils
import neobox.utils.WinRegistry
import org.apache.commons.lang.time.StopWatch
import org.yaml.snakeyaml.Yaml
import com.github.rcaller.rStuff.RCaller
import com.github.rcaller.rStuff.RCode
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

import static griffon.util.GriffonApplicationUtils.*

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */
@Slf4j
@ArtifactProviderFor(GriffonService)
class StatEngineService {

  @Inject
  GriffonApplication app

  @Inject
  DatabaseService databaseService
  def rScriptPath

  @PostConstruct
  void serviceInit() {
    rScriptPath = getRscriptPathForCurrentPlatform()

    log.debug "R script path set to: ${rScriptPath}"

    log.debug "Service inited"
  }

  @PreDestroy
  void serviceDestroy() {
    log.debug "Service destroyed"
  }

  void loadIndicators(File indicatorsDir, String language) {
    IndicatorsHolder indicatorsHolder = IndicatorsHolder.instance
    indicatorsHolder.clear()
    indicatorsHolder.parse(indicatorsDir, language)
  }

  Boolean runIndicators(List indicators, Map runningParams, File indicatorsDir, File workDir, File firstWorkDir, File libsDir) {
    IndicatorsHolder indicatorsHolder = IndicatorsHolder.instance

    StopWatch timer = new StopWatch()
    timer.start()

    def runList = indicatorsHolder.getExecutionList(indicators)

    app.eventRouter.publishEvent('RunIndicatorsStarted', [runList.size()])

    // Execute each indicator in the right order
    def result = true
    for (i in runList) {
      app.eventRouter.publishEvent('RunIndicatorsProgress', [i])

      def ind = indicatorsHolder.get(i)
      result = runIndicator(ind, runningParams, indicatorsDir, workDir, firstWorkDir, libsDir)
      if (!result) {
        log.error "Indicator ${i} failed!"
        break
      }
    }

    if (result) {
      app.eventRouter.publishEvent('RunIndicatorsFinished')
    }

    timer.stop()

    log("Overall execution time is ${timer.toString()}", 'complete')

    return result
  }

  Boolean runIndicator(Indicator indicator, Map runningParams, File indicatorsDir, File workDir, File firstWorkDir, File libsDir) {
    Boolean result = false

    StopWatch timer = new StopWatch()
    timer.start()

    // Prepare base directory for R libraries
    if (!libsDir.exists()) {
      libsDir.mkdirs()
    }

    // Prepare output/working directory for R script
    if (!workDir.exists()) {
      workDir.mkdirs()
    }
    def outputDir = new File(workDir, indicator.id)
    if (!outputDir.exists()) {
      outputDir.mkdirs()
    } else {
      // Cleanup directory
      outputDir.deleteDir()
      outputDir.mkdirs()
    }

    log("Running indicator ${indicator.id}", 'info')

    if (indicator.hasDataToPrepare() && runningParams.engine_type == 'local') {
      app.eventRouter.publishEvent('RunIndicatorsPhase', [indicator.id, 1])

      log("Data preparation start")

      if (firstWorkDir) {
        // Fast data preparation (copy input data from the first indicator)
        indicator.fastPrepareData(firstWorkDir, outputDir)

        log("Fast prepare data end in ${timer.toString()}")
      } else {
        // Select data as requested in specs
        Sql sql = databaseService.connect()
        try {
          indicator.prepareData(sql, outputDir)
        }
        catch (Exception e) {
          app.eventRouter.publishEvent('RunIndicatorError', [indicator.id])
          log("Data preparation error", "error")
          throw new DataPreparationException(app.messageSource.getMessage('application.message.indicators.error.dataPreparation'), e)
        }
        finally {
          sql.close()
        }

        log("Prepare data end in ${timer.toString()}")
      }
    } else {
      log("No data to prepare for indicator")
    }

    app.eventRouter.publishEvent('RunIndicatorsPhase', [indicator.id, 2])

    log.debug "Run R start"

    // Run R
    File scriptDir = new File(indicator.path)
    File scriptFile = new File(scriptDir, NeoboxUtils.INDICATOR_SOURCE_FILE)
    if (scriptFile.exists()) {
      try {
        execScript(scriptFile, runningParams, indicatorsDir, outputDir, libsDir)

        // Check if zip specifications are present and if present create the zip
        Yaml yaml = new Yaml()
        outputDir.eachFile { f ->
          if (f.name.endsWith(".zip.yml")) {
            def zipDesc = yaml.load(f.text)
            createZipFile(zipDesc, outputDir)
            if (zipDesc.cleanup) {
              f.delete()
            }
          }
        }

        // Check expected output
        result = indicator.outputFiles ? indicator.outputFiles.any { new File(outputDir, it).exists() } : true

        if (log.isDebugEnabled() && !result) {
          log.error "Expected output not found (${indicator.outputFiles})"
        }
      }
      catch (Exception e) {
        log.error e.message, e
        result = false
      }
    } else {
      log.debug "No code to run"
      result = true
    }

    if (result) {
      app.eventRouter.publishEvent('RunIndicatorsPhase', [indicator.id, 3])
    } else {
      log("Execution error", "error")
      app.eventRouter.publishEvent('RunIndicatorError', [indicator.id])
    }

    log.debug "Run R end"

    timer.stop()

    log("Done with indicator ${indicator.id} in ${timer.toString()}")

    return result
  }

  def execScript(File script, Map runningParams, File baseDir, File workDir, File libsDir) {
    String pScript = NeoboxUtils.convertToPlatformSpecificPath(script.absolutePath)
    String pBaseDir = NeoboxUtils.convertToPlatformSpecificPath(baseDir.absolutePath)
    String pWorkDir = NeoboxUtils.convertToPlatformSpecificPath(workDir.absolutePath)
    String pLibsDir = NeoboxUtils.convertToPlatformSpecificPath(libsDir.absolutePath)

    RCode code = new RCode()

    // Prepare env for R code
    code.addRCode("rm(list=ls(all=TRUE))")
    code.addRCode("baseDir <- \"${pBaseDir}\"")
    code.addRCode("workDir <- \"${pWorkDir}\"")
    code.addRCode("libsDir <- \"${pLibsDir}\"")

    // Add custom libraries path for R
    code.addRCode(".libPaths(c(\"${pLibsDir}\", .libPaths()))")

    // Define global parameters for R code
    def tmp
    runningParams.each { k, v ->
      if (v instanceof Collection) {
        if (!v.empty) {
          tmp = "${k} <- c(${v.collect { "\"${it}\"" }.join(",")})"
        } else {
          tmp = "${k} <- \"\""
        }
      } else {
        tmp = "${k} <- \"${v}\""
      }
      code.addRCode(tmp)
    }

    // Open logging
    code.addRCode("setwd(workDir)")

    code.addRCode("con <- file(\"indicator.log\")")
    code.addRCode("sink(con, append=FALSE)")
    code.addRCode("sink(con, append=FALSE, type=\"message\")")

    // Load indicator source code
    code.addRCode("source(\"${pScript}\")")

    // Close logging
    code.addRCode("sink()")
    code.addRCode("sink(type=\"message\")")

    // Init Rcaller
    RCaller rcaller = new RCaller();

    log.debug "R script path set to: ${rScriptPath}"

    rcaller.setRscriptExecutable(rScriptPath);

    // Cleanup
    rcaller.cleanRCode();

    // Init code
    rcaller.setRCode(code)

    log.debug "${rcaller.getRCode()}"

    // Excecute R code
    rcaller.runOnly()

    // Cleanup temp files
    rcaller.deleteTempFiles()
  }

  private String getRscriptPathForCurrentPlatform() {
    def rScript = null

    def options = NeoboxUtils.loadOptions()

    if (!options?.rpath) {
      if (isWindows) {
        log.debug "Found WINDOWS"
        String installPath = WinRegistry.readString(WinRegistry.HKEY_LOCAL_MACHINE, "SOFTWARE\\R-core\\R", "InstallPath", 0);
        rScript = "${installPath}${File.separator}bin${File.separator}${NeoboxUtils.R_WINDOWS_SCRIPT}"
        rScript = NeoboxUtils.convertToPlatformSpecificPath(rScript)
      } else if (isLinux) {
        log.debug "Found LINUX"
        rScript = "/usr/bin/${NeoboxUtils.R_UNIX_SCRIPT}"
      } else if (isMacOSX) {
        log.debug "Found OSX ${getOsVersion()}"
        if (NeoboxUtils.versionCompare(getOsVersion(), "10.10.5") == 1) {
          rScript = "/usr/local/bin/${NeoboxUtils.R_UNIX_SCRIPT}"
        } else {
          rScript = "/usr/bin/${NeoboxUtils.R_UNIX_SCRIPT}"
        }
      } else {
        log.debug "No OS Found"
        // Unsupported platform
        rScript = null
      }
    } else {
      log.debug "Get R script path from config file!"
      rScript = NeoboxUtils.convertToPlatformSpecificPath(options.rpath)
    }

    return rScript
  }

  private createZipFile(zipDesc, outputDir) {
    def zipFilename = zipDesc.file
    def cleanup = zipDesc.cleanup
    def files = zipDesc.files

    if (files.size > 0) {
      ByteArrayOutputStream baos = new ByteArrayOutputStream()
      ZipOutputStream zipFile = new ZipOutputStream(baos)
      File curFile
      def filename
      def renamedFilename
      def cleanupList = []
      files.each { f ->
        def parts = f.split(/\|/)
        if (parts.size() > 1) {
          filename = parts[0]
          renamedFilename = parts[1]
        } else {
          filename = f
          renamedFilename = f
        }

        curFile = new File(outputDir, filename)
        if (curFile.exists()) {
          zipFile.putNextEntry(new ZipEntry(renamedFilename))
          curFile.withInputStream { i -> zipFile << i }
          zipFile.closeEntry()

          if (cleanup) {
            cleanupList << curFile
          }
        } else {
          log("Missing file ${curFile.name} when creating ${zipFilename}", "error")
        }
      }
      zipFile.finish()

      OutputStream outputStream = new FileOutputStream(new File(outputDir, zipFilename))
      baos.writeTo(outputStream)
      outputStream.flush()
      outputStream.close()

      cleanupList.each { f ->
        f.delete()
      }
    }
  }

  private void log(String msg, String type = "", boolean consoleOnly = false) {
    app.eventRouter.publishEvent('WriteLog', [msg, type])
    if (!consoleOnly) {
      log.debug msg
    }
  }
}

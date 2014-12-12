/*
 * Copyright 2014 Stefano Gualdi, AGENAS.
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

import groovy.sql.Sql
import neobox.stat.DataPreparationException
import neobox.stat.Indicator
import neobox.stat.IndicatorsHolder
import neobox.utils.NeoboxUtils
import neobox.utils.WinRegistry
import org.apache.commons.lang.time.StopWatch
import org.yaml.snakeyaml.Yaml
import rcaller.RCaller
import rcaller.RCode

import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

import static griffon.util.GriffonApplicationUtils.*

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

class StatEngineService {

    def databaseService
    def rScriptPath

    void serviceInit() {
        rScriptPath = getRscriptPathForCurrentPlatform()

        log.debug "Service inited"
    }

    void serviceDestroy() {
        log.debug "Service destroyed"
    }

    void loadIndicators(File indicatorsDir, String language) {
        IndicatorsHolder indicatorsHolder = IndicatorsHolder.instance
        indicatorsHolder.clear()
        indicatorsHolder.parse(indicatorsDir, language)
    }

    Boolean runIndicators(List indicators, Map runningParams, File indicatorsDir, File workDir, File firstWorkDir) {
        IndicatorsHolder indicatorsHolder = IndicatorsHolder.instance

        StopWatch timer = new StopWatch()
        timer.start()

        def runList = indicatorsHolder.getExecutionList(indicators)

        app.event('RunIndicatorsStarted', [runList.size()])

        // Execute each indicator in the right order
        def result = true
        for (i in runList) {
            app.event('RunIndicatorsProgress', [i])

            def ind = indicatorsHolder.get(i)
            result = runIndicator(ind, runningParams, indicatorsDir, workDir, firstWorkDir)
            if (!result) {
                log.error "Indicator ${i} failed!"
                break
            }
        }

        if (result) {
            app.event('RunIndicatorsFinished')
        }

        timer.stop()

        log("Overall execution time is ${timer.toString()}", 'complete')

        return result
    }

    Boolean runIndicator(Indicator indicator, Map runningParams, File indicatorsDir, File workDir, File firstWorkDir) {
        Boolean result = false

        StopWatch timer = new StopWatch()
        timer.start()

        // Prepare output/working directory for R script
        if (!workDir.exists()) {
            workDir.mkdirs()
        }
        def outputDir = new File(workDir, indicator.id)
        if (!outputDir.exists()) {
            outputDir.mkdirs()
        }
        else {
            // Cleanup directory
            outputDir.deleteDir()
            outputDir.mkdirs()
        }

        log("Running indicator ${indicator.id}", 'info')

        if (indicator.hasDataToPrepare() && runningParams.engine_type == 'local') {
            app.event('RunIndicatorsPhase', [indicator.id, 1])

            log("Data preparation start")

            if (firstWorkDir) {
                // Fast data preparation (copy input data from the first indicator)
                indicator.fastPrepareData(firstWorkDir, outputDir)

                log("Fast prepare data end in ${timer.toString()}")
            }
            else {
                // Select data as requested in specs
                Sql sql = databaseService.connect()
                try {
                    indicator.prepareData(sql, outputDir)
                }
                catch (Exception e) {
                    app.event('RunIndicatorError', [indicator.id])
                    log("Data preparation error", "error")
                    throw new DataPreparationException(app.getMessage('application.message.indicators.error.dataPreparation', 'Data preparation error'), e)
                }
                finally {
                    sql.close()
                }

                log("Prepare data end in ${timer.toString()}")
            }
        }
        else {
            log("No data to prepare for indicator")
        }

        app.event('RunIndicatorsPhase', [indicator.id, 2])

        log.debug "Run R start"

        // Run R
        File scriptDir = new File(indicator.path)
        File scriptFile = new File(scriptDir, NeoboxUtils.INDICATOR_SOURCE_FILE)
        if (scriptFile.exists()) {
            try {
                execScript(scriptFile, runningParams, indicatorsDir, outputDir)

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
        }
        else {
            log.debug "No code to run"
            result = true
        }

        if (result) {
            app.event('RunIndicatorsPhase', [indicator.id, 3])
        }
        else {
            log("Execution error", "error")
            app.event('RunIndicatorError', [indicator.id])
        }

        log.debug "Run R end"

        timer.stop()

        log("Done with indicator ${indicator.id} in ${timer.toString()}")

        return result
    }

    def execScript(File script, Map runningParams, File baseDir, File workDir) {
        String pScript = NeoboxUtils.convertToPlatformSpecificPath(script.absolutePath)
        String pBaseDir = NeoboxUtils.convertToPlatformSpecificPath(baseDir.absolutePath)
        String pWorkDir = NeoboxUtils.convertToPlatformSpecificPath(workDir.absolutePath)

        RCode code = new RCode()

        // Prepare env for R code
        code.addRCode("rm(list=ls(all=TRUE))")
        code.addRCode("baseDir <- \"${pBaseDir}\"")
        code.addRCode("workDir <- \"${pWorkDir}\"")

        // Define global parameters for R code
        def tmp
        runningParams.each { k, v ->
            if (v instanceof Collection) {
                if (!v.empty) {
                    tmp = "${k} <- c(${v.collect { "\"${it}\"" }.join(",")})"
                }
                else {
                    tmp = "${k} <- \"\""
                }
            }
            else {
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

        rcaller.setRscriptExecutable(rScriptPath);

        log.debug "R script path set to: ${rScriptPath}"

        // Cleanup
        rcaller.cleanRCode();

        // Init code
        rcaller.setRCode(code)

        log.debug "${rcaller.getRCode()}"

        // Excecute R code
        rcaller.runOnly()
    }

    private String getRscriptPathForCurrentPlatform() {
        def rScript = null
        if (isWindows) {
            String installPath = WinRegistry.readString(WinRegistry.HKEY_LOCAL_MACHINE, "SOFTWARE\\R-core\\R", "InstallPath", 0);
            rScript = "${installPath}${File.separator}bin${File.separator}${NeoboxUtils.R_WINDOWS_SCRIPT}"
            rScript = NeoboxUtils.convertToPlatformSpecificPath(rScript)
        }
        else if (isLinux) {
            rScript = "/usr/bin/${NeoboxUtils.R_UNIX_SCRIPT}"
        }
        else if (isMacOSX) {
            rScript = "/usr/bin/${NeoboxUtils.R_UNIX_SCRIPT}"
        }
        else {
            // Unsupported platform
            rScript = null
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
                }
                else {
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
                }
                else {
                    log("Missing file ${curFile.name} when creating ${zipFilename}", "error")
                }
            }
            zipFile.finish()

            OutputStream outputStream = new FileOutputStream(new File(outputDir, zipFilename))
            baos.writeTo(outputStream)

            cleanupList.each { f ->
                f.delete()
            }
        }
    }

    private void log(String msg, String type = "", boolean consoleOnly = false) {
        app.event('WriteLog', [msg, type])
        if (!consoleOnly) {
            log.debug msg
        }
    }
}

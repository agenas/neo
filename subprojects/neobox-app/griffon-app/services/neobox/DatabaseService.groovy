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
import neobox.db.CSVFile
import neobox.db.MappingsHolder
import neobox.db.LookupsHelper
import neobox.db.SpecsFile
import neobox.db.ImportEnhancerCategory
import neobox.stat.IndicatorsHolder
import org.apache.commons.lang.time.StopWatch
import org.h2.tools.Server

import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import java.sql.ResultSet

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */
@Slf4j
@ArtifactProviderFor(GriffonService)
class DatabaseService {
  static String DATABASE_DRIVER = 'org.h2.Driver'
  static String DATABASE_NAME = 'neobox-tcp-db'
  static String DATABASE_PORT = '9123'
  static String DATABASE_USERNAME = ''
  static String DATABASE_PASSWORD = ''

  @Inject
  GriffonApplication app

  Server localServer = null
  String connectionURL

  @PostConstruct
  void serviceInit() {
    def args = ["-tcpPort", DATABASE_PORT, "-tcpAllowOthers"] as String[]
    localServer = Server.createTcpServer(args)
    localServer.start()

    // File wd = Metadata.current.getGriffonWorkingDir()
    // FIXME: trovare dir corrente
    File wd = new File(".")
    if (!wd.exists()) {
      wd.mkdirs()
    }

    connectionURL = "jdbc:h2:tcp://localhost:${DATABASE_PORT}/file://${new File(wd, DATABASE_NAME).absolutePath}"

    log.debug "Local db url: ${connectionURL}"

    log.debug "Service inited"
  }

  @PreDestroy
  void serviceDestroy() {
    if (localServer) {
      localServer.stop()
    }

    log.debug "Service destroyed"
  }

  Sql connect() {
    Sql.newInstance(connectionURL, DATABASE_USERNAME, DATABASE_PASSWORD, DATABASE_DRIVER)
  }

  void loadSpecs(File specsFile) {
    MappingsHolder mappingsHolder = MappingsHolder.instance
    mappingsHolder.clear()
    mappingsHolder.parse(specsFile)
  }

  def importAll(File file, Map context, Map lookupsFiles) {
    MappingsHolder mappingsHolder = MappingsHolder.instance

    LookupsHelper.instance.cleanup()

    StopWatch timer = new StopWatch()
    timer.start()

    def filesToImport = []

    lookupsFiles.each { k, v ->
      filesToImport << [
        specs  : mappingsHolder.get(k),
        file   : new File(v),
        context: [:]
      ]
    }

    if (file) {
      // Import THE UNIQUE master file after mappings files
      String masterSpecId = mappingsHolder.getMasterSpecs()[0].id
      filesToImport << [
        specs  : mappingsHolder.get(masterSpecId),
        file   : file,
        context: context
      ]
    }

    // Import all
    filesToImport.each { d ->
      importFile(d)
    }

    timer.stop()

    log("Overall import time is ${timer.toString()}", 'complete')
  }

  private importFile(Map importDef) {
    SpecsFile specs = importDef.specs

    File file = importDef.file
    String tableName = specs.id

    log("Import started for ${tableName}", 'info')

    StopWatch timer = new StopWatch()
    timer.start()

    Map context = importDef.context

    // Elenco dei campi richesti per l'import
    def fieldsToImport = specs.getFieldsMap()

    // Definizione del file csv da importare
    def csvFile = new CSVFile(file)

    // Mappatura ed indicizzazione dei campi presenti nel file csv
    def csvFieldsMap = csvFile.getColumnsMap(fieldsToImport.collect { it.name })

    // DDL di creazione tabella ed indici
    def createTableStmt = specs.createTableDDL()
    def createIndexesStmt = specs.createIndexesDDL()

    Sql sql = connect()

    // FIXME: ugly!!!
    LookupsHelper.instance.setDb(sql)

    sql.execute(createTableStmt)
    if (createIndexesStmt) {
      sql.execute(createIndexesStmt)
    }

    def fileSize = file.size()

    app.eventRouter.publishEvent('ImportStarted', [fileSize])

    def vars
    def values
    def varPlaceholder

    def record

    def missingLogged = [:]
    def recNo = 1
    def counter = 0


    new FileInputStream(file.absolutePath).toCsvReader([skipLines: 1]).eachLineCsv { tokens ->
      vars = []
      values = []
      varPlaceholder = []

      record = [:]

      fieldsToImport.each { f ->
        def fname = f.name.trim()

        // Import only non empty fields. We want NULL in the db to recognize missing values!
        def v
        if (f?.calculated) {
          try {
            use(ImportEnhancerCategory) {
              v = f.value(record, context)
            }
          }
          catch (Exception e) {
            v = null
            log.error("Cannot calculate field ${fname}", e)
          }
        } else {
          def fidx = csvFieldsMap[fname]
          if (fidx == null) {
            if (!missingLogged[fname]) {
              // To prevent too much logging tracks missing fields only once
              log("Missing field ${fname} in CSV file.", 'error')
              missingLogged[fname] = true
            }
            v = null
          } else {
            v = tokens[fidx]
          }
        }

        record[fname] = v

        if (v && f.persist) {
          vars << fname
          values << v
          varPlaceholder << "?"
        }
      }

      sql.execute("INSERT INTO ${tableName} (${vars.join(',')}) VALUES(${varPlaceholder.join(',')})", values)

      app.eventRouter.publishEvent('ImportProgressBarUpdate', [recNo, counter])

      recNo++
      counter += tokens.join(" ").size()
    }

    app.eventRouter.publishEvent('ImportFinished', [recNo - 1])

    sql.close()

    timer.stop()

    log("Import completed for ${tableName} - ${recNo - 1} records in ${timer.toString()}")
  }

  List collectUniqueValuesFor(String varName) {
    List result = []

    IndicatorsHolder indicators = IndicatorsHolder.instance

    Sql sql = connect()

    String masterTableName = indicators.getVariable(varName).table

    String stmt = "select distinct ${varName} from ${masterTableName} order by ${varName}".toString()
    sql.eachRow(stmt) { row ->
      def tmpRow = row.toRowResult()
      result << tmpRow[varName]
    }

    sql.close()

    return result
  }

  Map collectStats() {
    Map<String, Long> stats = [:]

    Sql sql = connect()

    // Collect all tables' names
    def md = sql.connection.metaData
    ResultSet rs = md.getTables(null, null, "%", ["TABLE"] as String[]);
    while (rs.next()) {
      stats[rs.getString(3)] = 0
    }

    // Count records for each table
    stats.keySet().each { k ->
      String countSql = "SELECT COUNT(*) AS numberOfRows FROM ${k}".toString()
      def countRows = sql.firstRow(countSql)
      stats[k] = countRows.numberOfRows
    }

    sql.close()

    return stats
  }

  private void log(String msg, String type = "", boolean consoleOnly = false) {
    app.eventRouter.publishEvent('WriteLog', [msg, type])
    if (!consoleOnly) {
      log.debug msg
    }
  }
}

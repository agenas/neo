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
package neobox.stat

import groovy.sql.Sql
import org.apache.commons.io.FileUtils
import au.com.bytecode.opencsv.CSVWriter

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */
class Indicator {
  String id
  String description

  Boolean hidden
  Boolean excludeReport

  List<String> dependsOn

  List<IndicatorInput> input = []

  List<String> outputFiles

  String path

  void addInput(IndicatorInput inputDef) {
    input << inputDef
  }

  void prepareData(Sql connection, File outputDir) {
    File outputFile
    String sqlStmt
    input?.each { inputDef ->
      outputFile = new File(outputDir, inputDef.file)
      sqlStmt = createSqlSelect(inputDef)
      writeCSV(connection, sqlStmt, outputFile)
    }
  }

  void fastPrepareData(File sourceDir, File outputDir) {
    File sourceIndicatorDir = new File(sourceDir, id)
    File fileToCopy
    input?.each { inputDef ->
      fileToCopy = new File(sourceIndicatorDir, inputDef.file)
      FileUtils.copyFileToDirectory(fileToCopy, outputDir)
    }
  }

  private void writeCSV(Sql connection, String sqlStmt, File outputFile) {
    outputFile.withWriter { writer ->
      CSVWriter csvWriter = new CSVWriter(writer)

      boolean writeHeader = true
      connection.eachRow(sqlStmt) { row ->
        def tmpRow = row.toRowResult()

        if (writeHeader) {
          // Write header
          csvWriter.writeNext(tmpRow.keySet() as String[])
          writeHeader = false
        }

        csvWriter.writeNext(tmpRow.values() as String[])
      }
    }
  }

  private String createSqlSelect(IndicatorInput inputDef) {
    def sqlStmt = "" << ""
    if (inputDef.sql) {
      sqlStmt << inputDef.sql.toString()
    } else {
      // Export data
      sqlStmt << "SELECT"
      sqlStmt << " "
      sqlStmt << inputDef.fields.join(',')
      sqlStmt << " "
      sqlStmt << "from " << inputDef.table

      if (inputDef.criteria) {
        sqlStmt << " "
        sqlStmt << "WHERE"
        sqlStmt << " "
        sqlStmt << inputDef.criteria.join(' AND ')
      }

      if (inputDef.groups) {
        sqlStmt << " "
        sqlStmt << "GROUP BY"
        sqlStmt << " "
        sqlStmt << inputDef.groups.join(',')
      }

      if (inputDef.order) {
        sqlStmt << " "
        sqlStmt << "ORDER BY"
        sqlStmt << " "
        sqlStmt << inputDef.order.join(',')
      }

      sqlStmt << ";"
    }

    return sqlStmt.toString()
  }

  boolean hasDataToPrepare() {
    input ? input?.any { it?.hasDataToPrepare() ?: false } : false
  }
}

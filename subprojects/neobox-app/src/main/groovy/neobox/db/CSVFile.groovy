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
package neobox.db

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */
class CSVFile {

  List<String> columns = []

  public CSVFile() {
  }

  public CSVFile(String csvFilename) {
    parse(csvFilename)
  }

  public CSVFile(File csvFile) {
    parse(csvFile)
  }

  void parse(String csvFilename) {
    parse(new File(csvFilename))
  }

  void parse(File csvFile) {
    def header = new FileReader(csvFile).readLine()
    columns = header?.split(/,/)
  }

  /**
   * Creates the fields map from the original CSV file
   *
   * @param fieldsToImport - fields to use for the indexing
   * @return
   */
  Map getColumnsMap(List<String> fieldsToImport = []) {
    def columnsMap = [:]
    int idx = 0
    boolean mapField
    columns.each { c ->
      mapField = fieldsToImport.empty ? true : (c in fieldsToImport)
      if (mapField) {
        columnsMap[c] = idx
      }
      idx++
    }
    return columnsMap
  }
}

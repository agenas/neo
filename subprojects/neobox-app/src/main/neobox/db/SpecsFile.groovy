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

package neobox.db

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */
class SpecsFile {
    String id
    String label
    Boolean mandatory
    Boolean master
    Boolean skipAutoId

    List<SpecsVariable> variables = []
    List<SpecsField> fields = []
    List<SpecsField> calculatedFields = []

    List<SpecsIndex> indexes = []

    void clear() {
        clearFields()
        clearCalculatedFields()
        clearVariables()
        clearIndexes()
    }

    void clearFields() {
        fields.clear()
    }

    void clearCalculatedFields() {
        calculatedFields.clear()
    }

    void clearVariables() {
        variables.clear()
    }

    void clearIndexes() {
        indexes.clear()
    }

    void addField(SpecsField f) {
        fields << f
    }

    void addCalculatedField(SpecsField f) {
        calculatedFields << f
    }

    void addVariable(SpecsVariable v) {
        variables << v
    }

    void addIndex(SpecsIndex v) {
        indexes << v
    }

    List getFieldsMap() {
        def map = []
        fields.each {
            map << [name: it.name, type: it.type, size: it.size, persist: it.persist]
        }
        calculatedFields.each {
            map << [name: it.name, type: it.type, size: it.size, value: it.value, calculated: true, persist: it.persist]
        }

        return map
    }

    List getVariablesMap() {
        def map = []
        variables.each {
            map << [name: it.name, type: it.type, label: it.label, mandatory: it.mandatory, value: ""]
        }

        return map
    }

    List getIndexesMap() {
        def map = []
        indexes.each {
            map << [name: it.name, primary: it.primary, unique: it.unique, fields: it.fields]
        }

        return map
    }

    String createTableDDL() {
        def ddl = "" << ""
        ddl << "DROP TABLE IF EXISTS ${id};\n"
        ddl << "CREATE TABLE ${id} (\n"

        if (!skipAutoId) {
            ddl << "id BIGINT AUTO_INCREMENT NOT NULL PRIMARY KEY,\n"
        }

        def primaryKeyFields = [:]
        indexes.each { idx ->
            idx?.fields.each { f ->
                if (idx.primary) {
                    primaryKeyFields[f.toUpperCase()] = true
                }
            }
        }

        def fieldsToCreate = []
        String type
        getFieldsMap().each { f ->
            if (f.persist) {
                if (f.type.toLowerCase() == "varchar") {
                    type = "varchar(${f.size})"
                }
                else {
                    type = f.type
                }

                // If the field is used in a primary index must be NOT NULLABLE
                fieldsToCreate << "${f.name} ${type} ${primaryKeyFields[f.name.toUpperCase()] ? 'NOT NULL' : 'NULL'}"
            }
        }

        ddl << "\n"
        ddl << fieldsToCreate.join(",\n")
        ddl << ");\n"

        return ddl.toString()
    }

    String createIndexesDDL() {
        def ddl = "" << ""

        getIndexesMap().each { idx ->
            if(idx.primary) {
                ddl << "CREATE PRIMARY KEY"
            }
            else {
                ddl << "CREATE ${idx.unique ? 'UNIQUE' : ''} INDEX ${idx.name}"
            }
            ddl << " ON ${id}"
            ddl << " ("
            ddl << idx.fields.join(",")
            ddl << ");\n"
        }

        return ddl.toString()
    }
}

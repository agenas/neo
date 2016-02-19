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

import groovy.util.logging.*

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

@Slf4j
@Singleton
class MappingsHolder {

  Map<String, SpecsFile> specsFiles = [:]

  void clear() {
    specsFiles.clear()
  }

  void parse(String specsFilename) {
    parse(new File(specsFilename))
  }

  void parse(File specsFile) {
    def config = new ConfigSlurper().parse(specsFile.toURL())

    def obj
    ['master', 'lookups'].each { section ->
      obj = config[section]
      def id
      obj.each { k, v ->
        id = k.trim()
        specsFiles[id] = parseSpecification(id, section, v)
      }
    }
  }

  private SpecsFile parseSpecification(String id, String section, ConfigObject spec) {
    def label = spec?.label
    def mandatory = spec.containsKey('mandatory') ? spec?.mandatory : true
    def skipAutoId = spec.containsKey('skipAutoId') ? spec?.skipAutoId : false
    def master = (section == 'master')

    SpecsFile specs = new SpecsFile(
      id: id,
      label: label,
      mandatory: mandatory,
      skipAutoId: skipAutoId,
      master: master
    )

    def fields = spec?.fields
    def field
    fields.keySet().each {
      field = new SpecsField(
        name: it,
        type: fields[it].type,
        size: fields[it]?.size ?: null,
        persist: true
      )
      specs.addField(field)
    }
    def calculatedFields = spec?.calculatedFields
    calculatedFields.keySet().each {
      field = new SpecsField(
        name: it,
        type: calculatedFields[it].type,
        size: calculatedFields[it]?.size ?: null,
        value: calculatedFields[it]?.value ?: null,
        persist: calculatedFields[it].containsKey('persist') ? calculatedFields[it]?.persist : true
      )
      specs.addCalculatedField(field)
    }

    def context = spec?.context
    def variable
    context.keySet().each {
      variable = new SpecsVariable(
        name: it,
        label: context[it].label,
        type: context[it].type,
        mandatory: context[it].containsKey('mandatory') ? context[it]?.mandatory : true
      )
      specs.addVariable(variable)
    }

    def indexes = spec?.indexes
    def idx
    indexes.keySet().each {
      idx = new SpecsIndex(
        name: it,
        primary: indexes[it].containsKey('primary') ? indexes[it]?.primary : false,
        unique: indexes[it].containsKey('unique') ? indexes[it]?.unique : false,
        fields: indexes[it]?.fields
      )
      specs.addIndex(idx)
    }

    return specs
  }

  SpecsFile get(String id) {
    specsFiles[id]
  }

  def getVariables() {
    def variables = []
    specsFiles.each { k, v ->
      v.getVariablesMap().each {
        variables << it
      }
    }
    return variables
  }

  def getMasterSpecs() {
    def result = []
    specsFiles.each { k, v ->
      if (v.master) {
        result << [id: v.id, label: v.label, mandatory: v.mandatory]
      }
    }
    return result
  }

  def getLookupsSpecs() {
    def result = []
    specsFiles.each { k, v ->
      if (!v.master) {
        result << [id: v.id, label: v.label, mandatory: v.mandatory]
      }
    }
    return result
  }
}

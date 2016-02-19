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

import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.WritableTableFormat
import ca.odell.glazedlists.swing.EventSelectionModel
import ca.odell.glazedlists.swing.EventTableModel
import griffon.core.artifact.GriffonView
import griffon.metadata.ArtifactProviderFor
import neobox.utils.AlphanumComparator

import neobox.utils.PersonalizedTableFormat

import javax.swing.BoxLayout
import java.awt.BorderLayout
import javax.swing.SwingConstants
import java.awt.GridBagConstraints

import static griffon.util.GriffonApplicationUtils.isMacOSX

// interface PersonalizedTableFormat extends AdvancedTableFormat, WritableTableFormat {}

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

@ArtifactProviderFor(GriffonView)
class NeoboxView {
  FactoryBuilderSupport builder
  NeoboxModel model

  void initUI() {
    builder.with {

      actions {
        action(importTheMatrixAction, enabled: bind { model.canImport })
        action(importLookupsAction, enabled: bind { model.canImportLookups })
        action(refreshIndicatorsAction, enabled: bind { model.canRefresh })
        action(runIndicatorsAction, enabled: bind { model.canRun })
        action(refreshFilesAction, enabled: bind { model.canRefreshFiles })
        action(transferFilesAction, enabled: bind { model.canTransferFiles })
        action(clearLogAction, enabled: bind { model.canClearLog })
      }

      def fileChooserWindow = fileChooser()

      def referenceSelectionModel = new EventSelectionModel(model.referenceFilesList)
      referenceSelectionModel.selectionMode = javax.swing.ListSelectionModel.SINGLE_SELECTION

      def inputSelectionModel = new EventSelectionModel(model.inputFilesList)
      inputSelectionModel.selectionMode = javax.swing.ListSelectionModel.SINGLE_SELECTION

      AlphanumComparator alfaComparator = new AlphanumComparator()

      application(id: 'mainWindow',
        title: 'NEObox',
        preferredSize: [800, 600],
        pack: true,

        //location: [50,50],
        locationByPlatform: true,

        iconImage: imageIcon('/neobox-icon-48x48.png').image,
        iconImages: [
          imageIcon('/neobox-icon-48x48.png').image,
          imageIcon('/neobox-icon-32x32.png').image,
          imageIcon('/neobox-icon-16x16.png').image
        ]) {

        menuBar {
          if (!isMacOSX) {
            menu(text: application.messageSource.getMessage('application.menu.file.name'),
              mnemonic: application.messageSource.getMessage('application.menu.file.mnemonic')) {
              menuItem(preferencesAction)
              separator()
              menuItem quitAction
            }
          }

          if (!isMacOSX) {
            menu(text: application.messageSource.getMessage('application.menu.help.name')) {
              menuItem aboutAction
            }
          }
        }

        migLayout(layoutConstraints: 'fill')

        // Content area
        tabbedPane(constraints: 'center, grow') {
          busyComponent(name: application.messageSource.getMessage('application.tab.config.name'),
            id: "c0",
            constraints: BorderLayout.CENTER,
            busy: bind { model.importing || model.running }) {
            busyModel(description: application.messageSource.getMessage('application.tab.config.busy.description'))
            panel() {
              borderLayout()
              panel(border: titledBorder(title: application.messageSource.getMessage('application.form.config.title')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 4dlu")

                // See filter in NeoboxController.mvcGroupInit()
                label(application.messageSource.getMessage('application.form.config.field.operatorName.label'), constraints: cc(x: 1, y: 1))
                textField(id: 'operatorName', text: bind('operatorName', target: model, mutual: true), constraints: cc(x: 3, y: 1))

                // See filter in NeoboxController.mvcGroupInit()
                label(application.messageSource.getMessage('application.form.config.field.year.label'), constraints: cc(x: 1, y: 3))
                textField(id: 'year', text: bind('year', target: model, mutual: true), constraints: cc(x: 3, y: 3))

                def languages = [
                  (application.messageSource.getMessage('application.form.config.field.mainLanguage.it.label')): 'it',
                  (application.messageSource.getMessage('application.form.config.field.mainLanguage.en.label')): 'en'
                ]
                label(application.messageSource.getMessage('application.form.config.field.mainLanguage.label'), constraints: cc(x: 1, y: 5))
                comboBox(selectedItem: bind(target: model, targetProperty: "mainLanguage", mutual: true,
                  converter: { v -> languages[v] },
                  reverseConverter: { v -> languages.find { it.value == v }?.key }
                ),
                  items: languages.collect(['']) { k, v -> k },
                  constraints: cc(x: 3, y: 5)
                )
              }

              panel() {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: application.messageSource.getMessage('application.form.config.tables.title'))) {
                  table(id: "tablesTable", model: createTablesTableModel())
                }
              }

              hbox(constraints: SOUTH) {
                button(savePreferencesAction)
              }
            }
          }

          busyComponent(name: application.messageSource.getMessage('application.tab.import.name'),
            id: "c1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.importing }) {
            busyModel(description: application.messageSource.getMessage('application.tab.import.busy.description'))
            panel() {
              borderLayout()
              panel(constraints: NORTH) {
                boxLayout(axis: BoxLayout.Y_AXIS)
                panel(border: titledBorder(title: application.messageSource.getMessage('application.form.import.title'))) {
                  formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 4dlu")

                  label(application.messageSource.getMessage('application.form.import.field.csvFile.label'), constraints: cc(x: 1, y: 1))
                  textField(text: bind('csvFile', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                  button(action: selectCsvFileAction, constraints: cc(x: 5, y: 1))

                  label(application.messageSource.getMessage('application.form.import.field.specsFile.label'), constraints: cc(x: 1, y: 3))
                  textField(text: bind('specsFile', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 3))
                  button(action: selectSpecsFileAction, constraints: cc(x: 5, y: 3))
                }

                panel(id: 'moreFiles', border: titledBorder(title: application.messageSource.getMessage('application.form.import.lookups.title')), visible: false) {
                }
              }

              panel(constraints: CENTER) {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: application.messageSource.getMessage('application.form.import.variables.title'))) {
                  table(id: "variablesTable", model: createVariablesTableModel())
                }
              }

              hbox(constraints: SOUTH) {
                button(importTheMatrixAction)
                button(importLookupsAction, visible: bind { model.canImportLookups })
              }
            }
          }

          busyComponent(name: application.messageSource.getMessage('application.tab.indicators.name'),
            id: "i1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.running }) {
            busyModel(description: application.messageSource.getMessage('application.tab.indicators.busy.description'))
            panel() {
              borderLayout()
              panel(border: titledBorder(title: application.messageSource.getMessage('application.form.indicators.title')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 4dlu")

                label(application.messageSource.getMessage('application.form.indicators.field.indicatorsDir.label'), constraints: cc(x: 1, y: 1))
                textField(text: bind('indicatorsDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                button(action: selectIndicatorsDirAction, constraints: cc(x: 5, y: 1))

                label(application.messageSource.getMessage('application.form.indicators.field.workDir.label'), constraints: cc(x: 1, y: 3))
                textField(text: bind('workDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 3))
                button(action: selectWorkDirAction, constraints: cc(x: 5, y: 3))

                def languages = [
                  (application.messageSource.getMessage('application.form.indicators.field.language.it.label')): 'it',
                  (application.messageSource.getMessage('application.form.indicators.field.language.en.label')): 'en'
                ]
                label(application.messageSource.getMessage('application.form.indicators.field.language.label'), constraints: cc(x: 1, y: 5))
                comboBox(selectedItem: bind(target: model, targetProperty: "language", mutual: true,
                  converter: { v -> languages[v] },
                  reverseConverter: { v -> languages.find { it.value == v }?.key }
                ),
                  items: languages.collect(['']) { k, v -> k },
                  constraints: cc(x: 3, y: 5)
                )

                label(application.messageSource.getMessage('application.form.indicators.field.selectUnit.label'), constraints: cc(x: 1, y: 7))
                panel(constraints: cc(x: 3, y: 7)) {
                  formLayout(columns: "right:pref, 4dlu, fill:50dlu:grow, min", rows: "pref")
                  comboBox(selectedItem: bind(target: model, targetProperty: "selectUnitVariable"),
                    model: eventComboBoxModel(source: model.selectUnitVariableList),
                    constraints: cc(x: 1, y: 1)
                  )
                  textField(text: bind('selectUnitValue', target: model), constraints: cc(x: 3, y: 1))
                }

                label(application.messageSource.getMessage('application.form.indicators.field.highlights.label'), constraints: cc(x: 1, y: 9))
                panel(constraints: cc(x: 3, y: 9)) {
                  formLayout(columns: "right:pref, 4dlu, fill:50dlu:grow, min", rows: "pref")
                  comboBox(selectedItem: bind(target: model, targetProperty: "highlightsVariable"),
                    model: eventComboBoxModel(source: model.selectUnitVariableList),
                    constraints: cc(x: 1, y: 1)
                  )
                  textField(text: bind('highlightsValue', target: model), constraints: cc(x: 3, y: 1))
                }

                def referenceTypes = [
                  (application.messageSource.getMessage('application.form.indicators.field.reference.none.label'))    : 'none',
                  (application.messageSource.getMessage('application.form.indicators.field.reference.internal.label')): '_internal_',
                  (application.messageSource.getMessage('application.form.indicators.field.reference.external.label')): '_external_'
                ]
                label(application.messageSource.getMessage('application.form.indicators.field.reference.label'), constraints: cc(x: 1, y: 11))
                comboBox(selectedItem: bind(target: model, targetProperty: "referenceType", mutual: true,
                  converter: { v -> referenceTypes[v] },
                  reverseConverter: { v -> referenceTypes.find { it.value == v }?.key }
                ),
                  items: referenceTypes.collect(['']) { k, v -> k },
                  actionPerformed: {
                    // Fix panel update!!!
                    builder.indicatorsPanel.revalidate()
                    builder.indicatorsPanel.repaint()
                  },
                  constraints: cc(x: 3, y: 11)
                )

                def engineTypes = [
                  (application.messageSource.getMessage('application.form.indicators.field.engineType.local.label'))  : 'local',
                  (application.messageSource.getMessage('application.form.indicators.field.engineType.central.label')): 'central'
                ]
                label(application.messageSource.getMessage('application.form.indicators.field.engineType.label'), constraints: cc(x: 1, y: 13))
                comboBox(selectedItem: bind(target: model, targetProperty: "engineType", mutual: true,
                  converter: { v -> engineTypes[v] },
                  reverseConverter: { v -> engineTypes.find { it.value == v }?.key }
                ),
                  items: engineTypes.collect(['']) { k, v -> k },
                  constraints: cc(x: 3, y: 13)
                )
              }

              panel(id: 'indicatorsPanel') {
                boxLayout(axis: BoxLayout.Y_AXIS)
                panel(border: titledBorder(title: application.messageSource.getMessage('application.form.indicators.referenceFiles.title')), visible: bind(source: model, sourceProperty: 'canShowExternalFiles')) {
                  boxLayout(axis: BoxLayout.X_AXIS)
                  scrollPane() {
                    table(id: "referenceFilesTable", model: createReferenceFilesTableModel(), selectionModel: referenceSelectionModel)
                  }
                  noparent {
                    referenceSelectionModel.selected.addListEventListener(model.referenceFilesListSelection)
                  }
                  panel() {
                    boxLayout(axis: BoxLayout.Y_AXIS)
                    button(addReferenceFileAction)
                    button(delReferenceFileAction)
                  }
                }
                panel(border: titledBorder(title: application.messageSource.getMessage('application.form.indicators.inputFiles.title')), visible: bind(source: model, sourceProperty: 'canShowInputFiles')) {
                  boxLayout(axis: BoxLayout.X_AXIS)
                  scrollPane() {
                    table(id: "inputFilesTable", model: createInputFilesTableModel(), selectionModel: inputSelectionModel)
                  }
                  noparent {
                    inputSelectionModel.selected.addListEventListener(model.inputFilesListSelection)
                  }
                  panel() {
                    boxLayout(axis: BoxLayout.Y_AXIS)
                    button(addInputFileAction)
                    button(delInputFileAction)
                  }
                }
                scrollPane(border: titledBorder(title: application.messageSource.getMessage('application.form.indicators.indicators.title'))) {
                  table(id: "indicatorsTable", model: createIndicatorsTableModel())
                }
              }

              hbox(constraints: SOUTH) {
                button(refreshIndicatorsAction)
                button(runIndicatorsAction)
              }
            }
          }

          busyComponent(name: application.messageSource.getMessage('application.tab.transfer.name'),
            id: "t1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.transferring }) {
            busyModel(description: application.messageSource.getMessage('application.tab.transfer.busy.description'))
            panel() {
              borderLayout()
              panel(border: titledBorder(title: application.messageSource.getMessage('application.form.transfer.title')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref,4dlu")

                label(application.messageSource.getMessage('application.form.transfer.field.workDir.label'), constraints: cc(x: 1, y: 1))
                textField(text: bind('workDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                button(action: selectWorkDirAction, constraints: cc(x: 5, y: 1))

                label(application.messageSource.getMessage('application.form.transfer.field.server.label'), constraints: cc(x: 1, y: 3))
                textField(text: bind('server', target: model, mutual: true), constraints: cc(x: 3, y: 3))
                button(action: savePreferencesAction, constraints: cc(x: 5, y: 3))

                label(application.messageSource.getMessage('application.form.transfer.field.username.label'), constraints: cc(x: 1, y: 5))
                textField(text: bind('username', target: model, mutual: true), constraints: cc(x: 3, y: 5))

                label(application.messageSource.getMessage('application.form.transfer.field.password.label'), constraints: cc(x: 1, y: 7))
                passwordField(text: bind('password', target: model, mutual: true), constraints: cc(x: 3, y: 7))

                label(application.messageSource.getMessage('application.form.transfer.field.activeFtp.label'), constraints: cc(x: 1, y: 9))
                checkBox(selected: bind('activeFtp', target: model, mutual: true), constraints: cc(x: 3, y: 9))
              }

              panel() {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: application.messageSource.getMessage('application.form.transfer.files.title'))) {
                  table(id: "filesTable", model: createFilesTableModel())
                }
              }

              hbox(constraints: SOUTH) {
                button(refreshFilesAction)
                button(transferFilesAction)
              }
            }
          }

          panel(name: application.messageSource.getMessage('application.tab.log.name'), constraints: BorderLayout.CENTER) {
            borderLayout()
            panel(border: titledBorder(title: "Log", constraints: CENTER)) {
              boxLayout(axis: BoxLayout.Y_AXIS)
              scrollPane() {
                textPane(id: 'logArea', editable: false)
              }
            }

            hbox(constraints: SOUTH) {
              button(clearLogAction)
            }
          }
        }

        // Status bar
        vbox(constraints: 'south, grow') {
          separator()
          panel {
            gridBagLayout()
            label(id: 'status', text: bind { model.status },
              constraints: gbc(weightx: 1.0,
                anchor: GridBagConstraints.WEST,
                fill: GridBagConstraints.HORIZONTAL,
                insets: [1, 3, 1, 3])
            )
            separator(orientation: SwingConstants.VERTICAL, constraints: gbc(fill: GridBagConstraints.VERTICAL))
            progressBar id: 'progressBar', value: bind { model.current }, indeterminate: false,
              minimum: bind { model.minProgressBar }, maximum: bind { model.maxProgressBar }
            constraints:
            gbc(insets: [1, 3, 1, 3])
          }
        }
      }
    }
  }

  def createTablesTableModel() {
    def columnFields = ["id", "count"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.tables.${it}.name", it.capitalize())
    }

    new EventTableModel(model.tablesList, [
      getColumnCount: { columnNames.size() },
      getColumnName : { index -> columnNames[index] },
      getColumnValue: { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass: { index ->
        if (index == 1) {
          Long.class
        } else {
          String.class
        }
      },
      isEditable    : { object, index ->
        false
      }
    ] as PersonalizedTableFormat)
  }

  def createVariablesTableModel() {
    def columnFields = ["name", "type", "label", "mandatory", "value"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.variables.${it}.name", it.capitalize())
    }
    new EventTableModel(model.variablesList, [
      getColumnCount: { columnNames.size() },
      getColumnName : { index -> columnNames[index] },
      getColumnValue: { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass: { index ->
        if (index == 3) {
          Boolean.class
        } else if (index == 4) {
          String.class
        } else {
          Object.class
        }
      },
      isEditable    : { object, index ->
        index == 4
      },
      setColumnValue: { object, cell, index ->
        def item = model.variablesList.find { it.name == object.name }
        item.value = cell
        return item
      }
    ] as PersonalizedTableFormat)
  }

  def createIndicatorsTableModel() {
    def columnFields = ["selected", "id", "description"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.indicators.${it}.name", it.capitalize())
    }
    new EventTableModel(model.indicatorsList, [
      getColumnCount     : { columnNames.size() },
      getColumnName      : { index -> columnNames[index] },
      getColumnValue     : { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass     : { index ->
        index == 0 ? Boolean.class : Object.class
      },
      isEditable         : { object, index ->
        index == 0
      },
      setColumnValue     : { object, cell, index ->
        def item = model.indicatorsList.find { it.id == object.id }
        item.selected = !item.selected
        return item
      },
      getColumnComparator: { column ->
        if (column == 1) {
          return alphaComparator
        }
      }
    ] as PersonalizedTableFormat)
  }

  def createReferenceFilesTableModel() {
    def columnFields = ["name"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.referenceFiles.${it}.name", it.capitalize())
    }
    new EventTableModel(model.referenceFilesList, [
      getColumnCount     : { columnNames.size() },
      getColumnName      : { index -> columnNames[index] },
      getColumnValue     : { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass     : { index ->
        index == 0 ? String.class : Object.class
      },
      isEditable         : { object, index ->
        false
      },
      getColumnComparator: { column ->
        if (column == 1) {
          return alphaComparator
        }
      }
    ] as PersonalizedTableFormat)
  }

  def createInputFilesTableModel() {
    def columnFields = ["name"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.inputFiles.${it}.name", it.capitalize())
    }
    new EventTableModel(model.inputFilesList, [
      getColumnCount     : { columnNames.size() },
      getColumnName      : { index -> columnNames[index] },
      getColumnValue     : { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass     : { index ->
        index == 0 ? String.class : Object.class
      },
      isEditable         : { object, index ->
        false
      },
      getColumnComparator: { column ->
        if (column == 1) {
          return alphaComparator
        }
      }
    ] as PersonalizedTableFormat)
  }

  def createFilesTableModel() {
    def columnFields = ["selected", "name", "path", "date"]
    def columnNames = columnFields.collect {
      application.messageSource.getMessage("application.table.files.${it}.name", it.capitalize())
    }
    new EventTableModel(model.filesList, [
      getColumnCount     : { columnNames.size() },
      getColumnName      : { index -> columnNames[index] },
      getColumnValue     : { object, index ->
        object."${columnFields[index]}"
      },
      getColumnClass     : { index ->
        index == 0 ? Boolean.class : Object.class
      },
      isEditable         : { object, index ->
        index == 0
      },
      setColumnValue     : { object, cell, index ->
        def item = model.filesList.find { it.id == object.id }
        item.selected = !item.selected
        return item
      },
      getColumnComparator: { column ->
        if (column == 1) {
          return alphaComparator
        }
      }
    ] as PersonalizedTableFormat)
  }

}

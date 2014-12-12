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

import neobox.utils.AlphanumComparator
import javax.swing.BoxLayout
import javax.swing.ListSelectionModel
import java.awt.*
import ca.odell.glazedlists.gui.*
import ca.odell.glazedlists.swing.*

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

interface PersonalizedTableFormat extends AdvancedTableFormat, WritableTableFormat {}

AlphanumComparator alfaComparator = new AlphanumComparator()

def createTablesTableModel() {
    def columnFields = ["id", "count"]
    def columnNames = columnFields.collect { app.getMessage("application.table.tables.${it}.name", it.capitalize()) }

    new EventTableModel(model.tablesList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                if (index == 1) {
                    Long.class
                }
                else {
                    String.class
                }
            },
            isEditable: { object, index ->
                false
            }
    ] as PersonalizedTableFormat)
}

def createVariablesTableModel() {
    def columnFields = ["name", "type", "label", "mandatory", "value"]
    def columnNames = columnFields.collect { app.getMessage("application.table.variables.${it}.name", it.capitalize()) }
    new EventTableModel(model.variablesList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                if (index == 3) {
                    Boolean.class
                }
                else if (index == 4) {
                    String.class
                }
                else {
                    Object.class
                }
            },
            isEditable: { object, index ->
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
    def columnNames = columnFields.collect { app.getMessage("application.table.indicators.${it}.name", it.capitalize()) }
    new EventTableModel(model.indicatorsList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                index == 0 ? Boolean.class : Object.class
            },
            isEditable: { object, index ->
                index == 0
            },
            setColumnValue: { object, cell, index ->
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

def referenceSelectionModel = new EventSelectionModel(model.referenceFilesList)
referenceSelectionModel.selectionMode = ListSelectionModel.SINGLE_SELECTION

def createReferenceFilesTableModel() {
    def columnFields = ["name"]
    def columnNames = columnFields.collect { app.getMessage("application.table.referenceFiles.${it}.name", it.capitalize()) }
    new EventTableModel(model.referenceFilesList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                index == 0 ? String.class : Object.class
            },
            isEditable: { object, index ->
                false
            },
            getColumnComparator: { column ->
                if (column == 1) {
                    return alphaComparator
                }
            }
    ] as PersonalizedTableFormat)
}

def inputSelectionModel = new EventSelectionModel(model.inputFilesList)
inputSelectionModel.selectionMode = ListSelectionModel.SINGLE_SELECTION

def createInputFilesTableModel() {
    def columnFields = ["name"]
    def columnNames = columnFields.collect { app.getMessage("application.table.inputFiles.${it}.name", it.capitalize()) }
    new EventTableModel(model.inputFilesList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                index == 0 ? String.class : Object.class
            },
            isEditable: { object, index ->
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
    def columnNames = columnFields.collect { app.getMessage("application.table.files.${it}.name", it.capitalize()) }
    new EventTableModel(model.filesList, [
            getColumnCount: { columnNames.size() },
            getColumnName: { index -> columnNames[index] },
            getColumnValue: { object, index ->
                object."${columnFields[index]}"
            },
            getColumnClass: { index ->
                index == 0 ? Boolean.class : Object.class
            },
            isEditable: { object, index ->
                index == 0
            },
            setColumnValue: { object, cell, index ->
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

tabbedPane() {
    busyComponent(name: app.getMessage('application.tab.config.name', 'Configuration'),
            id: "c0",
            constraints: BorderLayout.CENTER,
            busy: bind { model.importing || model.running }) {
        busyModel(description: app.getMessage('application.tab.config.busy.description', 'Working...'))
        panel() {
            borderLayout()
            panel(border: titledBorder(title: app.getMessage('application.form.config.title', 'Configuration options')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 4dlu")

                // See filter in NeoboxController.mvcGroupInit()
                label(app.getMessage('application.form.config.field.operatorName.label', 'Operator name'), constraints: cc(x: 1, y: 1))
                textField(id: 'operatorName', text: bind('operatorName', target: model, mutual: true), constraints: cc(x: 3, y: 1))

                // See filter in NeoboxController.mvcGroupInit()
                label(app.getMessage('application.form.config.field.year.label', 'Year'), constraints: cc(x: 1, y: 3))
                textField(id: 'year', text: bind('year', target: model, mutual: true), constraints: cc(x: 3, y: 3))

                def languages = [
                    (app.getMessage('application.form.config.field.mainLanguage.it.label', 'Italian')): 'it',
                    (app.getMessage('application.form.config.field.mainLanguage.en.label', 'English')): 'en'
                ]
                label(app.getMessage('application.form.config.field.mainLanguage.label', 'Language'), constraints: cc(x: 1, y: 5))
                comboBox(selectedItem: bind(target: model, targetProperty: "mainLanguage", mutual: true,
                        converter: {v -> languages[v]},
                        reverseConverter: {v -> languages.find { it.value == v }?.key}
                    ),
                    items: languages.collect(['']) {k, v -> k},
                    constraints: cc(x: 3, y: 5)
                )
            }

            panel() {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: app.getMessage('application.form.config.tables.title', 'Tables'))) {
                    table(id: "tablesTable", model: createTablesTableModel())
                }
            }

            hbox(constraints: SOUTH) {
                button(saveAction)
            }
        }
    }

    busyComponent(name: app.getMessage('application.tab.import.name', 'Import'),
            id: "c1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.importing }) {
        busyModel(description: app.getMessage('application.tab.import.busy.description', 'Importing...'))
        panel() {
            borderLayout()
            panel(constraints: NORTH) {
                boxLayout(axis: BoxLayout.Y_AXIS)
                panel(border: titledBorder(title: app.getMessage('application.form.import.title', 'Import options'))) {
                    formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 4dlu")

                    label(app.getMessage('application.form.import.field.csvFile.label', 'The Matrix CSV file'), constraints: cc(x: 1, y: 1))
                    textField(text: bind('csvFile', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                    button(action: selectCsvFileAction, constraints: cc(x: 5, y: 1))

                    label(app.getMessage('application.form.import.field.specsFile.label', 'Import specifications'), constraints: cc(x: 1, y: 3))
                    textField(text: bind('specsFile', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 3))
                    button(action: selectSpecsFileAction, constraints: cc(x: 5, y: 3))
                }

                panel(id: 'moreFiles', border: titledBorder(title: app.getMessage('application.form.import.lookups.title', 'Lookups tables')), visible: false) {
                }
            }

            panel(constraints: CENTER) {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: app.getMessage('application.form.import.variables.title', 'Import variables'))) {
                    table(id: "variablesTable", model: createVariablesTableModel())
                }
            }

            hbox(constraints: SOUTH) {
                button(importAction)
                button(importLookupsAction, visible: bind {model.canImportLookups} )
            }
        }
    }

    busyComponent(name: app.getMessage('application.tab.indicators.name', 'Indicators'),
            id: "i1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.running }) {
        busyModel(description: app.getMessage('application.tab.indicators.busy.description', 'Running...'))
        panel() {
            borderLayout()
            panel(border: titledBorder(title: app.getMessage('application.form.indicators.title', 'Running options')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 4dlu")

                label(app.getMessage('application.form.indicators.field.indicatorsDir.label', 'Indicators directory'), constraints: cc(x: 1, y: 1))
                textField(text: bind('indicatorsDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                button(action: selectIndicatorsDirAction, constraints: cc(x: 5, y: 1))

                label(app.getMessage('application.form.indicators.field.workDir.label', 'Working directory'), constraints: cc(x: 1, y: 3))
                textField(text: bind('workDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 3))
                button(action: selectWorkDirAction, constraints: cc(x: 5, y: 3))

                def languages = [
                    (app.getMessage('application.form.indicators.field.language.it.label', 'Italian')): 'it',
                    (app.getMessage('application.form.indicators.field.language.en.label', 'English')): 'en'
                ]
                label(app.getMessage('application.form.indicators.field.language.label', 'Language'), constraints: cc(x: 1, y: 5))
                comboBox(selectedItem: bind(target: model, targetProperty: "language", mutual: true,
                        converter: {v -> languages[v]},
                        reverseConverter: {v -> languages.find { it.value == v }?.key}
                    ),
                    items: languages.collect(['']) {k, v -> k},
                    constraints: cc(x: 3, y: 5)
                )

                label(app.getMessage('application.form.indicators.field.selectUnit.label', 'Select unit'), constraints: cc(x: 1, y: 7))
                panel(constraints: cc(x: 3, y: 7)) {
                    formLayout(columns: "right:pref, 4dlu, fill:50dlu:grow, min", rows: "pref")
                    comboBox(selectedItem: bind(target: model, targetProperty: "selectUnitVariable"),
                        model: eventComboBoxModel(source: model.selectUnitVariableList),
                        constraints: cc(x: 1, y: 1)
                    )
                    textField(text: bind('selectUnitValue', target: model), constraints: cc(x: 3, y: 1))
                }

                label(app.getMessage('application.form.indicators.field.highlights.label', 'Highlights'), constraints: cc(x: 1, y: 9))
                panel(constraints: cc(x: 3, y: 9)) {
                    formLayout(columns: "right:pref, 4dlu, fill:50dlu:grow, min", rows: "pref")
                    comboBox(selectedItem: bind(target: model, targetProperty: "highlightsVariable"),
                        model: eventComboBoxModel(source: model.selectUnitVariableList),
                        constraints: cc(x: 1, y: 1)
                    )
                    textField(text: bind('highlightsValue', target: model), constraints: cc(x: 3, y: 1))
                }

                def referenceTypes = [
                    (app.getMessage('application.form.indicators.field.reference.none.label', 'None')): 'none',
                    (app.getMessage('application.form.indicators.field.reference.internal.label', 'Internal')): '_internal_',
                    (app.getMessage('application.form.indicators.field.reference.external.label', 'External')): '_external_'
                ]
                label(app.getMessage('application.form.indicators.field.reference.label', 'Reference type'), constraints: cc(x: 1, y: 11))
                comboBox(selectedItem: bind(target: model, targetProperty: "referenceType", mutual: true,
                        converter: {v -> referenceTypes[v]},
                        reverseConverter: {v -> referenceTypes.find { it.value == v }?.key}
                    ),
                    items: referenceTypes.collect(['']) {k, v -> k},
                    actionPerformed: {
                        // Fix panel update!!!
                        view.indicatorsPanel.revalidate()
                        view.indicatorsPanel.repaint()
                    },
                    constraints: cc(x: 3, y: 11)
                )

                def engineTypes = [
                    (app.getMessage('application.form.indicators.field.engineType.local.label', 'Local')): 'local',
                    (app.getMessage('application.form.indicators.field.engineType.central.label', 'Central')): 'central'
                ]
                label(app.getMessage('application.form.indicators.field.engineType.label', 'Engine type'), constraints: cc(x: 1, y: 13))
                comboBox(selectedItem: bind(target: model, targetProperty: "engineType", mutual: true,
                        converter: {v -> engineTypes[v]},
                        reverseConverter: {v -> engineTypes.find { it.value == v }?.key}
                    ),
                    items: engineTypes.collect(['']) {k, v -> k},
                    constraints: cc(x: 3, y: 13)
                )
            }

            panel(id: 'indicatorsPanel') {
                boxLayout(axis: BoxLayout.Y_AXIS)
                panel(border: titledBorder(title: app.getMessage('application.form.indicators.referenceFiles.title', 'Reference files')), visible: bind(source: model, sourceProperty:'canShowExternalFiles')) {
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
                panel(border: titledBorder(title: app.getMessage('application.form.indicators.inputFiles.title', 'Input files')), visible: bind(source: model, sourceProperty:'canShowInputFiles')) {
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
                scrollPane(border: titledBorder(title: app.getMessage('application.form.indicators.indicators.title', 'Indicators'))) {
                    table(id: "indicatorsTable", model: createIndicatorsTableModel())
                }
            }

            hbox(constraints: SOUTH) {
                button(refreshIndicatorsAction)
                button(runIndicatorsAction)
            }
        }
    }

    busyComponent(name: app.getMessage('application.tab.transfer.name', 'Transfer'),
            id: "t1",
            constraints: BorderLayout.CENTER,
            busy: bind { model.transferring }) {
        busyModel(description: app.getMessage('application.tab.transfer.busy.description', 'Working...'))
        panel() {
            borderLayout()
            panel(border: titledBorder(title: app.getMessage('application.form.transfer.title', 'Transfer options')), constraints: NORTH) {
                formLayout(columns: "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min", rows: "pref, 2dlu, pref, 2dlu, pref, 2dlu, pref, 2dlu, pref,4dlu")

                label(app.getMessage('application.form.transfer.field.workDir.label', 'Working directory'), constraints: cc(x: 1, y: 1))
                textField(text: bind('workDir', target: model, mutual: true), editable: false, constraints: cc(x: 3, y: 1))
                button(action: selectWorkDirAction, constraints: cc(x: 5, y: 1))

                label(app.getMessage('application.form.transfer.field.server.label', 'Server'), constraints: cc(x: 1, y: 3))
                textField(text: bind('server', target: model, mutual: true), constraints: cc(x: 3, y: 3))
                button(action: saveAction, constraints: cc(x: 5, y: 3))

                label(app.getMessage('application.form.transfer.field.username.label', 'Username'), constraints: cc(x: 1, y: 5))
                textField(text: bind('username', target: model, mutual: true), constraints: cc(x: 3, y: 5))

                label(app.getMessage('application.form.transfer.field.password.label', 'Password'), constraints: cc(x: 1, y: 7))
                passwordField(text: bind('password', target: model, mutual: true), constraints: cc(x: 3, y: 7))

                label(app.getMessage('application.form.transfer.field.activeFtp.label', 'Active FTP mode'), constraints: cc(x: 1, y: 9))
                checkBox(selected: bind('activeFtp', target: model, mutual: true), constraints: cc(x: 3, y: 9))
            }

            panel() {
                boxLayout(axis: BoxLayout.Y_AXIS)
                scrollPane(border: titledBorder(title: app.getMessage('application.form.transfer.files.title', 'Files'))) {
                    table(id: "filesTable", model: createFilesTableModel())
                }
            }

            hbox(constraints: SOUTH) {
                button(refreshFilesAction)
                button(transferFilesAction)
            }
        }
    }

    panel(name: app.getMessage('application.tab.log.name', 'Log'), constraints: BorderLayout.CENTER) {
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

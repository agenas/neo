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

import com.jgoodies.forms.layout.CellConstraints
import com.jgoodies.forms.layout.FormLayout
import griffon.transform.Threading
import neobox.db.MappingsHolder
import neobox.transfer.WrongCredentialsException
import neobox.utils.NeoboxUtils
import neobox.stat.*
import javax.swing.*
import javax.swing.text.AttributeSet
import javax.swing.text.SimpleAttributeSet
import javax.swing.text.StyleConstants
import javax.swing.text.StyleContext
import javax.swing.text.StyledDocument
import java.awt.*
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import neobox.utils.PatternFilter
import javax.swing.text.AbstractDocument

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

class NeoboxController {
    def databaseService
    def statEngineService
    def reportService
    def transferService

    def model
    def view

    private textBoxes = [:]

    void mvcGroupInit(Map args) {
        loadPreferences()

        // Setup operatorName/code input filter
        AbstractDocument doc
        doc = (AbstractDocument) view.operatorName.getDocument()
        doc.setDocumentFilter(new PatternFilter("\\d*"))

        // Setup year input filter
        doc = (AbstractDocument) view.year.getDocument()
        doc.setDocumentFilter(new PatternFilter("\\d{0,4}"))
    }

    void mvcGroupDestroy() {
    }

    def onStartupEnd = { app ->
        refreshTablesStats()

        // Init indicator panel
        execInsideUIAsync {
            model.selectUnitVariableList.clear()
            model.selectUnitVariableList.add('')
        }
    }

    // =================================================================================
    // Files and directories selection
    // =================================================================================

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def selectIndicatorsDir = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.INDICATORS_DIR,
                NeoboxUtils.INDICATORS_DIR_ID,
                JFileChooser.DIRECTORIES_ONLY,
                app.getMessage('application.form.indicators.field.indicatorsDir.fileChooser.title', 'Select indicators directory'))

        if (file) {
            model.indicatorsDir = file.absoluteFile
            execOutsideUI {
                refreshIndicators()
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def selectWorkDir = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.WORK_DIR,
                NeoboxUtils.WORK_DIR_ID,
                JFileChooser.DIRECTORIES_ONLY,
                app.getMessage('application.form.indicators.field.workDir.fileChooser.title', 'Select working directory'))

        if (file) {
            model.workDir = file.absoluteFile
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def selectCsvFile = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.CSV_FILE_CHOOSER_DIR,
                NeoboxUtils.CSV_FILE_CHOOSER_DIR_ID,
                JFileChooser.FILES_ONLY,
                app.getMessage('application.form.import.field.csvFile.fileChooser.title', 'Select CSV data source'),
                NeoboxUtils.CSV_FILES_FILTER)

        if (file) {
            model.csvFile = file.absolutePath
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def selectSpecsFile = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.SPECS_FILE_CHOOSER_DIR,
                NeoboxUtils.SPECS_FILE_CHOOSER_DIR_ID,
                JFileChooser.FILES_ONLY,
                app.getMessage('application.form.import.field.specsFile.fileChooser.title', 'Select import specification file'),
                NeoboxUtils.SPECS_FILES_FILTER)

        if (file) {
            model.specsFile = file.absolutePath

            execInsideUISync {
                model.importing = true
            }

            execOutsideUI {
                try {
                    // Load the chosen spec file and populate MappingsHolder singleton
                    databaseService.loadSpecs(file)
                    def mappingsHolder = MappingsHolder.instance

                    def variables = mappingsHolder.getVariables()
                    execInsideUISync {
                        model.lookupsFiles.clear()
                        model.variablesList.clear()
                        model.variablesList.addAll(variables)
                    }

                    def mappings = mappingsHolder.getLookupsSpecs()
                    createLookupsPanel(mappings)
                }
                catch (Exception e) {
                    log.error e.message, e
                }
                finally {
                    execInsideUISync {
                        model.importing = false
                    }
                }
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    private createLookupsPanel(lookups) {
        def totalFiles = lookups.size()

        def formPanel = view.moreFiles

        formPanel.removeAll()
        textBoxes = [:]

        if (totalFiles == 0) {
            formPanel.setVisible(false)
        }
        else {
            def rows = []
            for (int i = 0; i < totalFiles; i++) {
                rows << "pref, 2dlu"
            }

            FormLayout layout = new FormLayout(
                  "right:pref, 4dlu, fill:150dlu:grow, 4dlu, pref, min",
                  rows.join(',')
            )
            CellConstraints cc = new CellConstraints()
            formPanel.setLayout(layout)

            for (int i = 0; i < totalFiles; i++) {
                def id = lookups[i].id

                def pos = (i * 2) + 1
                def label = lookups[i].label
                formPanel.add(new JLabel(label), cc.xy(1, pos))

                JTextField text = new JTextField()
                text.setEditable(false)
                formPanel.add(text, cc.xy(3, pos))
                textBoxes[id] = text

                JButton button = new JButton(app.getMessage('application.button.select.name', 'Select'))
                button.setName(id)
                button.addActionListener(selectLookupsFile as ActionListener)

                formPanel.add(button, cc.xy(5, pos))
            }
            formPanel.setVisible(true)
        }
        formPanel.revalidate()
        formPanel.repaint()
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def selectLookupsFile = { ActionEvent evt = null ->
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.LOOKUPS_FILE_CHOOSER_DIR,
                NeoboxUtils.LOOKUPS_FILE_CHOOSER_DIR_ID,
                JFileChooser.FILES_ONLY,
                app.getMessage('application.form.import.field.lookupsFile.fileChooser.title', 'Select CSV data source'),
                NeoboxUtils.CSV_FILES_FILTER)
        def id = evt.source.name
        if (file) {
            textBoxes[id].setText(file.absolutePath)
            model.lookupsFiles[id] = file.absolutePath
        }
    }

    // =================================================================================
    // Import
    // =================================================================================

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def importTheMatrix = {
        def file = new File(model.csvFile)
        def variablesFilled = model.areMandatoryVariablesFilled()
        def lookupsFilled = model.areLookupsTablesFilled()
        if (file && file.exists() && variablesFilled && lookupsFilled) {
            execInsideUISync {
                model.importing = true
            }

            execOutsideUI {
                try {
                    // Define context variables
                    def context = model.getVariables()

                    // Do import
                    databaseService.importAll(file, context, model.lookupsFiles)

                    refreshTablesStats()
                }
                catch (Exception e) {
                    log.error e.message, e

                    JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.import.error', 'Error while importing file!'),             app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
                }
                finally {
                    execInsideUISync {
                        model.importing = false
                    }
                }
            }
        }
        else {
            if (!lookupsFilled) {
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.fillLookups', 'Please define lookups tables'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            else {
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.fillVariables', 'Please fill all mandatory variables'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def importLookups = {
        def lookupsFilled = model.areLookupsTablesFilled()
        if (lookupsFilled) {
            execInsideUISync {
                model.importing = true
            }

            execOutsideUI {
                try {
                    // Do import
                    databaseService.importAll(null, [:], model.lookupsFiles)

                    refreshTablesStats()
                }
                catch (Exception e) {
                    log.error e.message, e

                    JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.import.error', 'Error while importing file!'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
                }
                finally {
                    execInsideUISync {
                        model.importing = false
                    }
                }
            }
        }
        else {
            JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.fillLookups', 'Please define lookups tables'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
        }
    }

    // Import events
    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onImportStarted = { max ->
        model.maxProgressBar = max
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onImportProgressBarUpdate = { recNo, counter ->
        model.status = app.getMessage('application.message.import.status.importing', [recNo])
        model.current = counter
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onImportFinished = { maxRecords ->
        model.current = model.maxProgressBar
        model.status = app.getMessage('application.message.import.status.done', [maxRecords])
    }

    // =================================================================================
    // Config page
    // =================================================================================

    def refreshTablesStats = {
        execInsideUISync {
            model.running = true
            model.tablesList.clear()
        }

        execOutsideUI {
            try {
                model.dbStats = databaseService.collectStats()
                execInsideUIAsync {
                    model.dbStats.each { k, v ->
                        model.tablesList.add([id: k, count: v])
                    }
                }
            }
            catch (Exception e) {
                log.error e.message, e
            }
            finally {
                execInsideUIAsync {
                    model.running = false
                }
            }
        }
    }

    // =================================================================================
    // Indicators
    // =================================================================================

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def addReferenceFile = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.REFERENCE_FILE_CHOOSER_DIR,
                NeoboxUtils.REFERENCE_FILE_CHOOSER_DIR_ID,
                JFileChooser.FILES_ONLY,
                app.getMessage('application.form.indicators.field.referenceFile.fileChooser.title', 'Select reference file'),
                NeoboxUtils.ZIP_FILES_FILTER)
        if (file) {
            // TODO: Verify if file is valid
            def filename = file.absolutePath
            def item = model.referenceFilesList.find { it.name == filename }
            if (!item) {
                model.referenceFilesList.add([name: filename])
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def delReferenceFile = {
        def selection = model.referenceFilesListSelection.selection
        if (selection) {
            def name = selection[0].name
            def item = model.referenceFilesList.find { it.name == name }
            if (item) {
                model.referenceFilesList.remove(item)
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def addInputFile = {
        def file = NeoboxUtils.selectFileOrDir(
                NeoboxUtils.INPUT_FILE_CHOOSER_DIR,
                NeoboxUtils.INPUT_FILE_CHOOSER_DIR_ID,
                JFileChooser.FILES_ONLY,
                app.getMessage('application.form.indicators.field.inputFile.fileChooser.title', 'Select input file'),
                NeoboxUtils.ZIP_FILES_FILTER)
        if (file) {
            // TODO: Verify if file is valid
            def filename = file.absolutePath
            def item = model.inputFilesList.find { it.name == filename }
            if (!item) {
                model.inputFilesList.add([name: filename])
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def delInputFile = {
        def selection = model.inputFilesListSelection.selection
        if (selection) {
            def name = selection[0].name
            def item = model.inputFilesList.find { it.name == name }
            if (item) {
                model.inputFilesList.remove(item)
            }
        }
    }

    def refreshIndicators = {
        def indicatorsDir = new File(model.indicatorsDir)

        execInsideUISync {
            model.running = true
            model.indicatorsList.clear()
            model.selectUnitVariableList.clear()
        }

        execOutsideUI {
            try {
                statEngineService.loadIndicators(indicatorsDir, model.mainLanguage)
                execInsideUIAsync {
                    model.indicatorsList.addAll(IndicatorsHolder.instance.visibleIndicatorsForUI)
                    model.selectUnitVariableList.add('')
                    model.selectUnitVariableList.addAll(IndicatorsHolder.instance.variablesForUI.name)
                }
            }
            catch (Exception e) {
                log.error e.message, e
            }
            finally {
                execInsideUIAsync {
                    model.current = 0
                    model.status = ""
                    model.running = false
                }
            }
        }
    }

    def runIndicators = {
        def indicatorsDir = new File(model.indicatorsDir)
        def workDir = new File(model.workDir)

        execInsideUISync {
            model.running = true
        }

        execOutsideUI {
            savePreferences()

            try {
                if (checkMandatoryVariablesForIndicators()) {
                    def highlights = parseSelectUnitVariable(model.highlightsVariable, model.highlightsValue)

                    def definedReferenceType = model.referenceType == 'none' ? '' : model.referenceType;
                    def definedEngineType = model.engineType == 'none' ? '' : model.engineType;

                    def runningParams = [
                        language: model.language ?: "it",
                        operator: model.operatorName ?: "000000",
                        year: model.year ?: "0000",
                        engine_type: definedEngineType,
                        reference: definedReferenceType,
                        reference_files: definedReferenceType == '_external_' ? model.referenceFilesList?.collect { it.name } : [],
                        input_files: definedEngineType == 'central' ? model.inputFilesList?.collect { it.name } : [],
                        funnel_group: highlights[0]?.value ?: ''
                    ]

                    def selectedIndicatorsForReport = model.indicatorsList.findAll { it.selected && !it.excludeReport }.obj

                    def selectUnit = parseSelectUnitVariable(model.selectUnitVariable, model.selectUnitValue)

                    def totalRuns = selectUnit.size()

                    boolean doIt = true
                    if (totalRuns > 1) {
                        doIt = JOptionPane.showConfirmDialog(Window.windows.find { it.focused }, app.getMessage('application.message.indicators.multireportWarning', [totalRuns]), app.getMessage('application.dialog.warning.title', 'Warning'), JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION
                    }

                    if (doIt) {
                        File firstWorkDir = null
                        for(int i = 0; i < totalRuns; i++) {
                            // Calcola il nome della sottodirectory di workdir (all se non loop)
                            def realWorkDir = new File(workDir, selectUnit[i].id)

                            // Imposta la select unit
                            if (selectUnit[i].value) {
                                runningParams['select_unit'] = selectUnit[i].value
                                runningParams['reference'] = definedReferenceType
                            }
                            else {
                                runningParams['select_unit'] = ''
                                // Impostare reference == _internal_ senza valorizzare select_unit non ha senso. Forziamo reference a blank!
                                if (definedReferenceType == "_internal_") {
                                    runningParams['reference'] = ''
                                }
                                else {
                                    runningParams['reference'] = definedReferenceType
                                }
                            }

                            if (statEngineService.runIndicators(model.indicatorsList, runningParams, indicatorsDir, realWorkDir, firstWorkDir)) {
                                reportService.runReportGeneration(selectedIndicatorsForReport, runningParams, indicatorsDir, realWorkDir)
                            }

                            if (i == 0) {
                                firstWorkDir = realWorkDir
                            }
                        }
                    }
                }
                else {
                    JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.indicators.variables.error'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
                }
            }
            catch (CircularDependencyException cde) {
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, cde.getMessage(), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            catch (DataPreparationException dpe) {
                log.error dpe.message, dpe
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, dpe.getMessage(), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            catch (Exception e) {
                log.error e.message, e
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, e.getMessage(), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            finally {
                execInsideUIAsync {
                    model.running = false
                }
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onRunIndicatorsStarted = { maxSteps ->
        model.current = 0
        model.maxProgressBar = maxSteps
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onRunIndicatorsProgress = { indicatorId ->
        model.status = app.getMessage('application.message.indicators.status.running', [indicatorId])
        model.current = model.current + 1
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onRunIndicatorsPhase = { indicatorId, phaseId ->
        def phase = app.getMessage("application.message.indicators.status.phase.${phaseId}")
        model.status = app.getMessage('application.message.indicators.status.phase', [indicatorId, phase])
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onRunIndicatorsFinished = {
        model.status = app.getMessage('application.message.indicators.status.allDone')
        model.current = model.maxProgressBar
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onRunIndicatorError = { indicatorId ->
        model.status = app.getMessage('application.message.indicators.status.error', [indicatorId])
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onReportGenerationStarted = { maxSteps ->
        model.status = app.getMessage('application.message.report.status.running')
        model.current = 0
        model.maxProgressBar = maxSteps
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onReportGenerationProgress = {
        model.current = model.current + 1
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onReportGenerationFinished = {
        model.status = app.getMessage('application.message.report.status.done')
        model.current = model.maxProgressBar
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onReportGenerationError = {
        model.status = app.getMessage('application.message.report.status.error')
    }

    // =================================================================================
    // Transfer
    // =================================================================================

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def refreshFiles = {
        def workDir = new File(model.workDir)

        execInsideUISync {
            model.transferring = true
            model.filesList.clear()
        }

        execOutsideUI {
            try {
                def files = transferService.collectFiles(workDir)
                execInsideUIAsync {
                    model.filesList.addAll(files)
                }
            }
            catch (Exception e) {
                log.error e.message, e
            }
            finally {
                execInsideUIAsync {
                    model.current = 0
                    model.status = ""
                    model.transferring = false
                }
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def transferFiles = {
        execInsideUISync {
            model.transferring = true
        }

        execOutsideUI {
            try {
                def selectedFiles = model.filesList.findAll { it.selected }
                if (selectedFiles.size() > 0) {
                    def serverParams = [
                            server: model.server,
                            username: model.username,
                            password: model.password,
                            activeFtp: model.activeFtp
                    ]

                    transferService.transferFiles(serverParams, selectedFiles)
                }
                else {
                    JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.transfer.files.error'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
                }
            }
            catch (UnknownHostException uhe) {
                log.error uhe.message, uhe
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.transfer.host.error', [model.server]), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            catch (WrongCredentialsException wce) {
                log.error wce.message, wce
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, app.getMessage('application.message.transfer.account.error'), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            catch (Exception e) {
                log.error e.message, e
                JOptionPane.showMessageDialog(Window.windows.find { it.focused }, e.getMessage(), app.getMessage('application.dialog.error.title', 'Error'), JOptionPane.ERROR_MESSAGE)
            }
            finally {
                execInsideUIAsync {
                    model.transferring = false
                }
            }
        }
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onTransferFilesStarted = { maxSteps ->
        model.current = 0
        model.maxProgressBar = maxSteps
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onTransferFilesProgress = { filename ->
        model.status = app.getMessage('application.message.transfer.status.running', [filename])
        model.current = model.current + 1
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onTransferFilesFinished = {
        model.status = app.getMessage('application.message.transfer.status.done')
        model.current = model.maxProgressBar
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onTransferFilesError = { filename ->
        model.status = app.getMessage('application.message.transfer.status.error', [filename])
    }

    // =================================================================================
    // Logging
    // =================================================================================

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def clearLog = {
        JTextPane tp = view.logArea
        tp.setText("")
    }

    @Threading(Threading.Policy.INSIDE_UITHREAD_ASYNC)
    def onWriteLog = { txt, type ->
        def color
        if (type == "error") {
            color = Color.RED
        }
        else if (type == "info") {
            color = Color.BLUE
        }
        else if (type == "complete") {
            color = Color.GREEN
        }
        else {
            color = Color.BLACK
        }

        writeLogMessage(txt + "\n", color)
    }

    private void writeLogMessage(String msg, Color c)
    {
        JTextPane tp = view.logArea

        StyleContext sc = StyleContext.getDefaultStyleContext();
        AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, c)

        StyledDocument doc = tp.getStyledDocument()
        int l = doc.getLength()
        doc.insertString(l, msg, aset)

        // Scroll
        tp.setCaretPosition(tp.getDocument().getLength())
    }

    // =================================================================================
    // OSX specific
    // =================================================================================

    def onOSXAbout = { app ->
        about()
    }

    def onOSXPrefs = { app ->
        preferences()
    }

    def onOSXQuit = { app ->
        app.shutdown()
    }

    // =================================================================================
    // General
    // =================================================================================

    def about = {
        withMVCGroup('about') { m, v, c ->
            c.show()
        }
    }

    def preferences = {
        withMVCGroup('preferences') { m, v, c ->
            c.show()
        }
    }

    def quit = {
        app.shutdown()
    }

    // =================================================================================
    // Config preferences
    // =================================================================================

    def savePreferencesAction = {
        execOutsideUI {
                savePreferences()
        }
    }

    private void savePreferences() {
        // Save config preferences before running indicators
        NeoboxUtils.PREFERENCES.put("operatorName", model.operatorName)
        NeoboxUtils.PREFERENCES.put("year", model.year)
        NeoboxUtils.PREFERENCES.put("mainLanguage", model.mainLanguage)
        NeoboxUtils.PREFERENCES.put("engineType", model.engineType)
        NeoboxUtils.PREFERENCES.put("language", model.language)
        NeoboxUtils.PREFERENCES.put("referenceType", model.referenceType)
        NeoboxUtils.PREFERENCES.put("server", model.server)
        NeoboxUtils.PREFERENCES.put("username", model.username)
    }

    private void loadPreferences() {
        // Load preferences
        model.operatorName = NeoboxUtils.PREFERENCES.get("operatorName", "")
        model.year = NeoboxUtils.PREFERENCES.get("year", "")
        model.mainLanguage = NeoboxUtils.PREFERENCES.get("mainLanguage", "it")
        model.engineType = NeoboxUtils.PREFERENCES.get("engineType", "")
        model.language = NeoboxUtils.PREFERENCES.get("language", "it")
        model.referenceType = NeoboxUtils.PREFERENCES.get("referenceType", "none")
        model.server = NeoboxUtils.PREFERENCES.get("server", "")
        model.username = NeoboxUtils.PREFERENCES.get("username", "")
    }

    // =================================================================================
    // Utility
    // =================================================================================

    private parseSelectUnitVariable(String var, String val, String op = "||") {
        def result = []

        if (var && val.indexOf('|') > 0) {
            def inputs = val.split(/\|/).findAll { it.trim() != ''}
            inputs.each { value ->
                result.addAll(parseSelectUnitVariableImpl(var, value, op))
            }
        }
        else {
            result = parseSelectUnitVariableImpl(var, val, op)
        }

        return result
    }

    private parseSelectUnitVariableImpl(String var, String value, String op = "||") {
        def result = []

        if (var && value) {
            def parts = value.split(/ /).findAll { it.trim() != ''}
            if (parts.size() == 1 && parts[0] == "*") {
                def values = databaseService.collectUniqueValuesFor(var)
                values.each {
                    result << [id: "${it ?: '_NA_'}", var: var , value: "${var}=='${it ?: ''}'"]
                }
            }
            else {
                result << [id: parts.collect { "${it ?: '_NA_'}" }.join("_"), var: var, value: parts.collect { "${var}=='${it}'" }.join(op)]
            }
        }
        else if (!var && value) {
            result << [id: 'SINGLE', var: '' , value: value.replaceAll("\"", "'")]
        }
        else {
            result << [id: 'SINGLE', var: '' , value: '']
        }

        return result
    }

    private boolean checkMandatoryVariablesForIndicators() {
        return model.operatorName && model.year.trim()
    }
}

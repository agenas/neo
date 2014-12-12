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

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

actions {
    // Files selection
    action(id: 'selectWorkDirAction', name: app.getMessage('application.button.select.name', 'Select'), closure: controller.selectWorkDir)
    action(id: 'selectIndicatorsDirAction', name: app.getMessage('application.button.select.name', 'Select'), closure: controller.selectIndicatorsDir)
    action(id: 'selectCsvFileAction', name: app.getMessage('application.button.select.name', 'Select'), closure: controller.selectCsvFile)
    action(id: 'selectSpecsFileAction', name: app.getMessage('application.button.select.name', 'Select'), closure: controller.selectSpecsFile)

    // Config
    action(id: 'saveAction', name: app.getMessage('application.button.save.name', 'Save'), closure: controller.savePreferencesAction)

    // Import
    action(id: 'importAction', name: app.getMessage('application.button.import.name', 'Import'), closure: controller.importTheMatrix, enabled: bind {model.canImport})
    action(id: 'importLookupsAction', name: app.getMessage('application.button.importLookups.name', 'Import lookups'), closure: controller.importLookups, enabled: bind {model.canImportLookups})

    // Indicators
    action(id: 'addReferenceFileAction', name: app.getMessage('application.button.plus.name', '+'), closure: controller.addReferenceFile)
    action(id: 'delReferenceFileAction', name: app.getMessage('application.button.minus.name', '-'), closure: controller.delReferenceFile)
    action(id: 'addInputFileAction', name: app.getMessage('application.button.plus.name', '+'), closure: controller.addInputFile)
    action(id: 'delInputFileAction', name: app.getMessage('application.button.minus.name', '-'), closure: controller.delInputFile)
    action(id: 'refreshIndicatorsAction', name: app.getMessage('application.button.refresh.name', 'Refresh'), closure: controller.refreshIndicators, enabled: bind {model.canRefresh})
    action(id: 'runIndicatorsAction', name: app.getMessage('application.button.run.name', 'Run'), closure: controller.runIndicators, enabled: bind {model.canRun})

    // Transmission
    action(id: 'refreshFilesAction', name: app.getMessage('application.button.refresh.name', 'Refresh'), closure: controller.refreshFiles, enabled: bind {model.canRefreshFiles})
    action(id: 'transferFilesAction', name: app.getMessage('application.button.transfer.name', 'Transfer'), closure: controller.transferFiles, enabled: bind {model.canTransferFiles})

    // Log
    action(id: 'clearLogAction', name: app.getMessage('application.button.clear.name', 'Clear'), closure: controller.clearLog, enabled: bind {model.canClearLog})

    // General
    action(id: 'aboutAction', name: app.getMessage('application.menu.about.name', 'About'), closure: controller.about)
    action(id: 'preferencesAction', name: app.getMessage('application.menu.preferences.name', 'Preferences'), closure: controller.preferences)
    action(id: 'quitAction', name: app.getMessage('application.menu.quit.name', 'Quit'), mnemonic: app.getMessage('application.menu.quit.mnemonic', 'Q'), accelerator: shortcut(app.getMessage('application.menu.quit.mnemonic', 'Q')), closure: controller.quit)
}

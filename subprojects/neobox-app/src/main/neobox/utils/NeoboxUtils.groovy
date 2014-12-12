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

package neobox.utils

import griffon.util.ApplicationHolder
import griffon.core.GriffonApplication
import javax.swing.*
import java.util.prefs.Preferences

import static griffon.util.GriffonNameUtils.isBlank
import static griffon.util.GriffonApplicationUtils.isWindows

final class NeoboxUtils {
    static final Preferences PREFERENCES = Preferences.userNodeForPackage(NeoboxUtils)

    static String CSV_FILE_CHOOSER_DIR_ID = 'CSV_FILE_CHOOSER_DIR'
    static String SPECS_FILE_CHOOSER_DIR_ID = 'SPECS_FILE_CHOOSER_DIR'
    static String LOOKUPS_FILE_CHOOSER_DIR_ID = 'LOOKUPS_FILE_CHOOSER_DIR'
    static String REFERENCE_FILE_CHOOSER_DIR_ID = 'REFERENCE_FILE_CHOOSER_DIR'
    static String INPUT_FILE_CHOOSER_DIR_ID = 'INPUT_FILE_CHOOSER_DIR'
    static String WORK_DIR_ID = 'WORK_DIR'
    static String INDICATORS_DIR_ID = 'INDICATORS_DIR'

    static File CSV_FILE_CHOOSER_DIR = new File(PREFERENCES.get(CSV_FILE_CHOOSER_DIR_ID, '.'))
    static File SPECS_FILE_CHOOSER_DIR = new File(PREFERENCES.get(SPECS_FILE_CHOOSER_DIR_ID, '.'))
    static File LOOKUPS_FILE_CHOOSER_DIR = new File(PREFERENCES.get(LOOKUPS_FILE_CHOOSER_DIR_ID, '.'))
    static File REFERENCE_FILE_CHOOSER_DIR = new File(PREFERENCES.get(REFERENCE_FILE_CHOOSER_DIR_ID, '.'))
    static File INPUT_FILE_CHOOSER_DIR = new File(PREFERENCES.get(INPUT_FILE_CHOOSER_DIR_ID, '.'))
    static File WORK_DIR = new File(PREFERENCES.get(WORK_DIR_ID, '.'))
    static File INDICATORS_DIR = new File(PREFERENCES.get(INDICATORS_DIR_ID, '.'))

    static ExtensionFileFilter ALL_FILES_FILTER = new ExtensionFileFilter("All files", "")
    static ExtensionFileFilter CSV_FILES_FILTER = new ExtensionFileFilter("CSV files", "csv")
    static ExtensionFileFilter SPECS_FILES_FILTER = new ExtensionFileFilter("SPECS files", "specs")
    static ExtensionFileFilter ZIP_FILES_FILTER = new ExtensionFileFilter("ZIP files", "zip")

    static String INDICATOR_SPECS_FILE = 'indicator.specs'
    static String INDICATOR_SOURCE_FILE = 'indicator.r'
    static String INDICATORS_SELECTUNIT_FILE = 'selectUnit.specs'

    static String R_UNIX_SCRIPT = 'Rscript'
    static String R_WINDOWS_SCRIPT = 'Rscript.exe'

    static File selectFileOrDir(File location, String locationPrefs, int selectionMode = JFileChooser.FILES_ONLY, String name = null, fileFilter = null) {
        GriffonApplication app = ApplicationHolder.application
        if (isBlank(name)) {
            name = 'Open'
        }
        JFileChooser fc = new JFileChooser(location)
        fc.fileSelectionMode = selectionMode
        fc.acceptAllFileFilterUsed = false
        fc.dialogTitle = name

        if (fileFilter) {
            fc.setFileFilter(fileFilter)
        }

        if (fc.showOpenDialog(app.windowManager.startingWindow) == JFileChooser.APPROVE_OPTION) {
            PREFERENCES.put(locationPrefs, fc.currentDirectory.path)
            return fc.selectedFile
        }
        return null
    }

    static Map splitFilename(String fileName) {
        def idx = fileName.lastIndexOf(".")
        def name = fileName
        def ext = ""
        if (idx > 0) {
            name = fileName[0..idx - 1]
            if (fileName.length() > idx + 1) {
                ext = fileName[idx + 1..-1]
            }
        }
        return [name: name, ext: ext]
    }

    static String convertToPlatformSpecificPath(String path) {
        if (isWindows) {
            return path.replaceAll("\\\\", "\\\\\\\\");
        }
        return path
    }
}

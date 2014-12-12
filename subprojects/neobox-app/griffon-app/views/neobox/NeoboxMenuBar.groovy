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

import static griffon.util.GriffonApplicationUtils.isMacOSX

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

menuBar {
    if (!isMacOSX) {
        menu(text: app.getMessage('application.menu.file.name', ' File'),
                mnemonic: app.getMessage('application.menu.file.mnemonic', 'F')) {
            menuItem(preferencesAction)
            separator()
            menuItem quitAction
        }
    }

    if (!isMacOSX) {
        menu(text: app.getMessage('application.menu.help.name', ' ?')) {
            menuItem aboutAction
        }
    }
}

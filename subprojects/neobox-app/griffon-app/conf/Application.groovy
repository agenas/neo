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

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

application {
    title = 'NEObox'
    startupGroups = ['neobox']

    // Should Griffon exit when no Griffon created frames are showing?
    autoShutdown = true

    // If you want some non-standard application class, apply it here
    //frameClass = 'javax.swing.JFrame'
}

mvcGroups {
    // MVC Group for "neobox"
    'neobox' {
        model      = 'neobox.NeoboxModel'
        view       = 'neobox.NeoboxView'
        controller = 'neobox.NeoboxController'
    }

    // MVC Group for "credits"
    'about' {
        model      = 'neobox.AboutModel'
        view       = 'neobox.AboutView'
        controller = 'neobox.DialogController'
    }

    // MVC Group for "preferences"
    'preferences' {
        model      = 'neobox.PreferencesModel'
        view       = 'neobox.PreferencesView'
        controller = 'neobox.DialogController'
    }

}

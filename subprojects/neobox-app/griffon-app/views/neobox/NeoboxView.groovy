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

build(NeoboxActions)

fileChooserWindow = fileChooser()

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

    build(NeoboxMenuBar)

    migLayout(layoutConstraints: 'fill')

    widget(build(NeoboxContent), constraints: 'center, grow')
    widget(build(NeoboxStatusBar), constraints: 'south, grow')
}

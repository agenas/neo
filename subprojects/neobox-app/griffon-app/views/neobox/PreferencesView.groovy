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
panel(id: 'content') {
    migLayout layoutConstraints: 'fill'
    panel(constraints: 'grow, wrap') {
        borderLayout(hgap: 10, vgap: 10)
        label("""
            <html><p>${app.getMessage('application.message.noPreferences', 'No preferences available.')}</p>
            <br/>
            </html>
        """.stripIndent(12).trim(), constraints: CENTER)
    }

    // button(hideAction, constraints: 'right')

    keyStrokeAction(component: current,
            keyStroke: 'ESCAPE',
            condition: 'in focused window',
            action: hideAction)
}

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

import java.awt.Font

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

rowConstraints = "center, span 1, wrap".toString()

panel(id: 'content') {
    migLayout layoutConstraints: 'fill'
    label(icon: imageIcon('/neobox-icon-128x128.png'), constraints: "center, span 1, wrap".toString())
    label(GriffonNameUtils.capitalize(app.getMessage('application.title', app.config.application.title)) +
            ' ' + Metadata.current.getApplicationVersion(),
            font: current.font.deriveFont(Font.BOLD),
            constraints: rowConstraints)
    label(text: bind { model.description }, constraints: rowConstraints)

    scrollPane(preferredSize: [420, 80], constraints: rowConstraints) {
        table {
            tableFormat = defaultTableFormat(columnNames: ['Name', 'Task', 'Language'])
            eventTableModel(source: model.authors, format: tableFormat)
            installTableComparatorChooser(source: model.authors)
        }
    }

    keyStrokeAction(component: current,
            keyStroke: 'ESCAPE',
            condition: 'in focused window',
            action: hideAction)
}
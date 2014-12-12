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

import javax.swing.SwingConstants
import java.awt.GridBagConstraints

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

vbox {
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
                constraints: gbc(insets: [1, 3, 1, 3])
    }
}

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

package neobox.stat;

public class DataPreparationException extends RuntimeException {
	public DataPreparationException(Object message) {
		this(message, null);
	}
	public DataPreparationException(Object message, Throwable cause) {
		super(message == null ? null : message.toString(), cause);
	}

	public DataPreparationException(Throwable cause) {
		this(null, cause);
	}
}
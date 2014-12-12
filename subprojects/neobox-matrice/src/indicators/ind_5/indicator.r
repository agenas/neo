####################################################################
# Copyright 2014 Fabrizio Carinci, AGENAS.
#
# Licensed under the European Union Public Licence (EUPL), Version 1.1 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#       http://joinup.ec.europa.eu/software/page/eupl/licence-eupl
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
####################################################################

####################################################################
# Author: Fabrizio Carinci, AGENAS, carinci@agenas.it
# February 2014
####################################################################

# =====================================================
# Entry point
#
# baseDir = Root directory for all indicators
# workDir = Work directory for this indicator
#
# =====================================================

# Load common functions
source(paste(baseDir, "/commons/tools.r", sep=""))
source(paste(baseDir, "/commons/options.r", sep=""))

liste_opzioni()

# Load implementation
source(paste(baseDir,"/ind_5/implementation.r", sep=""))

# entry point
indicator5(target=funnel_unit,target_name=funnel_unit_name)

rm(list=ls(all=TRUE))

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

main<- function(verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore viewDBStore: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator viewDBStore: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ################ NEO Main Output dataset

 if (engine_type=="local") {
  input_data<-
   merge_table(c("db_master","db_demographics",
                 "db_sources_hyperte","db_sources_diab","db_sources_ihd","db_sources_hf","db_sources_demen","db_sources_allpat",
                 "db_aveind_hyperte","db_aveind_diab","db_aveind_ihd","db_aveind_hf","db_aveind_demen",
                 "db_pctind_hyperte","db_pctind_diab","db_pctind_ihd","db_pctind_hf","db_pctind_demen",
                 "db_globind_num","db_globind_den",
                 "db_cost","db_discharge"))

  write.csv(input_data,paste(workDir,"/input.csv",sep=""),row.names=FALSE,na="")

  rm(input_data)

 } else {
  if (language=="italian") {
   cat(paste("viewDBStore definito solo con engine type = local\r",sep=""))
   cat(paste("Nessun dataset prodotto\r",sep=""))
  } else if (language=="english") {
   cat(paste("viewDBStore defined only with engine type = local\r",sep=""))
   cat(paste("No dataset produced\r",sep=""))
  }
 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore viewDBStore",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator viewDBStore",sep=""))
  }
 }

}

# entry point
main()

rm(list=ls(all=TRUE))

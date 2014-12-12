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
   cat("Indicatore viewOnTheFly: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator viewOnTheFly: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ################ NEO Main Output dataset

 if (engine_type=="local") {
  input_data<-read.table("../viewDBstore/input.csv",header=TRUE,sep=",",colClasses="character")
  input_data<-make_numeric(input_data,c(list_patologie,list_fonti_vars,list_costi_ext,list_costi_ext_2,list_numvars,list_highscore,list_ricoveri,"SUM_STRANIERI","COUNT"))
 } else if (engine_type=="central") {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 engine_type<-""  # do not load dataset in each indicator (taken from above)
 assign("input_data",input_data,envir=.GlobalEnv)
 assign("engine_type",engine_type,envir=.GlobalEnv)

 source(paste(baseDir,"/ind_1/implementation.r", sep=""))
 source(paste(baseDir,"/ind_2/implementation.r", sep=""))
 source(paste(baseDir,"/ind_3/implementation.r", sep=""))
 source(paste(baseDir,"/ind_4/implementation.r", sep=""))
 source(paste(baseDir,"/ind_5/implementation.r", sep=""))
 source(paste(baseDir,"/ind_6/implementation.r", sep=""))

 fileConn<-file("report.xml")
 writeLines("",fileConn)
 close(fileConn)

 fileConn<-file("report.xml",open="at")
 writeLines('<title>Report on the Fly</title>',fileConn)
 writeLines('<sect1 id="Indicator1">',fileConn)
 close(fileConn)

 indicator1(append=1)

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 writeLines('<?custom-pagebreak?>',fileConn)
 writeLines('<sect1 id="Indicator2">',fileConn)
 close(fileConn)

 indicator2(append=1)

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 writeLines('<?custom-pagebreak?>',fileConn)
 writeLines('<sect1 id="Indicator3">',fileConn)
 close(fileConn)

 indicator3(append=1)

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 writeLines('<?custom-pagebreak?>',fileConn)
 writeLines('<sect1 id="Indicator4">',fileConn)
 close(fileConn)

 indicator4(append=1)

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 writeLines('<?custom-pagebreak?>',fileConn)
 writeLines('<sect1 id="Indicator5">',fileConn)
 close(fileConn)

 indicator5(xml=0,target=funnel_unit,target_name=funnel_unit_name,append=1)

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 writeLines('<?custom-pagebreak?>',fileConn)
 writeLines('<sect1 id="Glossario">',fileConn)
 close(fileConn)

 indicator6()

 fileConn<-file("report.xml",open="at")
 writeLines('</sect1>',fileConn)
 close(fileConn)

 rm(input_data)

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore viewOnTheFly",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator viewOnTheFly",sep=""))
  }
  cat("\r")
 }

}

# entry point
main()

rm(list=ls(all=TRUE))

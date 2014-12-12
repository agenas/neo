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

indicator6 <- function(verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 6: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 6: started processing\r")
  }
  cat("########################################\r")
 }

 # xml=0 indicator in a loop xml=1 indicator directly to report

 # Set the working directory
 setwd(workDir)

 ########################
 # Patologie

 fileConn<-file("report.xml",open="at")
 if (language=="italian") {
  writeLines("<title>Glossario</title>",fileConn)
 } else if (language=="english") {
  writeLines("<title>Glossary</title>",fileConn)
 }
 # Write report section in xml
 writeLines(paste('<para>',sep=""),fileConn)
 if (language=="italian") {
  writeLines(paste("<emphasis role='red'>Identificazione delle Patologie</emphasis>",sep=""),fileConn)
 } else if (language=="english") {
  writeLines(paste("<emphasis role='red'>Identification of Diseases</emphasis>",sep=""),fileConn)
 }
 writeLines(paste('</para>',sep=""),fileConn)
 close(fileConn)

 table<- data.frame(matrix(ncol=2,nrow=length(list_patologie)))

 for (i in 1:length(list_patologie)) {

  table[i,"patologie"]<-list_patologie_names[i]
  table[i,"desc"]<-list_patologie_algo[i]

 }

 if (language=="italian") {
  title<-"Definizioni Operative"
 } else if (language=="english") {
  title<-"Operational Definitions"
 }

 if (language=="italian") {
  labs=c("Patologia","Descrizione")
 } else if (language=="english") {
  labs=c("Disease","Description")
 }

 writeTable(file="report.xml",
            data=table,
            vars=c("patologie","desc"),
            headlabs=labs,
            footlabs=labs,
            headwidt=c("190pt","490pt"),
            colalign=c("left","left"),
            title=title,
            graph=NULL)

 rm(table)

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Aggiunto glossario definizioni operative patologie\r",sep=""))
  } else if (language=="english") {
   cat(paste("Added glossary of operational definitions of diseases\r",sep=""))
  }
 }

 ########################
 # Indicatori

 for (i in 1:length(list_patologie)) {

  if (i==1) {
   fileConn<-file("report.xml",open="at")
   # Write report section in xml
   writeLines(paste('<para>',sep=""),fileConn)
   if (language=="italian") {
    writeLines(paste("<emphasis role='red'>Indicatori di Qualita' dell'assistenza sanitaria</emphasis>",sep=""),fileConn)
   } else if (language=="english") {
    writeLines(paste("<emphasis role='red'>Quality of Care Indicators</emphasis>",sep=""),fileConn)
   }
   writeLines(paste('</para>',sep=""),fileConn)
   close(fileConn)
  }

  table<- data.frame(matrix(ncol=2,nrow=length(indicatori[[i]])))

  for (j in 1:length(indicatori[[i]])) {

   table[j,"indicator"]<-indicatori_desc_short[[i]][j]
   table[j,"desc"]<-indicatori_desc[[i]][j]

  }

  if (language=="italian") {
   labs=c("Indicatore","Descrizione")
  } else if (language=="english") {
   labs=c("Indicator","Description")
  }

  writeTable(file="report.xml",
             data=table,
             vars=c("indicator","desc"),
             headlabs=labs,
             footlabs=labs,
             headwidt=c("190pt","490pt"),
             colalign=c("left","left"),
             title=list_patologie_names[i],
             graph=NULL)

  rm(table)

 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Aggiunto glossario indicatori\r",sep=""))
  } else if (language=="english") {
   cat(paste("Added glossary of indicators\r",sep=""))
  }
 }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 6",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 6",sep=""))
  }
  cat("\r")
 }

}

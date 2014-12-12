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

indicator1 <- function(xml=1,graphs=1,output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 1: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 1: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ##########################################
 # Input Dataset
 ##########################################

 if (engine_type=="local")  {
  input_data<-merge_table(c("db_master","db_demographics"))
  input_data<-make_numeric(input_data,c("SUM_STRANIERI","COUNT"))
 } else if (engine_type=="central") {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 ##########################################
 # Reference Dataset
 ##########################################

 if (reference!="") {

  arrow_up  <-"<graphic fileref='resources/arrow_up.png'></graphic>"
  arrow_down<-"<graphic fileref='resources/arrow_down.png'></graphic>"

  if (reference=="_internal_") {  ### Reference population is whole input dataset
   ref_data<-input_data
  } else if (reference=="_external_") {   ### Reference population is a collection of output datasets created by neobox
   ref_data<-createCentralData(input_files=reference_files,list_numvars=list_numvars)
  }

 }

 ##########################################
 # Select Unit
 ##########################################

 if (select_unit!="") {
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("N=",sum(input_data[,"COUNT"])," osservazioni originariamente presenti in dati in Input\r",sep=""))
   } else if (language=="english") {
    cat(paste("N=",sum(input_data[,"COUNT"])," observations originally present in Input data\r",sep=""))
   }
  }
  input_data<-do.call(subset,list(x=input_data,subset=parse(text=select_unit)))
 }

 source(paste(baseDir,"/commons/stresstests.r",sep=""),local=TRUE)

 totale<-0
 if (nrow(input_data)>0) {
  totale<-sum(input_data[,"COUNT"])
 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("N=",totale," osservazioni caricate da dati in Input\r",sep=""))
  } else if (language=="english") {
   cat(paste("N=",totale," observations loaded from Input data\r",sep=""))
  }
  if (select_unit!="") {
   if (language=="italian") {
    cat(paste("** per effetto della selezione: ",select_unit,"\r",sep=""))
   } else if (language=="english") {
    cat(paste("** due to selection: ",select_unit,"\r",sep=""))
   }
  }
 }

 if (reference!="") {
  ref_totale<-0
  if (nrow(ref_data)>0) {
   ref_totale<-sum(ref_data[,"COUNT"])
  }
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("N=",ref_totale," osservazioni caricate da dati di Riferimento\r",sep=""))
   } else if (language=="english") {
    cat(paste("N=",ref_totale," observations loaded from Reference data\r",sep=""))
   }
  }
 }

 ##########################################
 # Tabella 1.1
 ##########################################

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Elaborazione dati Tabella 1.1 avviata..",sep=""))
  } else if (language=="english") {
   cat(paste("Processing Table 1.1 started..",sep=""))
  }
 }

 if (reference!="") {

  table1_1<- data.frame(matrix(ncol=16,nrow=4))

  names(table1_1)<-c("desc","n","perc","ref_n","ref_perc","delta","hist","deltarrow",
                  "align_1","align_2","align_3","align_4","align_5","align_6","align_7","align_8")
  table1_1[,"align_1"]<-"right"
  table1_1[,"align_2"]<-"right"
  table1_1[,"align_3"]<-"right"
  table1_1[,"align_4"]<-"right"
  table1_1[,"align_5"]<-"right"
  table1_1[,"align_6"]<-"right"
  table1_1[,"align_7"]<-"center"
  table1_1[,"align_8"]<-"center"
 } else {
  table1_1<- data.frame(matrix(ncol=6,nrow=4))
  names(table1_1)<-c("desc","n","perc","align_1","align_2","align_3")
  table1_1[,"align_1"]<-"right"
  table1_1[,"align_2"]<-"right"
  table1_1[,"align_3"]<-"right"
 }

 #######################
 # Initialize
 table1_1[,"n"]     <-"0"
 table1_1[,"perc"]  <-"-"

 if (reference!="") {
  table1_1[,"ref_n"]<-"0"
  table1_1[,"ref_perc"]<-"-"
  table1_1[,"delta"]<-"-"
  table1_1[,"hist"] <-""
  table1_1[,"sig"]<-0
 }

 #######################

 if (language=="italian") {
  table1_1[1,"desc"]<-"Totale Assistiti"
 } else {
  table1_1[1,"desc"]<-"Total Population"
 }

 table1_1[1,"align_1"]<-"left"

 if (totale>0) {
  table1_1[1,"n"]   <-format(sum(input_data[,"COUNT"]),big.mark=bigmark)
 }
 table1_1[1,"perc"]<-paste("100",decimalmark,"00",sep="")

 if (reference!="") {
  if (ref_totale>0) {
   table1_1[1,"ref_n"]   <-format(sum(ref_data[,"COUNT"]),big.mark=bigmark)
  }
  table1_1[1,"ref_perc"]<-paste("100",decimalmark,"00",sep="")
  table1_1[1,"delta"]<-""
 }

 #######################

 if (language=="italian") {
  table1_1[2,"desc"]<-"di cui donne"
 } else {
  table1_1[2,"desc"]<-"women"
 }

 if (totale>0) {
  table1_1[2,"n"]   <-format(sum(input_data[input_data$SESSO=="F","COUNT"]),big.mark=bigmark)
  table1_1[2,"perc"]<-format(round((sum(input_data[input_data$SESSO=="F","COUNT"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
 }

 if (reference!="") {

  if (ref_totale>0) {
   table1_1[2,"ref_n"]   <-format(sum(ref_data[ref_data$SESSO=="F","COUNT"]),big.mark=bigmark)
   table1_1[2,"ref_perc"]<-format(round((sum(ref_data[ref_data$SESSO=="F","COUNT"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
   if (totale>0) {
    table1_1[2,"delta"]   <-format(round((sum(input_data[input_data$SESSO=="F","COUNT"])/totale)*100,2)-round((sum(ref_data[ref_data$SESSO=="F","COUNT"])/sum(ref_data[,"COUNT"]))*100,2),decimal.mark=decimalmark,nsmall=2)
    minidot(file=paste("d_1_1_t2",sep=""),mean=c(as.numeric(gsub(decimalmark,".",table1_1[2,"perc"],fixed=TRUE)),as.numeric(gsub(decimalmark,".",table1_1[2,"ref_perc"],fixed=TRUE))),n=c(as.numeric(gsub(bigmark,"",table1_1[2,"n"],fixed=TRUE)),as.numeric(gsub(bigmark,"",table1_1[2,"ref_n"],fixed=TRUE))))
    table1_1[2,"sig"]     <-sig
    table1_1[2,"hist"]    <-"<graphic fileref='d_1_1_t2.pdf'></graphic>"
   }
  }

 }

 #######################

 if (language=="italian") {
  table1_1[3,"desc"]<-"di cui di eta'>=75"
 } else {
  table1_1[3,"desc"]<-"aged 75 and over"
 }

 if (totale>0) {
  table1_1[3,"n"]   <-format(sum(input_data[input_data$AGE_RANGE=="5","COUNT"]),big.mark=bigmark)
  table1_1[3,"perc"]<-format(round((sum(input_data[input_data$AGE_RANGE=="5","COUNT"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
 }

 if (reference!="") {

  if (ref_totale>0) {
   table1_1[3,"ref_n"]   <-format(sum(ref_data[ref_data$AGE_RANGE=="5","COUNT"]),big.mark=bigmark)
   table1_1[3,"ref_perc"]<-format(round((sum(ref_data[ref_data$AGE_RANGE=="5","COUNT"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
   if (totale>0) {
    table1_1[3,"delta"]   <-format(round((sum(input_data[input_data$AGE_RANGE=="5","COUNT"])/totale)*100,2)-round((sum(ref_data[ref_data$AGE_RANGE=="5","COUNT"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
    minidot(file=paste("d_1_1_t3",sep=""),mean=c(as.numeric(gsub(decimalmark,".",table1_1[3,"perc"],fixed=TRUE)),as.numeric(gsub(decimalmark,".",table1_1[3,"ref_perc"],fixed=TRUE))),n=c(as.numeric(gsub(bigmark,"",table1_1[3,"n"],fixed=TRUE)),as.numeric(gsub(bigmark,"",table1_1[3,"ref_n"],fixed=TRUE))))
    table1_1[3,"sig"]<-sig
    table1_1[3,"hist"]<-"<graphic fileref='d_1_1_t3.pdf'></graphic>"
   }
  }

 }

 #######################

 if (language=="italian") {
  table1_1[4,"desc"]<-"di cui stranieri"
 } else {
  table1_1[4,"desc"]<-"foreign nationality"
 }

 if (totale>0) {
  table1_1[4,"n"]   <-format(sum(input_data[,"SUM_STRANIERI"]),big.mark=bigmark)
  table1_1[4,"perc"]<-format(round((sum(input_data[,"SUM_STRANIERI"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
 }

 if (reference!="") {

  if (ref_totale>0) {
   table1_1[4,"ref_n"]   <-format(sum(ref_data[,"SUM_STRANIERI"]),big.mark=bigmark)
   table1_1[4,"ref_perc"]<-format(round((sum(ref_data[,"SUM_STRANIERI"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
   if (totale>0) {
    table1_1[4,"delta"]   <-format(round((sum(input_data[,"SUM_STRANIERI"])/totale)*100,2)-round((sum(ref_data[,"SUM_STRANIERI"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
    minidot(file=paste("d_1_1_t4",sep=""),mean=c(as.numeric(gsub(decimalmark,".",table1_1[4,"perc"],fixed=TRUE)),as.numeric(gsub(decimalmark,".",table1_1[4,"ref_perc"],fixed=TRUE))),n=c(as.numeric(gsub(bigmark,"",table1_1[4,"n"],fixed=TRUE)),as.numeric(gsub(bigmark,"",table1_1[4,"ref_n"],fixed=TRUE))))
    table1_1[4,"sig"]<-sig
    table1_1[4,"hist"]<-"<graphic fileref='d_1_1_t4.pdf'></graphic>"
   }
  }

 }

 #########################

 if (reference!="") {
  table1_1[,"deltarrow"]<-""
  table1_1[,"deltarrow"]<-ifelse(table1_1$sig==2,arrow_up,table1_1[,"deltarrow"])
  table1_1[,"deltarrow"]<-ifelse(table1_1$sig==1,arrow_down,table1_1[,"deltarrow"])
 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("completata\r",sep=""))
  } else if (language=="english") {
   cat(paste("completed\r",sep=""))
  }
 }

 #################################### Write xml ######################################

 if (xml==1) {

  if (language=="italian") {
   title<-"Tabella 1.1 - Inquadramento demografico nel periodo osservato"
   section<-"Sezione 1. Analisi di contesto"
  } else if (language=="english") {
   title<-"Table 1.1 - Demographic characteristics"
   section<-"Section 1. Reference Population"
  }

  if (reference=="") {
   writeTable(file="report.xml",
     data=table1_1,
     append=append,
     vars=c("desc","n","perc"),
     headlabs=c("","N","%"),
     headwidt=c("260pt","60pt","60pt"),
     colalign=c("left","right","right"),
     headalign=c("left","center","center"),
     varcolalign=c("align_1","align_2","align_3"),
     footlabs=c("","N","%"),
     title=title,
     section=section,
     graph=NULL)
  } else {

   if (language=="italian") {
    headlabs<-c("","N","%","N [P.R.]","% [P.R.]","Delta","[0-100%]","")
    footlabs<-c("","N","%","N<?custom-linebreak?>[P.R.]","%<?custom-linebreak?>[P.R.]","Delta","[0-100%]","")
    footnote<-"[P.R.]=Popolazione Riferimento"
  } else if (language=="english") {
    headlabs<-c("","N","%","N [R.P.]","% [R.P.]","Delta","[0-100%]","")
    footlabs<-c("","N","%","N<?custom-linebreak?>[R.P.]","%<?custom-linebreak?>[R.P.]","Delta","[0-100%]","")
    footnote<-"[R.P.]=Reference Population"
   }

   writeTable(file="report.xml",
     data=table1_1,
     append=append,
     vars=c("desc","n","perc","ref_n","ref_perc","delta","hist","deltarrow"),
     headlabs=headlabs,
     headwidt=c("260pt","60pt","60pt","60pt","60pt","60pt","80pt","15pt"),
     colalign=c("left","right","right","right","right","center","center","center"),
     headalign=c("left","center","center","center","center","center","center","center"),
     varcolalign=c("align_1","align_2","align_3","align_4","align_5","align_6","align_7","align_8"),
     footlabs=footlabs,
     footnote=footnote,
     title=title,
     section=section,
     graph=NULL)
  }

 }

 ####################################################

 table1_1$ASL<-operator
 table1_1$YEAR<-year

 table1_1<-table1_1[,c("ASL","YEAR","desc","n","perc")]
 table1_1$Anno           <-as.integer(table1_1$YEAR)
 table1_1$CodiceASL      <-table1_1$ASL
 table1_1$NumeroAssistiti<-suppressWarnings(as.integer(gsub(bigmark,"",table1_1$n,fixed=TRUE)))
 table1_1$CodiceRiga     <-as.numeric(rownames(table1_1))

 table1_1$Percentuale<-suppressWarnings(ifelse(table1_1$perc!="-",as.numeric(gsub(decimalmark,".",table1_1$perc,fixed=TRUE)),""))

 table1_1<-table1_1[,c("Anno","CodiceASL","CodiceRiga","NumeroAssistiti","Percentuale")]

 ####################################################

 coding1<- data.frame(matrix(ncol=4,nrow=4))
 names(coding1)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 if (language=="italian") {
  coding1[1,"Descrizione"]<-"Totale Assistiti"
  coding1[2,"Descrizione"]<-"di cui donne"
  coding1[3,"Descrizione"]<-"di cui di eta'>=75"
  coding1[4,"Descrizione"]<-"di cui stranieri"
 } else {
  coding1[1,"Descrizione"]<-"Total Population"
  coding1[2,"Descrizione"]<-"Women"
  coding1[3,"Descrizione"]<-"Aged 75 and over"
  coding1[4,"Descrizione"]<-"Foreign nationality"
 }
 coding1$SezioneTabella<-1
 coding1$CodiceTabella<-1
 coding1$CodiceRiga<-as.numeric(rownames(coding1))

 #########################################################################################

 if (output!="") {

  data_output<-list(table1_1)
  files_output<-c("table1_1")

  if (verbose==1) {
   if (language=="italian") {
    cat(paste("Tabella risultati salvata in file: [",workDir,"/",files_output,".csv]\r",sep=""))
   } else if (language=="english") {
    cat(paste("Table of results saved in file: [",workDir,"/",files_output,".csv]\r",sep=""))
   }
  }

  for (i in 1:length(data_output)) {
   write.csv(data_output[[i]],paste(output,"/",files_output[i],".csv",sep=""),row.names=FALSE,na="")
  }

  write.csv(coding1,paste(output,"/CodificheRighe.csv",sep=""),row.names=FALSE,na="")

 }

 ########################################################################################

 if (xml==1) {
  if (engine_type!="") {rm(input_data)}
  rm(table1_1,coding1)
 } else {

  # Make table available globally
  assign("table1_1",table1_1,envir=.GlobalEnv)
  assign("coding1",coding1,envir=.GlobalEnv)

 }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 1",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 1",sep=""))
  }
  cat("\r")
 }

}

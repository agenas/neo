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
   cat("Indicatore output: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator output: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ################

 ASL<-""
 YEAR<-""

 ################ NEO Main Output dataset

 if (engine_type=="local")  {

  ################ Output dataset from local

  input_data<-merge_table(c("db_master","db_demographics","db_sources_hyperte","db_sources_diab","db_sources_ihd","db_sources_hf","db_sources_demen","db_sources_allpat","db_aveind_hyperte","db_aveind_diab","db_aveind_ihd","db_aveind_hf","db_aveind_demen","db_pctind_hyperte","db_pctind_diab","db_pctind_ihd","db_pctind_hf","db_pctind_demen","db_globind_num","db_globind_den","db_cost","db_discharge"))
  input_data<-make_numeric(input_data,c(list_patologie,"COUNT"))

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

  totale<-0
  tot_pat<-0

  if (nrow(input_data)>0) {
   totale<-sum(input_data[,"COUNT"])
   tot_pat<-sum(input_data[rowSums(input_data[list_patologie])>0,"COUNT"])
  }

  if (verbose>0) {
   cat("\r")
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
   if (language=="italian") {
    cat(paste("di cui N=",tot_pat," osservazioni relative a casi patologici\r",sep=""))
   } else if (language=="english") {
    cat(paste("of which N=",tot_pat," observations including subjects with diseases\r",sep=""))
   }
  }

  ##########################################

  names_drop<-c()

  drop_list<-c(pop_levels,local_system_levels,adjusters,list_patologie)

  for (i in 1:length(drop_list)) {
   names_drop<-c(names_drop,which(names(input_data)==drop_list[i]))
  }

  for (i in 1:length(input_data[-names_drop])) {
   input_data[-names_drop][i]<-as.numeric(unlist(input_data[-names_drop][i]))
  }

  output_data<-aggregate(x=input_data[-names_drop],by=input_data[,c(pop_levels,adjusters,list_patologie)],FUN="sum")

  ASL<-operator
  YEAR<-year
  output_data<-cbind(ASL,YEAR,output_data)

 } else if (engine_type=="central") {

  output_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)

  ##########################################
  # Select Unit
  ##########################################

  if (select_unit!="") {
   if (verbose>0) {
    if (language=="italian") {
     cat(paste("N=",sum(output_data[,"COUNT"])," osservazioni originariamente presenti in dati in Input\r",sep=""))
    } else if (language=="english") {
     cat(paste("N=",sum(output_data[,"COUNT"])," observsations originally present in Input data\r",sep=""))
    }
   }
   output_data<-do.call(subset,list(x=output_data,subset=parse(text=select_unit)))
  }

  ##########################################

  input_data<-output_data

  totale<-0
  tot_pat<-0

  if (nrow(input_data)>0) {
   totale<-sum(input_data[,"COUNT"])
   tot_pat<-sum(input_data[rowSums(input_data[list_patologie])>0,"COUNT"])
  }

  if (verbose>0) {
   cat("\r")
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
   if (language=="italian") {
    cat(paste("di cui N=",tot_pat," osservazioni relative a casi patologici\r",sep=""))
   } else if (language=="english") {
    cat(paste("of which N=",tot_pat," observations including subjects with diseases\r",sep=""))
   }
  }

  if (length(levels(as.factor(output_data$YEAR)))>1) {
   if (language=="italian") {
    print("Errore: Anni diversi nei files in Input")
   } else if (language=="english") {
    print("Error: different Years in Input data")
   }
   stop()
  } else {
   if (totale>0) {
    for (i in 1:length(levels(as.factor(output_data$ASL)))) {
     if (i==1) {
      ASL<-paste(ASL,levels(as.factor(output_data$ASL))[i],sep="")
     } else {
      ASL<-paste(ASL,levels(as.factor(output_data$ASL))[i],sep=",")
     }
    }
    YEAR<-levels(as.factor(output_data$YEAR))
   }
  }

 } # central

 ########################
 ################ Write Output dataset
 write.csv(output_data,"neobox_output.csv",row.names=FALSE,na="")
 fileConn<-file("descriptor-centrale.yml")
 writeLines("# NEOBOX STATISTICAL ENGINE OUTPUT DESCRIPTOR",fileConn)
 close(fileConn)
 fileConn<-file("descriptor-centrale.yml",open="at")
 writeLines(paste('operator: "',ASL,'"',sep=""),fileConn)
 writeLines(paste('year: "',YEAR,'"',sep=""),fileConn)
 writeLines(paste('select_unit: "',select_unit,'"',sep=""),fileConn)
 writeLines('type: "centrale"',fileConn)
 close(fileConn)

 writeZipDescriptorFor(paste(output_file,".zip",sep=""),c("descriptor-centrale.yml|descriptor.yml","neobox_output.csv"))

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Salvato Tracciato Record Centrale in file: [",workDir,"/tracciato_record_centrale.zip]\r",sep=""))
  } else if (language=="english") {
   cat(paste("Saved Central Record Format in file: [",workDir,"/tracciato_record_centrale.zip]\r",sep=""))
  }
 }

 ##########################################
 ################ ORACLE Output dataset

 ori_select_unit<-select_unit
 ori_engine_type<-engine_type

 if (engine_type=="local")  {
  levels_dist<-sort(unique(input_data[,c(target)]))
  n_levels<-length(levels_dist)
 } else if (engine_type=="central")  {
  levels_dist<-unique(input_data[,c("ASL",target)])
  n_levels<-nrow(levels_dist)
 }

 files_oracle<-c()

 reference<-""
 engine_type<-""  # do not load dataset in each indicator (take input_data from above)
 funnel_group<-""
 year<-YEAR

 assign("input_data",input_data,envir=.GlobalEnv)
 assign("reference",reference,envir=.GlobalEnv)
 assign("engine_type",engine_type,envir=.GlobalEnv)
 assign("funnel_group",funnel_group,envir=.GlobalEnv)
 assign("year",year,envir=.GlobalEnv)

 ############################
 ############### Indicators
 ############################

 source(paste(baseDir,"/ind_1/implementation.r", sep=""))
 source(paste(baseDir,"/ind_2/implementation.r", sep=""))
 source(paste(baseDir,"/ind_3/implementation.r", sep=""))
 source(paste(baseDir,"/ind_4/implementation.r", sep=""))
 source(paste(baseDir,"/ind_5/implementation.r", sep=""))

 for (i in 1:(n_levels+1)) {

  # set artificial global options

  if (i==n_levels+1) {
   select_unit<-""
   operator<-ASL
  } else {
   if (ori_engine_type=="local")  {
    select_unit<-paste(target,"=='",levels_dist[i],"'",sep="")
    operator<-ASL
   } else if (ori_engine_type=="central")  {
    select_unit<-paste("ASL=='",levels_dist[i,c("ASL")],"' & ",target,"=='",levels_dist[i,target],"'",sep="")
    operator<-levels_dist[i,c("ASL")]
   }
  }

  assign("operator",operator,envir=.GlobalEnv)

  ######## Check Data

  if (select_unit!="") {
   chk_data<-do.call(subset,list(x=input_data,subset=parse(text=select_unit)))
  } else {
   chk_data<-input_data
  }

  totale<-0
  tot_pat<-0

  if (nrow(chk_data)>0) {
   totale<-sum(chk_data[,"COUNT"])
   tot_pat<-sum(chk_data[rowSums(chk_data[list_patologie])>0,"COUNT"])
  }

  if (verbose>0) {
   if (i<n_levels+1) {
    cat("\r")
    if (language=="italian") {
     cat(paste("Estrazione dati sottogruppo :",select_unit,"\r",sep=""))
    } else if (language=="english") {
     cat(paste("Estracting data for subgroup :",select_unit,"\r",sep=""))
    }
   } else {
    cat("\r")
    if (language=="italian") {
     cat(paste("Estrazione intero set di dati\r",sep=""))
    } else if (language=="english") {
     cat(paste("Estracting entire dataset\r",sep=""))
    }
   }
   if (language=="italian") {
    cat(paste("N=",totale," osservazioni selezionate\r",sep=""))
    cat(paste("di cui N=",tot_pat," osservazioni relative a casi patologici\r",sep=""))
   } else if (language=="english") {
    cat(paste("N=",totale," osservazioni selezionate\r",sep=""))
    cat(paste("of which N=",tot_pat," observations including subjects with diseases\r",sep=""))
   }
  }

  ########################################################

  assign("select_unit",select_unit,envir=.GlobalEnv)

  if (language=="italian") {
   cat("Elaborazione Indicatore 1 avviata....",sep="")
  } else if (language=="english") {
   cat("Processing Indicator 1 started....",sep="")
  }
  indicator1(xml=0,output="",verbose=0)
  if (language=="italian") {
   cat("completata\r",sep="")
  } else if (language=="english") {
   cat("completed\r",sep="")
  }

  if (language=="italian") {
   cat("Elaborazione Indicatore 2 avviata....",sep="")
  } else if (language=="english") {
   cat("Processing Indicator 2 started....",sep="")
  }
  indicator2(xml=0,graph=0,output="",verbose=0)
  if (language=="italian") {
   cat("completata\r",sep="")
  } else if (language=="english") {
   cat("completed\r",sep="")
  }

  if (language=="italian") {
   cat("Elaborazione Indicatore 3 avviata....",sep="")
  } else if (language=="english") {
   cat("Processing Indicator 3 started....",sep="")
  }
  indicator3(xml=0,output="",verbose=0)
  if (language=="italian") {
   cat("completata\r",sep="")
  } else if (language=="english") {
   cat("completed\r",sep="")
  }

  if (language=="italian") {
   cat("Elaborazione Indicatore 4 avviata....",sep="")
  } else if (language=="english") {
   cat("Processing Indicator 4 started....",sep="")
  }
  indicator4(xml=0,graphs=0,output="",verbose=0)
  if (language=="italian") {
   cat("completata\r",sep="")
  } else if (language=="english") {
   cat("completed\r",sep="")
  }

  if (i==n_levels+1) { # only general across targets (DIST_MMG), funnel plot for one point meaningless
   if (language=="italian") {
    cat("Elaborazione Indicatore 5 avviata....",sep="")
   } else if (language=="english") {
    cat("Processing Indicator 5 started....",sep="")
   }
   if (ori_engine_type=="central") {
    input_data[,target]<-ifelse(input_data[,target]!="",paste(input_data$ASL,input_data[,target],sep=""),input_data[,target])
   }
   assign("input_data",input_data,envir=.GlobalEnv)
   indicator5(xml=0,graphs=0,target=target,target_name=target_name,output="",verbose=0)
   if (language=="italian") {
    cat("completata\r",sep="")
   } else if (language=="english") {
    cat("completed\r",sep="")
   }
  }

  ##############################

  data_pileup<-c("output_table1_1",
                 "output_table2_1","output_table2_2","output_table2_3",
                 "output_table3_1","output_table3_2","output_table3_3","output_table3_4",
                 "output_table4_1","output_table4_1_2","output_table4_2"
                 )

  data_single<-list(table1_1,
                    table2_1,table2_2,table2_3,
                    table3_1,table3_2,table3_3,table3_4,
                    table4_1,table4_1_2,table4_2
                    )

  for (g in 1:length(data_single)) {

   if (i==n_levels+1) {
    data_single[[g]][,c("CodiceDistretto")]<-"*"
   } else {
    if (ori_engine_type=="local") {
     data_single[[g]][,c("CodiceDistretto")]<-levels_dist[i]
    } else if (ori_engine_type=="central") {
     data_single[[g]][,c("CodiceDistretto")]<-levels_dist[i,target]
    }
   }

   if (i>1) {
    getModified(data_pileup[g],rbind(get(data_pileup[g]),data_single[[g]]))
   } else {
    getModified(data_pileup[g],data_single[[g]])
   }

  }

  rm(data_single,data_pileup)

  if (i==1) {
   coding_righe<-rbind(coding1,coding2,coding3)
   coding_indicatori<-coding_indicatori3
   coding_patologie<-coding_patologie3
  }

  cat("\r",sep="")

 } # loop over districts

 ############## Indicator 5

 names(table5_1)<-c("Anno","CodiceASL","CodiceDistretto","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")
 names(table5_2)<-c("Anno","CodiceASL","CodiceDistretto","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")
 names(table5_3)<-c("Anno","CodiceASL","CodiceDistretto","CodicePatologia","CodiceIndicatore","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")
 names(table5_4)<-c("Anno","CodiceASL","CodiceDistretto","CodicePatologia","NumeroAssistiti","CostoTotaleProcapite","StandardError","FlagOutlier")
 names(table5_5)<-c("Anno","CodiceASL","CodiceDistretto","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")

 #################################

 data_oracle<-list(output_table1_1,
                   output_table2_1,output_table2_2,output_table2_3,
                   output_table3_1,output_table3_2,output_table3_3,output_table3_4,
                   output_table4_1,output_table4_1_2,output_table4_2,
                   table5_1,table5_2,table5_3,table5_4,table5_5,
                   coding_righe,coding_indicatori,coding_patologie)

 files_oracle<-c("Table1_1",
                 "Table2_1","Table2_2","Table2_3",
                 "Table3_1","Table3_2","Table3_3","Table3_4",
                 "Table4_1","Table4_1_2","Table4_2",
                 "Table5_1","Table5_2","Table5_3","Table5_4","Table5_5",
                 "CodificheRighe","CodificheIndicatori","CodifichePatologie")

 zipfiles<-c()

 for (i in 1:length(data_oracle)) {
  write.csv(data_oracle[[i]],paste(files_oracle[i],".csv",sep=""),row.names=FALSE,na="")
  zipfiles<-c(zipfiles,paste(files_oracle[i],".csv",sep=""))
 }

 fileConn<-file("descriptor-oracle.yml")
 writeLines("# NEOBOX STATISTICAL ENGINE OUTPUT DESCRIPTOR",fileConn)
 close(fileConn)
 fileConn<-file("descriptor-oracle.yml",open="at")
 writeLines(paste('operator: "',ASL,'"',sep=""),fileConn)
 writeLines(paste('year: "',YEAR,'"',sep=""),fileConn)
 writeLines(paste('select_unit: "',ori_select_unit,'"',sep=""),fileConn)
 writeLines('type: "oracle"',fileConn)
 close(fileConn)

 writeZipDescriptorFor("tracciato_record_oracle.zip",c("descriptor-oracle.yml|descriptor.yml",zipfiles))

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Salvato Tracciato Record Oracle in file: [",workDir,"/tracciato_record_oracle.zip]\r",sep=""))
  } else if (language=="english") {
   cat(paste("Saved Oracle Record Format in file: [",workDir,"/tracciato_record_oracle.zip]\r",sep=""))
  }
 }

 rm(input_data)

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore output",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator output",sep=""))
  }
  cat("\r")
 }

}

# entry point
main()

rm(list=ls(all=TRUE))

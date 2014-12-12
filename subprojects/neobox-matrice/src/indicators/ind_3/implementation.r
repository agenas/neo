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

indicator3 <- function(xml=1,graphs=1,output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 3: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 3: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory

 setwd(workDir)

 ##########################################

 if (engine_type=="local")  {
  input_data<-merge_table(c("db_master","db_aveind_hyperte","db_aveind_diab","db_aveind_ihd","db_aveind_hf","db_aveind_demen","db_pctind_hyperte","db_pctind_diab","db_pctind_ihd","db_pctind_hf","db_pctind_demen","db_cost","db_discharge"))
  input_data<-make_numeric(input_data,c(list_patologie,list_costi_ext,list_costi_ext_2,list_numvars,list_ricoveri,"COUNT"))
 } else if (engine_type=="central") {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 ##########################################
 # Reference Dataset
 ##########################################

 if (reference!="") {

  arrow_up           <-"<graphic fileref='resources/arrow_up.png'></graphic>"
  arrow_down         <-"<graphic fileref='resources/arrow_down.png'></graphic>"
  arrow_up_reversed  <-"<graphic fileref='resources/arrow_up_reversed.png'></graphic>"
  arrow_down_reversed<-"<graphic fileref='resources/arrow_down_reversed.png'></graphic>"

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
    cat(paste("N=",sum(input_data[,"COUNT"])," observations originally present in Input dataset\r",sep=""))
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
   cat(paste("of which N=",tot_pat," observations including subjects with disease\r",sep=""))
  }
 }

 ref_totale<-0
 ref_tot_pat<-0
 if (reference!="") {
  if (nrow(ref_data)>0) {
   ref_totale<-sum(ref_data[,"COUNT"])
   ref_tot_pat<-sum(ref_data[rowSums(ref_data[list_patologie])>0,"COUNT"])
  }
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("N=",ref_totale," osservazioni caricate da dati di Riferimento\r",sep=""))
    cat(paste("di cui N=",ref_tot_pat," osservazioni relative a casi patologici\r",sep=""))
   } else if (language=="english") {
    cat(paste("N=",ref_totale," observations loaded from Reference data\r",sep=""))
    cat(paste("of which N=",ref_tot_pat," observations including subjects with disease\r",sep=""))
   }
  }
 }

 source(paste(baseDir,"/commons/stresstests.r",sep=""),local=TRUE)

 ##########################################

 if (totale>0) {
  table_comb<-aggregate(x=input_data[c("COUNT",list_costi_ext)],by=input_data[list_patologie],FUN="sum")
  names(table_comb)<-c(list_patologie,"COUNT",list_costi_ext)
  tot_pop<-sum(input_data[,"COUNT"])
 } else {
  tot_pop<-0
 }

 if (reference!="") {
  if (ref_totale>0) {
   table_ref_comb<-aggregate(x=ref_data[c("COUNT",list_costi_ext)],by=ref_data[list_patologie],FUN="sum")
   names(table_ref_comb)<-c(list_patologie,"COUNT",list_costi_ext)
   ref_tot_pop<-sum(ref_data[,"COUNT"])
  } else {
   ref_tot_pop<-0
  }
 }

 ##########################################

 for (i in 1:length(list_patologie)) {

  if (verbose>0) {
   cat("\r")
   if (language=="italian") {
    cat(paste("[Elaborazione patologia ",list_patologie_names[i]," avviata]\r",sep=""))
   } else if (language=="english") {
    cat(paste("[Processing disease ",list_patologie_names[i]," started]\r",sep=""))
   }
  }

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Elaborazione dati Tabella 3.1 avviata..",sep=""))
   } else if (language=="english") {
    cat(paste("Processing data Table 3.1 started..",sep=""))
   }
  }

  #################################
  # Popolazione

  #################################
  # Initialize

  if (reference=="") {
   table3_1<- data.frame(matrix(ncol=6,nrow=5))
   names(table3_1)<-c("desc","n","perc","align_1","align_2","align_3")
  } else {
   table3_1<- data.frame(matrix(ncol=14,nrow=5))
   names(table3_1)<-c("desc","n","perc","ref_perc","delta","hist","deltarrow","align_1","align_2","align_3","align_4","align_5","align_6","align_7")
   table3_1[,"align_4"]<-"right"
   table3_1[,"align_5"]<-"right"
   table3_1[,"align_6"]<-"center"
   table3_1[,"align_7"]<-"center"
  }

  table3_1[,"n"]    <-"0"
  table3_1[,"perc"] <-"-"

  if (reference!="") {
   table3_1[,"ref_perc"]<-"-"
   table3_1[,"delta"]   <-"-"
   table3_1[,"hist"]    <-""
   table3_1[,"sig"]     <-0
  }

  #############################################

  ##### Numerosita' combinazioni patologie
  if (totale>0) {
   tot_pop_pat<-sum(input_data[input_data[list_patologie[i]]==1,"COUNT"])
  } else {
   tot_pop_pat<-0
  }

  if (tot_pop_pat>0) {
   table_comb_table<- table_comb[table_comb[list_patologie[i]]==1 & rowSums(table_comb[list_patologie])>1,]
   if (nrow(table_comb_table)>0) {
    table_comb_table<- table_comb_table[order(-table_comb_table$COUNT),]
   }
  }

  if (reference!="") {

   if (ref_totale>0) {
    ref_tot_pop_pat<-sum(ref_data[ref_data[list_patologie[i]]==1,"COUNT"])
   } else {
    ref_tot_pop_pat<-0
   }

   if (ref_tot_pop_pat>0) {
    table_ref_comb_table<- table_ref_comb[table_ref_comb[list_patologie[i]]==1 & rowSums(table_ref_comb[list_patologie])>1,]
    if (nrow(table_ref_comb_table)>0) {
     table_ref_comb_table<- table_ref_comb_table[order(-table_ref_comb_table$COUNT),]
    }
   }

  }

  #######################

  if (language=="italian") {
   table3_1[1,"desc"]<-"Prevalenza nella Popolazione"
  } else {
   table3_1[1,"desc"]<-"Population Prevalence"
  }

  n<-tot_pop_pat

  if (n>0) {
   perc<-round((n/tot_pop)*100,2)
   table3_1[1,"n"]   <-format(n,big.mark=bigmark)
   table3_1[1,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  }
  table3_1[1,"align_1"]<-"left"
  table3_1$align_2<-"right"
  table3_1$align_3<-"right"

  if (reference!="") {
   n_ref<-ref_tot_pop_pat
   if (n_ref>0) {
    ref_perc<-round((n_ref/ref_tot_pop)*100,2)
    table3_1[1,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_1[1,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_1_t1_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_1[1,"sig"]<-sig
     table3_1[1,"hist"]<-paste("<graphic fileref='d_3_1_t1_",i,".pdf'></graphic>",sep="")
    }
   }
  }

  #######################

  if (language=="italian") {
   table3_1[2,"desc"]<-"di cui con patologia isolata"
  } else {
   table3_1[2,"desc"]<-"with only disease"
  }

  if (tot_pop_pat>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1 & rowSums(input_data[list_patologie])==1,"COUNT"])
  } else {
   n<-0
  }

  if (n>0) {
   table3_1[2,"n"]   <-format(n,big.mark=bigmark)
   perc<-round((n/tot_pop_pat)*100,2)
   table3_1[2,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  }
  table3_1[2,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop_pat>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1 & rowSums(ref_data[list_patologie])==1,"COUNT"])
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round((n_ref/ref_tot_pop_pat)*100,2)
    table3_1[2,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_1[2,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_1_t2_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_1[2,"sig"]<-sig
     table3_1[2,"hist"]<-paste("<graphic fileref='d_3_1_t2_",i,".pdf'></graphic>",sep="")
    }
   }
  }

  #######################

  if (language=="italian") {
   table3_1[3,"desc"]<-"di cui con la combinazione di patologie piu' frequente"
  } else {
   table3_1[3,"desc"]<-"with most frequent disease"
  }

  if (tot_pop_pat>0) {
   if (nrow(table_comb_table)>0) {
    n<-table_comb_table[1,"COUNT"]
    if (n>0) {
     perc<-round((n/tot_pop_pat)*100,2)
     table3_1[3,"n"]   <-format(n,big.mark=bigmark)
     table3_1[3,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
    }
   } else {
    n<-0
   }
  }
  table3_1[3,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop_pat>0) {
    if (nrow(table_ref_comb_table)>0) {
     n_ref<-table_ref_comb_table[1,"COUNT"]
     if (n_ref>0) {
      ref_perc<-round((n_ref/ref_tot_pop_pat)*100,2)
      table3_1[3,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
      if (n>0) {
       table3_1[3,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
       minidot(file=paste("d_3_1_t3_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
       table3_1[3,"sig"]<-sig
       table3_1[3,"hist"]<-paste("<graphic fileref='d_3_1_t3_",i,".pdf'></graphic>",sep="")
      }
     }
    }
   }
  }

  #######################

  if (language=="italian") {
   table3_1[4,"desc"]<-"di cui donne"
  } else {
   table3_1[4,"desc"]<-"women"
  }

  if (tot_pop>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1 & input_data$SESSO=="F","COUNT"])
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round((n/tot_pop_pat)*100,2)
   table3_1[4,"n"]   <-format(n,big.mark=bigmark)
   table3_1[4,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  }
  table3_1[4,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1 & ref_data$SESSO=="F","COUNT"])
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round((n_ref/ref_tot_pop_pat)*100,2)
    table3_1[4,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_1[4,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_1_t4_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_1[4,"sig"]<-sig
     table3_1[4,"hist"]<-paste("<graphic fileref='d_3_1_t4_",i,".pdf'></graphic>",sep="")
    }
   }
  }

  #######################

  if (language=="italian") {
   table3_1[5,"desc"]<-"di cui di eta'>=75"
  } else {
   table3_1[5,"desc"]<-"aged 75 and over"
  }

  if (tot_pop>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1 & input_data$AGE_RANGE==5,"COUNT"])
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round((n/tot_pop_pat)*100,2)
   table3_1[5,"n"]   <-format(n,big.mark=bigmark)
   table3_1[5,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  }
  table3_1[5,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1 & ref_data$AGE_RANGE==5,"COUNT"])
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round((n_ref/ref_tot_pop_pat)*100,2)
    table3_1[5,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_1[5,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_1_t5_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_1[5,"sig"]<-sig
     table3_1[5,"hist"]<-paste("<graphic fileref='d_3_1_t5_",i,".pdf'></graphic>",sep="")
    }
   }
  }

  #######################

  if (reference!="") {
   table3_1[,"deltarrow"]<-""
   table3_1[,"deltarrow"]<-ifelse(table3_1$sig==2,arrow_up,table3_1[,"deltarrow"])
   table3_1[,"deltarrow"]<-ifelse(table3_1$sig==1,arrow_down,table3_1[,"deltarrow"])
  }

  #######################

  if (xml==1) {

   if (language=="italian") {
    labs<-"Soggetti con "
    title<-paste("Tabella 3.",i,".1 - Popolazione",sep="")
    section<-"Sezione 3. Focus su ciascuna delle singole patologie"
    subtitle<-paste("Inquadramento della popolazione affetta da ",toupper(list_patologie_names[i]),sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","% [P.R.]","Delta","[0-100%]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","%<?custom-linebreak?>[P.R.]","Delta","[0-100%]","")
     footnote<-"[P.R.]=Popolazione Riferimento"
    }
   } else if (language=="english") {
    labs<-"Subjects with "
    title<-paste("Tabella 3.",i,".1 - Population",sep="")
    section<-"Section 3. Focus on diseases"
    subtitle<-paste("Description of the population affected by ",toupper(list_patologie_names[i]),sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","% [R.P.]","Delta","[0-100%]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","%<?custom-linebreak?>[R.P.]","Delta","[0-100%]","")
     footnote<-"[R.P.]=Reference Population"
    }
   }

   if (i>1) {
    section<-""
    append<-1
   }

   if (reference=="") {

    writeTable(file="report.xml",
     data=table3_1,
     append=append,
     vars=c("desc","n","perc"),
     headlabs=headlabs,
     headwidt=c("500pt","100pt","50pt"),
     colalign=c("left","right","right"),
     varcolalign=c("align_1","align_2","align_3"),
     footlabs=headlabs,
     title=title,
     subtitle=subtitle,
     section=section,
     graph=NULL)

   } else {

    writeTable(file="report.xml",
     data=table3_1,
     append=append,
     vars=c("desc","n","perc","ref_perc","delta","hist","deltarrow"),
     headlabs=headlabs,
     headwidt=c("320pt","90pt","60pt","60pt","50pt","90pt","20pt"),
     colalign=c("left","right","right","right","right","center","right"),
     varcolalign=c("align_1","align_2","align_3","align_4","align_5","align_6","align_7"),
     headalign=c("left","center","center","center","center","center","center"),
     footlabs=footlabs,
     title=title,
     subtitle=subtitle,
     section=section,
     graph=NULL)

   }

  }

  table3_1$disease<-as.integer(i)
  table3_1$row<-rownames(table3_1)

  if (i>1) {output_table3_1<-rbind(output_table3_1,table3_1)} else {output_table3_1<-table3_1}

  rm(table3_1)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("completed\r",sep=""))
   }
  }

  #####################################################
  # Indicatori

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Elaborazione dati Tabella 3.2 avviata..\r",sep=""))
   } else if (language=="english") {
    cat(paste("Processing data Table 3.2 started..\r",sep=""))
   }
  }

  n_indicators<-length(indicatori[[i]])

  ##################################
  # Initialize

  if (reference=="") {
   table3_2<- data.frame(matrix(ncol=8,nrow=n_indicators))
   names(table3_2)<-c("desc","n_num","n_den","num_den","perc","align_1","align_2","align_3")
  } else {
   table3_2<- data.frame(matrix(ncol=16,nrow=n_indicators))
   names(table3_2)<-c("desc","n_num","n_den","num_den","perc","ref_perc","delta","hist","deltarrow","align_1","align_2","align_3","align_4","align_5","align_6","align_7")
  }

  table3_2[,"n_num"]<-"0"
  table3_2[,"n_den"]<-"0"
  table3_2[,"num_den"]<-"0/0"
  table3_2[,"perc"] <-"-"
  table3_2[,"align_1"]<-"left"
  table3_2[,"align_2"]<-"right"
  table3_2[,"align_3"]<-"right"

  if (reference!="") {
   table3_2[,"ref_perc"]<-"-"
   table3_2[,"delta"]   <-"-"
   table3_2[,"hist"]    <-""
   table3_2[,"sig"]     <-0
   table3_2[,"align_4"]<-"right"
   table3_2[,"align_5"]<-"right"
   table3_2[,"align_6"]<-"right"
   table3_2[,"align_7"]<-"center"
  }

  ########################################

  for (k in 1:n_indicators) { # Loop over indicators

   if (verbose>0) {
    if (language=="italian") {
     cat(paste(">>> Avviata Elaborazione Indicatore di Processo: ",indicatori_desc_short[[i]][k],"\r",sep=""))
    } else if (language=="english") {
     cat(paste(">>> Started Processing Process Indicator: ",indicatori_desc_short[[i]][k],"\r",sep=""))
    }
   }

   num_name<-paste("NUM_",indicatori[[i]][k],"_",list_patologie[i],sep="")
   den_name<-paste("DEN_",indicatori[[i]][k],"_",list_patologie[i],sep="")

   table3_2[k,"desc"]<-indicatori_desc_short[[i]][k]

   if (tot_pop>0) {
    n_num<-sum(input_data[input_data[list_patologie[i]]==1,c(num_name)])
    n_den<-sum(input_data[input_data[list_patologie[i]]==1,c(den_name)])
   } else {
    n_den<-0
   }

   if (n_den>0) {
    perc<-round((n_num/n_den)*100,2)
    table3_2[k,"n_num"]  <-n_num
    table3_2[k,"n_den"]  <-n_den
    table3_2[k,"num_den"]<-paste(format(n_num,big.mark=bigmark),"/",format(n_den,big.mark=bigmark),sep="")
    table3_2[k,"perc"]   <-format(perc,decimal.mark=decimalmark,nsmall=2)
   }

   if (reference!="") {

    if (ref_tot_pop>0) {
     n_num_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,c(num_name)])
     n_den_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,c(den_name)])
    } else {
     n_den_ref<-0
    }

    if (n_den_ref>0) {
     ref_perc<-round((n_num_ref/n_den_ref)*100,2)
     table3_2[k,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)

     if (n_den>0) {
      table3_2[k,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
      minidot(file=paste("d_3_2_t_",k,"_",i,sep=""),mean=c(perc,ref_perc),n=c(n_den,n_den_ref))
      table3_2[k,"sig"]<-sig
      table3_2[k,"hist"]<-paste("<graphic fileref='d_3_2_t_",k,"_",i,".pdf'></graphic>",sep="")
     }
    }
   } # end reference
  } # end loop over indicators

  #######################

  if (reference!="") {
   table3_2[,"deltarrow"]<-""
   table3_2[,"deltarrow"]<-ifelse(table3_2$sig==2,arrow_up_reversed,table3_2[,"deltarrow"])
   table3_2[,"deltarrow"]<-ifelse(table3_2$sig==1,arrow_down_reversed,table3_2[,"deltarrow"])
  }

  #####################

  if (xml==1) {

   if (language=="italian") {
    title<-paste("Tabella 3.",i,".2 - Indicatori di Processo",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%","% [P.R.]","Delta","[0-100%]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%","%<?custom-linebreak?>[P.R.]","Delta","[0-100%]","")
     footnote<-"[P.R.]=Popolazione Riferimento"
    }
   } else if (language=="english") {
    title<-paste("Tabella 3.",i,".2 - Process Indicators",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%","%[R.P.]","Delta","[0-100%]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N Num./Den.","%","%<?custom-linebreak?>[R.P.]","Delta","[0-100%]","")
     footnote<-"[R.P.]=Reference Population"
    }
   }

   if (reference=="") {

    writeTable(file="report.xml",
     data=table3_2,
     vars=c("desc","num_den","perc"),
     headlabs=headlabs,
     headwidt=c("500pt","100pt","50pt"),
     colalign=c("left","right","right"),
     varcolalign=c("align_1","align_2","align_3"),
     footlabs=headlabs,
     title=title,
     graph=NULL)

   } else {

    writeTable(file="report.xml",
     data=table3_2,
     vars=c("desc","num_den","perc","ref_perc","delta","hist","deltarrow"),
     headlabs=headlabs,
     headwidt=c("320pt","90pt","60pt","60pt","50pt","90pt","20pt"),
     colalign=c("left","right","right","right","right","center","center"),
     varcolalign=c("align_1","align_2","align_3","align_4","align_5","align_6","align_7"),
     headalign=c("left","center","center","center","center","center","center"),
     footlabs=footlabs,
     title=title,
     graph=NULL)

   }

  } # end print condition

  table3_2$disease<-as.integer(i)
  table3_2$row<-rownames(table3_2)

  if (i>1) {output_table3_2<-rbind(output_table3_2,table3_2)} else {output_table3_2<-table3_2}

  rm(table3_2)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("..completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("..completed\r",sep=""))
   }
  }

  #####################################################
  # Ricoveri

  # Initialize

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Elaborazione dati Tabella 3.3 avviata..",sep=""))
   } else if (language=="english") {
    cat(paste("Processing data Table 3.3 started..",sep=""))
   }
  }

  if (reference!="") {
   table3_3<- data.frame(matrix(ncol=6,nrow=3))
   names(table3_3)<-c("desc","n","perc","align_1","align_2","align_3")
  } else {
   table3_3<- data.frame(matrix(ncol=14,nrow=3))
   names(table3_3)<-c("desc","n","perc","ref_perc","delta","hist","deltarrow","align_1","align_2","align_3","align_4","align_5","align_6","align_7")
  }

  table3_3[,"n"]<-"0"
  table3_3[,"perc"] <-"-"
  table3_3[,"align_1"]<-"right"
  table3_3[,"align_2"]<-"right"
  table3_3[,"align_3"]<-"right"

  if (reference!="") {
   table3_3[,"ref_perc"]<-"-"
   table3_3[,"delta"]   <-"-"
   table3_3[,"hist"]    <-""
   table3_3[,"sig"]     <-0
   table3_3[,"align_4"]<-"right"
   table3_3[,"align_5"]<-"right"
   table3_3[,"align_6"]<-"center"
   table3_3[,"align_7"]<-"center"
  }

  ################

  if (language=="italian") {
   table3_3[1,"desc"]<-"Totale Ricoveri"
  } else {
   table3_3[1,"desc"]<-"Total Discharges"
  }

  if (tot_pop>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1,"SUM_RIC"])
   n_tot<-tot_pop_pat
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round((n/n_tot)*100,2)
   table3_3[1,"n"]   <-format(n,big.mark=bigmark)
   table3_3[1,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  } else {
   table3_3[1,"n"]<-"0"
   table3_3[1,"perc"]<-"-"
  }

  table3_3[1,"align_1"]<-"left"

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,"SUM_RIC"])
    n_tot_ref<-sum(ref_data$SUM_RIC)
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round((n_ref/n_tot_ref)*100,2)
    table3_3[1,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_3[1,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_3_t1_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_3[1,"sig"]<-sig
     table3_3[1,"hist"]<-paste("<graphic fileref='d_3_3_t1_",i,".pdf'></graphic>",sep="")
    }
   }

  }

  ################

  if (language=="italian") {
   table3_3[2,"desc"]<-"riferibili ad N Soggetti"
  } else {
   table3_3[2,"desc"]<-"related to N Subjects"
  }

  if (tot_pop>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1,"SUM_PAZ_RIC"])
   n_tot<-tot_pop_pat
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round((n/n_tot)*100,2)
   table3_3[2,"n"]   <-format(n,big.mark=bigmark)
   table3_3[2,"perc"]<-format(perc,decimal.mark=decimalmark,nsmall=2)
  }

  table3_3[2,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,"SUM_PAZ_RIC"])
    n_tot_ref<-sum(ref_data$SUM_PAZ_RIC)
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round((n_ref/n_tot_ref)*100,2)
    table3_3[2,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)
    if (n>0) {
     table3_3[2,"delta"]<-format(perc-ref_perc,decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_3_3_t2_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref))
     table3_3[2,"sig"]<-sig
     table3_3[2,"hist"]<-paste("<graphic fileref='d_3_3_t2_",i,".pdf'></graphic>",sep="")
    }
   }

  }

  ################

  if (language=="italian") {
   table3_3[3,"desc"]<-"N. medio di ricoveri per soggetto ricoverato"
  } else {
   table3_3[3,"desc"]<-"Av. no. discharges per subject discharged"
  }

  if (tot_pop>0) {
   n<-sum(input_data[input_data[list_patologie[i]]==1,"SUM_PAZ_RIC"])
   n_tot<-sum(input_data[input_data[list_patologie[i]]==1,"SUM_RIC"])
   n_tot_2<-sum(input_data[input_data[list_patologie[i]]==1,"SUM_RIC_2"])
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round(n_tot/n,2)
   table3_3[3,"n"]   <-format(perc,decimal.mark=decimalmark,nsmall=2)
   table3_3[3,"perc"]<-""
  }

  table3_3[3,"align_1"]<-"right"

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,"SUM_PAZ_RIC"])
    n_tot_ref<-sum(ref_data[ref_data[list_patologie[i]]==1,"SUM_RIC"])
    n_tot_ref_2<-sum(ref_data[ref_data[list_patologie[i]]==1,"SUM_RIC_2"])
   } else {
    n_ref<-0
   }

   if (n_ref>0) {

    ref_perc<-round((n_tot_ref/n_ref),2)
    table3_3[3,"ref_perc"]<-format(ref_perc,decimal.mark=decimalmark,nsmall=2)

    if (n>0) {
     table3_3[3,"delta"]<-format(perc-ref_perc,big.mark=bigmark,scientific=FALSE)
     # Variance(aX)=a^2*Variance(X)
     # Variance(X)=E(X^2)-E(X)^2
     var_tot_pro=(1/n)^2*((n_tot_2/n)-(n_tot/n)^2)
     var_tot_ref_pro=(1/n_ref)^2*((n_tot_ref_2/n_ref)-(n_tot_ref/n_ref)^2)
     se_tot_pro=sqrt(var_tot_pro)/sqrt(tot_pop_pat)
     se_ref_tot_pro=sqrt(var_tot_ref_pro)/sqrt(ref_tot_pop_pat)
     minidot(file=paste("d_3_3_t3_",i,sep=""),mean=c(perc,ref_perc),n=c(n,n_ref),se=c(se_tot_pro,se_ref_tot_pro),min=0,max=5)
     table3_3[3,"sig"]<-sig
     table3_3[3,"hist"]<-paste("<graphic fileref='d_3_3_t3_",i,".pdf'></graphic>",sep="")
    }
   }

  }

  #######################

  if (reference!="") {
   table3_3[,"deltarrow"]<-""
   table3_3[,"deltarrow"]<-ifelse(table3_3$sig==2,arrow_up,table3_3[,"deltarrow"])
   table3_3[,"deltarrow"]<-ifelse(table3_3$sig==1,arrow_down,table3_3[,"deltarrow"])
  }

  #######################

  if (xml==1) {

   if (language=="italian") {
    title<-paste("Tabella 3.",i,".3 - Ricoveri",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","% [P.R.]","Delta","[0-100%]*","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","%<?custom-linebreak?>[P.R.]","Delta","[0-100%*]","")
     footnote<-"[P.R.]=Popolazione Riferimento"
    }
   } else if (language=="english") {
    title<-paste("Tabella 3.",i,".3 - Discharges",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%")
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","% [R.P.]","Delta","[0-100%]*","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),"N","%","%<?custom-linebreak?>[R.P.]","Delta","[0-100%]*","")
     footnote<-"[R.P.]=Reference Population"
    }
   }

   if (reference=="") {

    writeTable(file="report.xml",
     data=table3_3,
     vars=c("desc","n","perc"),
     headlabs=headlabs,
     headwidt=c("500pt","100pt","50pt"),
     colalign=c("left","right","right"),
     varcolalign=c("align_1","align_2","align_3"),
     footlabs=headlabs,
     title=title,
     graph=NULL)

   } else {

    writeTable(file="report.xml",
     data=table3_3,
     vars=c("desc","n","perc","ref_perc","delta","hist","deltarrow"),
     headlabs=headlabs,
     headwidt=c("320pt","90pt","60pt","60pt","50pt","90pt","20pt"),
     colalign=c("left","right","right","right","right","center","right"),
     headalign=c("left","center","center","center","center","center","center"),
     varcolalign=c("align_1","align_2","align_3","align_4","align_5","align_6","align_7"),
     footlabs=footlabs,
     title=title,
     graph=NULL)

   }

  }

  table3_3$disease<-as.integer(i)
  table3_3$row<-rownames(table3_3)

  if (i>1) {output_table3_3<-rbind(output_table3_3,table3_3)} else {output_table3_3<-table3_3}

  rm(table3_3)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("completed\r",sep=""))
   }
  }

  ########################################################################################
  # Consumi
  ########################################################################################

  # Initialize

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Elaborazione dati Tabella 3.4 avviata..",sep=""))
   } else if (language=="english") {
    cat(paste("Processing data Table 3.4 started..",sep=""))
   }
  }

  if (reference=="") {
   table3_4<- data.frame(matrix(ncol=6,nrow=3+length(list_costi)))
   names(table3_4)<-c("desc","tot_euro","tot_pro","align_1","align_2","align_3")
   table3_4[,"align_1"]<-"left"
   table3_4[,"align_2"]<-"right"
   table3_4[,"align_3"]<-"right"
  } else {
   table3_4<- data.frame(matrix(ncol=14,nrow=3+length(list_costi)))
   names(table3_4)<-c("desc","tot_euro","tot_pro","ref_tot_pro","delta","hist","deltarrow","align_1","align_2","align_3","align_4","align_5","align_6","align_7")
  }

  table3_4[,"tot_euro"]<-"0"
  table3_4[,"tot_pro"] <-"-"
  table3_4[,"align_1"]<-"left"
  table3_4[,"align_2"]<-"right"
  table3_4[,"align_3"]<-"right"

  if (reference!="") {
   table3_4[,"ref_tot_pro"]<-"-"
   table3_4[,"delta"]   <-"-"
   table3_4[,"hist"]    <-""
   table3_4[,"sig"]     <-0
   table3_4[,"align_4"]<-"right"
   table3_4[,"align_5"]<-"right"
   table3_4[,"align_6"]<-"center"
   table3_4[,"align_7"]<-"center"
  }

  #################

  for (j in 1:length(list_costi)) {

   table3_4[j,"desc"]<-list_costi_names[j]

   if (tot_pop>0) {
    n  <-round(sum(input_data[input_data[list_patologie[i]]==1,list_costi[j]]))
    n_2<-round(sum(input_data[input_data[list_patologie[i]]==1,list_costi_2[j]]))
   } else {
    n<-0
   }

   if (n>0) {
    perc<-round(n/tot_pop_pat)
    table3_4[j,"tot_euro"]<-format(n,big.mark=bigmark,scientific=FALSE)
    table3_4[j,"tot_pro"]<-format(perc,big.mark=bigmark,scientific=FALSE)
   }

   if (reference!="") {

    if (ref_tot_pop>0) {
     n_ref<-round(sum(ref_data[ref_data[list_patologie[i]]==1,list_costi[j]]))
     n_ref_2<-round(sum(ref_data[ref_data[list_patologie[i]]==1,list_costi_2[j]]))
    } else {
     n_ref<-0
    }

    if (n_ref>0) {

     ref_perc<-round(n_ref/ref_tot_pop_pat)
     table3_4[j,"ref_tot_pro"]<-format(ref_perc,big.mark=bigmark,scientific=FALSE)

     if (n>0) {
      table3_4[j,"delta"]<-format(perc-ref_perc,big.mark=bigmark,scientific=FALSE)
      # Variance(aX)=a^2*Variance(X)
      # Variance(X)=E(X^2)-E(X)^2
      var_tot_pro=(1/tot_pop_pat)^2*((n_2/tot_pop_pat)-(n/tot_pop_pat)^2)
      var_tot_ref_pro=(1/ref_tot_pop_pat)^2*((n_ref_2/ref_tot_pop_pat)-(n_ref/ref_tot_pop_pat)^2)
      se_tot_pro=sqrt(var_tot_pro)/sqrt(tot_pop_pat)
      se_ref_tot_pro=sqrt(var_tot_ref_pro)/sqrt(ref_tot_pop_pat)
      minidot(file=paste("d_3_4_t",j,"_",i,sep=""),mean=c(perc,ref_perc),n=c(tot_pop_pat,ref_tot_pop_pat),se=c(se_tot_pro,se_ref_tot_pro),min=200,max=4000)
      table3_4[j,"sig"]<-sig
      table3_4[j,"hist"]<-paste("<graphic fileref='d_3_4_t",j,"_",i,".pdf'></graphic>",sep="")
     }
    }
   }

  }

  #################

  if (language=="italian") {
   table3_4[j+1,"desc"]<-"Consumo risorse totali"
  } else {
   table3_4[j+1,"desc"]<-"Total Expenditure"
  }

  if (tot_pop>0) {
   n<-  round(sum(input_data[input_data[list_patologie[i]]==1,"COSTO"]))
   n_2<-round(sum(input_data[input_data[list_patologie[i]]==1,"COSTO_2"]))
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round(n/tot_pop_pat)
   table3_4[j+1,"tot_euro"]<-format(n,big.mark=bigmark,scientific=FALSE)
   table3_4[j+1,"tot_pro"]<-format(perc,big.mark=bigmark,scientific=FALSE)
  }

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-round(sum(ref_data[ref_data[list_patologie[i]]==1,"COSTO"]))
    n_ref_2<-round(sum(ref_data[ref_data[list_patologie[i]]==1,"COSTO_2"]))
   } else {
    n_ref<-0
   }

   if (n_ref>0) {

    ref_perc<-round(n_ref/ref_tot_pop_pat)
    table3_4[j+1,"ref_tot_pro"]<-format(ref_perc,big.mark=bigmark,scientific=FALSE)

    if (n>0) {
     table3_4[j+1,"delta"]<-format(perc-ref_perc,big.mark=bigmark,scientific=FALSE)
     # Variance(aX)=a^2*Variance(X)
     # Variance(X)=E(X^2)-E(X)^2
     var_tot_pro=(1/tot_pop_pat)^2*((n_2/tot_pop_pat)-(n/tot_pop_pat)^2)
     var_tot_ref_pro=(1/ref_tot_pop_pat)^2*((n_ref_2/ref_tot_pop_pat)-(n_ref/ref_tot_pop_pat)^2)
     se_tot_pro=sqrt(var_tot_pro)/sqrt(tot_pop_pat)
     se_ref_tot_pro=sqrt(var_tot_ref_pro)/sqrt(ref_tot_pop_pat)
     minidot(file=paste("d_3_4_t",j+1,"_",i,sep=""),mean=c(perc,ref_perc),n=c(tot_pop_pat,ref_tot_pop_pat),se=c(se_tot_pro,se_ref_tot_pro),min=200,max=4000)
     table3_4[j+1,"sig"]<-sig
     table3_4[j+1,"hist"]<-paste("<graphic fileref='d_3_4_t",j+1,"_",i,".pdf'></graphic>",sep="")
    }
   }

  }

  #################

  if (language=="italian") {
   table3_4[j+2,"desc"]<-"di cui con patologia isolata"
  } else {
   table3_4[j+2,"desc"]<-"with only disease"
  }

  table3_4[j+2,"align_1"]<-"right"

  if (tot_pop>0) {
   n<-  round(sum(input_data[input_data[list_patologie[i]]==1 & rowSums(input_data[list_patologie])==1,"COSTO"]))
   n_2<-round(sum(input_data[input_data[list_patologie[i]]==1 & rowSums(input_data[list_patologie])==1,"COSTO_2"]))
  } else {
   n<-0
  }

  if (n>0) {
   perc<-round(n/tot_pop_pat)
   table3_4[j+2,"tot_euro"]<-format(n,big.mark=bigmark,scientific=FALSE)
   table3_4[j+2,"tot_pro"]<-format(perc,big.mark=bigmark,scientific=FALSE)
  }

  if (reference!="") {

   if (ref_tot_pop>0) {
    n_ref<-round(sum(ref_data[ref_data[list_patologie[i]]==1 & rowSums(ref_data[list_patologie])==1,"COSTO"]))
    n_ref_2<-round(sum(ref_data[ref_data[list_patologie[i]]==1 & rowSums(ref_data[list_patologie])==1,"COSTO_2"]))
   } else {
    n_ref<-0
   }

   if (n_ref>0) {
    ref_perc<-round(n_ref/ref_tot_pop_pat)
    table3_4[j+2,"ref_tot_pro"]<-format(ref_perc,big.mark=bigmark,scientific=FALSE)
    if (n>0) {
     table3_4[j+2,"delta"]<-format(perc-ref_perc,big.mark=bigmark,scientific=FALSE)
     # Variance(aX)=a^2*Variance(X)
     # Variance(X)=E(X^2)-E(X)^2
     var_tot_pro=(1/tot_pop_pat)^2*((n_2/tot_pop_pat)-(n/tot_pop_pat)^2)
     var_tot_ref_pro=(1/ref_tot_pop_pat)^2*((n_ref_2/ref_tot_pop_pat)-(n_ref/ref_tot_pop_pat)^2)
     se_tot_pro=sqrt(var_tot_pro)/sqrt(tot_pop_pat)
     se_ref_tot_pro=sqrt(var_tot_ref_pro)/sqrt(ref_tot_pop_pat)
     minidot(file=paste("d_3_4_t",j+2,"_",i,sep=""),mean=c(perc,ref_perc),n=c(tot_pop_pat,ref_tot_pop_pat),se=c(se_tot_pro,se_ref_tot_pro),min=200,max=4000)
     table3_4[j+2,"sig"]<-sig
     table3_4[j+2,"hist"]<-paste("<graphic fileref='d_3_4_t",j+2,"_",i,".pdf'></graphic>",sep="")
    }
   }

  }

  #################

  if (language=="italian") {
   table3_4[j+3,"desc"]<-"di cui con patologia piu' frequente"
  } else {
   table3_4[j+3,"desc"]<-"with most frequent disease"
  }

  table3_4[j+3,"align_1"]<-"right"

  if (tot_pop_pat>0) {
   if (nrow(table_comb_table)>0) {
    n<-table_comb_table[1,"COUNT"]
    n_tot<-round(sum(table_comb_table[1,"COSTO"]))
    perc<-round(n_tot/n)
    table3_4[j+3,"tot_euro"]<-format(n_tot,big.mark=bigmark,scientific=FALSE)
    table3_4[j+3,"tot_pro"]<-format(perc,big.mark=bigmark,scientific=FALSE)
   } else {
    n<-0
   }
  }

  if (reference!="") {

   if (ref_tot_pop_pat>0) {

    n_ref<-table_ref_comb_table[1,"COUNT"]
    n_ref_tot<-round(sum(table_ref_comb_table[1,"COSTO"]))

    if (n_ref>0) {

     ref_perc<-round(n_ref_tot/n_ref)
     table3_4[j+3,"ref_tot_pro"]<-format(ref_perc,big.mark=bigmark,scientific=FALSE)

     if (n>0) {
      table3_4[j+3,"delta"]<-format(perc-ref_perc,big.mark=bigmark,scientific=FALSE)
      # Variance(aX)=a^2*Variance(X)
      # Variance(X)=E(X^2)-E(X)^2
      var_tot_pro=(1/tot_pop_pat)^2*((n_2/tot_pop_pat)-(n/tot_pop_pat)^2)
      var_tot_ref_pro=(1/ref_tot_pop_pat)^2*((n_ref_2/ref_tot_pop_pat)-(n_ref/ref_tot_pop_pat)^2)
      se_tot_pro=sqrt(var_tot_pro)/sqrt(tot_pop_pat)
      se_ref_tot_pro=sqrt(var_tot_ref_pro)/sqrt(ref_tot_pop_pat)
      minidot(file=paste("d_3_4_t",j+3,"_",i,sep=""),mean=c(perc,ref_perc),n=c(tot_pop_pat,ref_tot_pop_pat),se=c(se_tot_pro,se_ref_tot_pro),min=200,max=4000)
      table3_4[j+3,"sig"]<-sig
      table3_4[j+3,"hist"]<-paste("<graphic fileref='d_3_4_t",j+3,"_",i,".pdf'></graphic>",sep="")
     }
    }

   } #nrow

  } # reference

  #######################

  if (reference!="") {
   table3_4[,"deltarrow"]<-""
   table3_4[,"deltarrow"]<-ifelse(table3_4$sig==2,arrow_up,table3_4[,"deltarrow"])
   table3_4[,"deltarrow"]<-ifelse(table3_4$sig==1,arrow_down,table3_4[,"deltarrow"])
  }

  #######################

  if (xml==1) {

   if (language=="italian") {
    labs_total<-"Totale (Euro)"
    labs_percapita<-"Procap. (Euro)"
    title<-paste("Tabella 3.",i,".4 - Consumo Risorse",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita)
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita,paste(labs_percapita," [P.R.]",sep=""),"Delta","[200-4000€]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita,paste(labs_percapita,"<?custom-linebreak?>[P.R.]",sep=""),"Delta","[200-4000€]","")
     footnote<-"[P.R.]=Popolazione Riferimento; *[0-5] per 'Numero medio di ricoveri'"
    }
   } else if (language=="english") {
    labs_total<-"Total (€)"
    labs_percapita<-"Per cap. (Euro)"
    title<-paste("Tabella 3.",i,".4 - Expenditure",sep="")
    if (reference=="") {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita)
    } else {
     headlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita,paste(labs_percapita," [R.P.]",sep=""),"Delta","[200-4000€]","")
     footlabs<-c(paste(labs,list_patologie_names[i],sep=""),labs_total,labs_percapita,paste(labs_percapita,"<?custom-linebreak?>[R.P.]",sep=""),"Delta","[200-4000€]","")
     footnote<-"[R.P.]=Reference Population; *[0-5] per 'Av. No. Discharges'"
    }
   }

   if (reference=="") {

    writeTable(file="report.xml",
     data=table3_4,
     vars=c("desc","tot_euro","tot_pro"),
     headlabs=headlabs,
     headwidt=c("500pt","100pt","50pt"),
     colalign=c("left","right","right"),
     headalign=c("left","center","center"),
     varcolalign=c("align_1","align_2","align_3"),
     footlabs=headlabs,
     title=title,
     graph=NULL)

   } else {

    writeTable(file="report.xml",
     data=table3_4,
     vars=c("desc","tot_euro","tot_pro","ref_tot_pro","delta","hist","deltarrow"),
     headlabs=headlabs,
     headwidt=c("320pt","90pt","60pt","60pt","50pt","90pt","20pt"),
     colalign=c("left","right","right","right","right","center","right"),
     headalign=c("left","center","center","center","center","center","center"),
     varcolalign=c("align_1","align_2","align_3","align_4","align_5","align_6","align_7"),
     footlabs=footlabs,
     footnote=footnote,
     title=title,
     graph=NULL)

   }

   fileConn<-file("report.xml",open="at")
   if (i<length(list_patologie)) {
    writeLines('<?custom-pagebreak?>',fileConn)
   }
   close(fileConn)

  }

  table3_4$disease<-as.integer(i)
  table3_4$row<-rownames(table3_4)

  if (i>1) {output_table3_4<-rbind(output_table3_4,table3_4)} else {output_table3_4<-table3_4}

  rm(table3_4)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("completed\r",sep=""))
   }
  }

 } # Patologia

 table3_1<-output_table3_1
 table3_2<-output_table3_2
 table3_3<-output_table3_3
 table3_4<-output_table3_4

 rm(output_table3_1,output_table3_2,output_table3_3,output_table3_4)

 ##############################################################################

 table3_1$ASL<-operator
 table3_1$YEAR<-year

 table3_1<-table3_1[,c("ASL","YEAR","row","disease","desc","n","perc")]

 table3_1$Anno<-as.integer(table3_1$YEAR)
 table3_1$CodiceASL<-table3_1$ASL
 table3_1$Patologia<-table3_1$disease
 table3_1$CodiceRiga<-table3_1$row
 table3_1$NumeroAssistiti<-as.integer(gsub(bigmark,"",table3_1$n,fixed=TRUE))
 table3_1$Percentuale<-suppressWarnings(ifelse(table3_1$perc!="-",as.numeric(gsub(decimalmark,".",table3_1$perc,fixed=TRUE)),""))

 table3_1<-table3_1[,c("Anno","CodiceASL","CodiceRiga","Patologia","NumeroAssistiti","Percentuale")]

 ##############################

 table3_2$ASL<-operator
 table3_2$YEAR<-year

 table3_2<-table3_2[,c("ASL","YEAR","row","disease","desc","n_num","n_den","perc")]

 table3_2$Anno<-as.integer(table3_2$YEAR)
 table3_2$CodiceASL<-table3_2$ASL
 table3_2$Patologia<-table3_2$disease
 table3_2$CodiceIndicatore<-table3_2$row
 table3_2$NumeroAssistitiNumeratore<-as.integer(table3_2$n_num)
 table3_2$NumeroAssistitiDenominatore<-as.integer(table3_2$n_den)
 table3_2$Percentuale<-suppressWarnings(ifelse(table3_2$perc!="-",as.numeric(gsub(decimalmark,".",table3_2$perc,fixed=TRUE)),""))

 table3_2<-table3_2[,c("Anno","CodiceASL","CodiceIndicatore","Patologia","NumeroAssistitiNumeratore","NumeroAssistitiDenominatore","Percentuale")]

 ##############################

 table3_3$ASL<-operator
 table3_3$YEAR<-year

 table3_3<-table3_3[,c("ASL","YEAR","row","disease","desc","n","perc")]

 table3_3$Anno<-as.integer(table3_3$YEAR)
 table3_3$CodiceASL<-table3_3$ASL
 table3_3$Patologia<-table3_3$disease
 table3_3$CodiceRiga<-table3_3$row
 table3_3$NumeroAssistiti<-as.integer(gsub(bigmark,"",table3_3$n,fixed=TRUE))
 table3_3$Percentuale<-suppressWarnings(ifelse(table3_3$perc!="-",as.numeric(gsub(decimalmark,".",table3_3$perc,fixed=TRUE)),""))

 table3_3<-table3_3[,c("Anno","CodiceASL","CodiceRiga","Patologia","NumeroAssistiti","Percentuale")]

 ##############################

 table3_4$ASL<-operator
 table3_4$YEAR<-year

 table3_4<-table3_4[,c("ASL","YEAR","row","disease","desc","tot_euro","tot_pro")]

 table3_4$Anno<-as.integer(table3_4$YEAR)
 table3_4$CodiceASL<-table3_4$ASL
 table3_4$Patologia<-table3_4$disease
 table3_4$CodiceRiga<-table3_4$row
 table3_4$Totale<-as.numeric(gsub(bigmark,"",table3_4$tot_euro,fixed=TRUE))
 table3_4$Procapite<-suppressWarnings(ifelse(table3_4$tot_pro!="-",as.numeric(gsub(bigmark,".",table3_4$tot_pro,fixed=TRUE)),""))

 table3_4<-table3_4[,c("Anno","CodiceASL","CodiceRiga","Patologia","Totale","Procapite")]

 ###############################################

 coding3_1<- data.frame(matrix(ncol=4,nrow=5))
 names(coding3_1)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 if (language=="italian") {
  coding3_1[1,"Descrizione"]<-"Prevalenza nella popolazione"
  coding3_1[2,"Descrizione"]<-"di cui con patologia isolata"
  coding3_1[3,"Descrizione"]<-"di cui con la combinazione di patologie piu' frequente"
  coding3_1[4,"Descrizione"]<-"di cui donne"
  coding3_1[5,"Descrizione"]<-"di cui di eta'>=75"
 } else {
  coding3_1[1,"Descrizione"]<-"Population prevalence"
  coding3_1[2,"Descrizione"]<-"Only disease"
  coding3_1[3,"Descrizione"]<-"With most frequent combination"
  coding3_1[4,"Descrizione"]<-"Women"
  coding3_1[5,"Descrizione"]<-"Aged 75 and over"
 }
 coding3_1$SezioneTabella<-3
 coding3_1$CodiceTabella<-1
 coding3_1$CodiceRiga<-as.numeric(rownames(coding3_1))

 ####

 ntot_indicators<-0
 for (j in 1:length(list_patologie)) {
  for (k in 1:length(indicatori[[j]])) {
   ntot_indicators<-ntot_indicators+1
  }
 }

 coding3_2<- data.frame(matrix(ncol=5,nrow=ntot_indicators))
 names(coding3_2)<-c("SezioneTabella","CodiceTabella","CodicePatologia","CodiceIndicatore","Descrizione")

 h<-0
 for (j in 1:length(list_patologie)) {
  for (k in 1:length(indicatori[[j]])) {
   h<-h+1
   coding3_2[h,"CodicePatologia"]<-j
   coding3_2[h,"CodiceIndicatore"]<-k
   coding3_2[h,"Descrizione"]<-indicatori_desc_short[[j]][k]
  }
 }

 coding3_2$SezioneTabella<-3
 coding3_2$CodiceTabella<-2

 ####

 coding3_3<- data.frame(matrix(ncol=4,nrow=3))
 names(coding3_3)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 if (language=="italian") {
  coding3_3[1,"Descrizione"]<-"Totale Ricoveri"
  coding3_3[2,"Descrizione"]<-"riferibili ad N Soggetti"
  coding3_3[3,"Descrizione"]<-"N. medio di ricoveri per soggetto ricoverato"
 } else {
  coding3_3[1,"Descrizione"]<-"Total Discharges"
  coding3_3[2,"Descrizione"]<-"related to N subjects"
  coding3_3[3,"Descrizione"]<-"Average No. Discharge per Hospitalized Patient"
 }
 coding3_3$SezioneTabella<-3
 coding3_3$CodiceTabella<-3
 coding3_3$CodiceRiga<-as.numeric(rownames(coding3_3))

 ####

 coding3_4<- data.frame(matrix(ncol=4,nrow=(length(list_costi_names)+3)))
 names(coding3_4)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 for (i in 1:length(list_costi_names)) {
  coding3_4[i,"Descrizione"]<-list_costi_names[i]
 }
 if (language=="italian") {
  coding3_4[length(list_costi_names)+1,"Descrizione"]<-"Consumo Risorse Totali"
  coding3_4[length(list_costi_names)+2,"Descrizione"]<-"Consumo Risorse con patologia isolata"
  coding3_4[length(list_costi_names)+3,"Descrizione"]<-"Consumo Risorse con la combinazione di patologie piu' frequente"
 } else {
  coding3_4[length(list_costi_names)+1,"Descrizione"]<-"Total Expenditure"
  coding3_4[length(list_costi_names)+2,"Descrizione"]<-"Expenditure for Only disease"
  coding3_4[length(list_costi_names)+3,"Descrizione"]<-"Expenditure for most frequent combination"
 }

 coding3_4$SezioneTabella<-3
 coding3_4$CodiceTabella<-4
 coding3_4$CodiceRiga<-as.numeric(rownames(coding3_4))

 ##############################

 coding3<-rbind(coding3_1,coding3_3,coding3_4)
 coding_indicatori3<-coding3_2

 rm(coding3_1,coding3_2,coding3_3,coding3_4)

 list_percent_names<-c(list_patologie_names,"GLOBAL")

 coding_patologie3<- data.frame(matrix(ncol=2,nrow=length(list_percent_names)))
 names(coding_patologie3)<-c("CodicePatologia","Descrizione")

 for (j in 1:length(list_percent_names)) {
  coding_patologie3[j,"Descrizione"]<-list_percent_names[j]
 }
 coding_patologie3$CodicePatologia<-as.numeric(rownames(coding_patologie3))

 ##############################

 if (output!="") {

  data_output<-list(table3_1,table3_2,table3_3,table3_4)
  files_output<-c("table3_1","table3_2","table3_3","table3_4")
  files_desc<-c("Tabella 3.1","Tabella 3.2","Tabella 3.3","Tabella 3.4")

  if (verbose==1) {
   cat("\r")
  }

  for (i in 1:length(data_output)) {
   write.csv(data_output[[i]],paste(output,"/",files_output[i],".csv",sep=""),row.names=FALSE,na="")
   if (verbose==1) {
    if (language=="italian") {
     cat(paste("Tabella risultati ",files_desc[i]," salvata in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    } else if (language=="english") {
     cat(paste("Table of Results ",files_desc[i]," saved in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    }
   }
  }

  write.csv(coding3,paste(output,"/CodificheRighe.csv",sep=""),row.names=FALSE,na="")
  write.csv(coding_indicatori3,paste(output,"/CodificheIndicatori.csv",sep=""),row.names=FALSE,na="")
  write.csv(coding_patologie3,paste(output,"/CodifichePatologie.csv",sep=""),row.names=FALSE,na="")

  if (verbose==1) {
   if (language=="italian") {
    cat(paste("Tabella Codifiche Righe salvata in file: [",workDir,"/CodificheRighe.csv]\r",sep=""))
    cat(paste("Tabella Codifiche Indicatori salvata in file: [",workDir,"/CodificheIndicatori.csv]\r",sep=""))
    cat(paste("Tabella Codifiche Patologie salvata in file: [",workDir,"/CodificheIndicatori.csv]\r",sep=""))
   } else if (language=="english") {
    cat(paste("Table Coding Rows saved in file: [",workDir,"/CodificheRighe.csv]\r",sep=""))
    cat(paste("Table Coding Indicator saved in file: [",workDir,"/CodificheIndicatori.csv]\r",sep=""))
    cat(paste("Table Coding Diseases saved in file: [",workDir,"/CodificheIndicatori.csv]\r",sep=""))
   }
  }

 }

 ####################################################################################################

 if (xml==1) {
  if (engine_type!="") {rm(input_data)}
  rm(table3_1,table3_2,table3_3,table3_4,coding3,coding_patologie3)
 } else {

  # Make table available globally
  assign("table3_1",table3_1,envir=.GlobalEnv)
  assign("table3_2",table3_2,envir=.GlobalEnv)
  assign("table3_3",table3_3,envir=.GlobalEnv)
  assign("table3_4",table3_4,envir=.GlobalEnv)
  assign("coding3",coding3,envir=.GlobalEnv)
  assign("coding_indicatori3",coding_indicatori3,envir=.GlobalEnv)
  assign("coding_patologie3",coding_patologie3,envir=.GlobalEnv)

 }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 3",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 3",sep=""))
  }
  cat("\r")
 }

}

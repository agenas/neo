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

indicator5 <- function(xml=1,graphs=1,target=NULL,target_name="",output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 5: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 5: started processing\r")
  }
  cat("########################################\r")
 }

 # xml=0 indicator in a loop xml=1 indicator directly to report

 # Set the working directory
 setwd(workDir)

 ##########################################
 # Grafici 5
 ##########################################
 ##########################################

 colour_list=c("indianred2","grey")
 colour_values<-c()
 selectolevo<-c()
 colour_var<-NULL

 check_error<-0

 for (i in 1:length(PkgsNeed)) {
  suppressMessages(library(PkgsNeed[i],character.only=TRUE))
 }

 unit<-target
 unit_name<-target_name
 dfoutput<-"dfout"

 ########### Load Data

 if (engine_type=="local")  {
  input_data<-merge_table(c("db_master","db_aveind_hyperte","db_aveind_diab","db_aveind_ihd","db_aveind_hf","db_aveind_demen","db_pctind_hyperte","db_pctind_diab","db_pctind_ihd","db_pctind_hf","db_pctind_demen","db_globind_num","db_globind_den","db_cost","db_discharge"))
  input_data<-make_numeric(input_data,c(list_patologie,list_costi_ext,list_costi_ext_2,list_numvars,list_highscore,list_ricoveri,"COUNT"))
 } else if (engine_type=="central") {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 if (reference!="") {

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
   cat(paste("of which N=",tot_pat," observations including subjects with diseases\r",sep=""))
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
    cat(paste("of which N=",ref_tot_pat," observations including subjects with diseases\r",sep=""))
   }
  }
 }

 tot_unit<-0
 if (nrow(input_data)>0) {
  tot_unit<-nrow(input_data[input_data[unit]!="",])
 }

 if (tot_unit==0) {
  totale<-0
  tot_pat<-0
 }

 if (tot_pat==0) {graphs<-0}

 if (totale==0) {

  table<-data.frame(matrix(ncol=2,nrow=1))
  names(table)<-c("error","desc")

  if (language=="italian") {
   section<-"Sezione 5. Grafici di dispersione"
   title="Presenza di soggetti nel dataset di input"
   labs=c("Tipo Errore","Descrizione")
   table[1,"error"]<-"Dati incoerenti"
   table[1,"desc"] <-"I Funnel Plots non possono essere prodotti: nessun soggetto presente"
   if (tot_unit==0) {
    table[1,"desc"] <-paste(table[1,"desc"]," con codice valido per ",unit,sep="")
   }
  } else if (language=="english") {
   section<-"Section 5. Dispersion graphs"
   title="Presence of subjects in input dataset"
   labs=c("Error Type","Description")
   table[1,"error"]<-"Inconsistent Data"
   table[1,"desc"] <-"Funnel Plots cannot be displayed: no subject selected"
   if (tot_unit==0) {
    table[1,"desc"] <-paste(table[1,"desc"]," with valid code for ",unit,sep="")
   }
  }

  writeTable(file="report.xml",
    data=table,
    append=append,
    vars=c("error","desc"),
    headlabs=labs,
    headwidt=c("140pt","500pt"),
    colalign=c("left","left"),
    footlabs=labs,
    title=title,
    section=section,
    graph=NULL)

  append<-1
  check_error<-1
  rm(table)

  # Prevalence
  table5_1<- data.frame(matrix(ncol=8,nrow=0))
  names(table5_1)<-c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")

 }

 if (tot_pat==0) { # no diseases: only prevalence

  # Discharges
  table5_2<- data.frame(matrix(ncol=8,nrow=0))
  names(table5_2)<-c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")

  # Average Process Indicators
  table5_3<- data.frame(matrix(ncol=9,nrow=0))
  names(table5_3)<-c("Anno","CodiceASL","CodiceProvider","CodicePatologia","CodiceIndicatore","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")

  # Average Costs
  table5_4<- data.frame(matrix(ncol=8,nrow=0))
  names(table5_4)<-c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","CostoTotaleProcapite","StandardError","FlagOutlier")

  # Global Scores
  table5_5<- data.frame(matrix(ncol=8,nrow=0))
  names(table5_5)<-c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")

 }

 if (tot_pat==0) {

  table<-data.frame(matrix(ncol=2,nrow=1))
  names(table)<-c("error","desc")

  if (language=="italian") {
   if (check_error==0) {section<-"Sezione 5. Grafici di dispersione"} else {section<-""}
   title="Presenza delle patologie indicate nel dataset in input"
   labs=c("Tipo Errore","Descrizione")
   table[1,"error"]<-"Dati incoerenti"
   table[1,"desc"] <-"Nessuna patologia nei soggetti in input: i funnel plot non saranno prodotti"
  } else if (language=="english") {
   if (append==0) {section<-"Section 5. Dispersion graphs"} else {section<-""}
   title="Presence of selected diseases in input dataset"
   labs=c("Error Type","Description")
   table[1,"error"]<-"Inconsistent Data"
   table[1,"desc"] <-"No disease present in input dataset: funnel plots will not be produced"
  }

  writeTable(file="report.xml",
    data=table,
    append=append,
    vars=c("error","desc"),
    headlabs=labs,
    headwidt=c("140pt","500pt"),
    colalign=c("left","left"),
    footlabs=labs,
    title=title,
    section=section,
    graph=NULL)

  append<-1
  check_error<-1
  rm(table)

 }

 if (reference!="" & ref_tot_pat==0) {

  table<-data.frame(matrix(ncol=2,nrow=1))
  names(table)<-c("error","desc")

  if (language=="italian") {
   if (check_error==0) {section<-"Sezione 5. Grafici di dispersione"} else {section<-""}
   title="Presenza delle patologie indicate nel riferimento"
   labs=c("Tipo Errore","Descrizione")
   table[1,"error"]<-"Dati incoerenti"
   table[1,"desc"] <-"Il riferimento prescelto non contiene dati sufficienti per i Funnel Plots. La linea di riferimento orizzontale rappresenta la media pesata dei punti"
  } else if (language=="english") {
   if (check_error==0) {section<-"Section 5. Dispersion graphs"} else {section<-""}
   title="Presence of selected diseases in reference population"
   labs=c("Error Type","Description")
   table[1,"error"]<-"Inconsistent Data"
   table[1,"desc"] <-"The selected reference does not include sufficient data to be used in Funnel Plots. The horizontal reference line represents the weighted average of the points used in the graph"
  }

  writeTable(file="report.xml",
    data=table,
    append=append,
    vars=c("error","desc"),
    headlabs=labs,
    headwidt=c("140pt","500pt"),
    colalign=c("left","left"),
    footlabs=labs,
    title=title,
    section=section,
    graph=NULL)

  if (check_error==0) {
   fileConn<-file("report.xml",open="at")
   writeLines('<?custom-pagebreak?>',fileConn)
   close(fileConn)
  }

  append<-1
  check_error<-1
  rm(table)

 }

 if (totale>0) { # totale>0

  ############## Graph labels

  if (language=="italian") {
   ylab<-"Indicatore per 100"
   xlab<-"Totale assistiti"
   selectlab<-"Selezione"
   selectlev<-c("Si","No")
  } else if (language=="english") {
   ylab<-"Indicator per 100"
   xlab<-"Total N"
   selectlab<-"Selection"
   selectlev<-c("Yes","No")
  }

  ##################################

  for (i in 1:length(list_patologie)) {

   if (verbose>0) {
    cat("\r")
    if (language=="italian") {
     cat(paste("[Elaborazione patologia ",list_patologie_names[i]," avviata]\r",sep=""))
    } else if (language=="english") {
     cat(paste("[Processing disease ",list_patologie_names[i]," started]\r",sep=""))
    }
   }
   ############# Funnel Prevalence

   if (language=="italian") {
    title<-"Prevalenza"
   } else if (language=="english") {
    title<-"Prevalence"
   }

   #### select disease

   if (tot_pat>0) {
    dataset_disease<-input_data[input_data[list_patologie[i]]==1,]
   }

   if (reference!="" & ref_tot_pat>0) {
    dataset_ref_disease<-ref_data[ref_data[list_patologie[i]]==1,]
   }

   # isolate names

   if (funnel_group!="") {

    colour_n<-c()

    dataset_1<-do.call(subset,list(x=input_data,subset=(parse(text=paste("(",funnel_group," & ",unit,"!='')==TRUE",sep="")))))
    if (nrow(dataset_1)>0) {
     dataset_1<-dataset_1[,c("COUNT",unit,list_patologie[i])]
     names(dataset_1) <- c("COUNT","UNIT","VAR")
     dataset_1$FUNNEL_GROUP<-1
     colour_n<-c(1)          # 1 is value for selection=yes
    } else {
     dataset_1<-data.frame(matrix(ncol=4,nrow=0))
     names(dataset_1) <- c("COUNT","UNIT","VAR","FUNNEL_GROUP")
    }

    dataset_2<-do.call(subset,list(x=input_data,subset=(parse(text=paste("(",funnel_group," | ",unit,"=='')==FALSE",sep="")))))

    if (nrow(dataset_2)>0) {
     dataset_2<-dataset_2[,c("COUNT",unit,list_patologie[i])]
     names(dataset_2) <- c("COUNT","UNIT","VAR")
     dataset_2$FUNNEL_GROUP<-2
     colour_n<-c(colour_n,2) # 2 is value for selection=no
    } else {
     dataset_2<-data.frame(matrix(ncol=4,nrow=0))
     names(dataset_2) <- c("COUNT","UNIT","VAR","FUNNEL_GROUP")
    }

    dataset<-rbind(dataset_1,dataset_2)
    rm(dataset_1,dataset_2)

    totale<-aggregate(x=dataset$COUNT,by=list(dataset$UNIT,dataset$VAR),FUN="sum")
    names(totale) <- c("UNIT","VAR","COUNT")

    total_units<-aggregate(x=dataset$COUNT,by=list(dataset$UNIT,dataset$FUNNEL_GROUP),FUN="sum")
    names(total_units) <- c("UNIT","FUNNEL_GROUP","COUNT")
    names_units<-c("UNIT","FUNNEL_GROUP")

   } else {

    dataset<-input_data[,c("COUNT",unit,list_patologie[i])]
    names(dataset) <- c("COUNT","UNIT","VAR")

    totale<-aggregate(x=dataset$COUNT,by=list(dataset$UNIT,dataset$VAR),FUN="sum")
    names(totale) <- c("UNIT","VAR","COUNT")

    total_units<-aggregate(x=dataset$COUNT,by=list(dataset$UNIT),FUN="sum")
    names(total_units) <- c("UNIT","COUNT")

    names_units<-c("UNIT")

   }

   rm(dataset)

   if (nrow(totale[totale$VAR==1,])>0) {
    totale_pat<- totale[totale$VAR==1,c("UNIT","COUNT")]
    names(totale_pat)<-c("UNIT","N_pat")
    totale_pat<-merge(total_units[c("UNIT")],totale_pat,by="UNIT",all.x=TRUE)
   } else {
    totale_pat<-total_units[c("UNIT")]
    totale_pat[c("N_pat")]<-0
   }

   if (nrow(totale[totale$VAR==0,])>0) {
    totale_nopat<- totale[totale$VAR==0,c("UNIT","COUNT")]
    names(totale_nopat)<-c("UNIT","N_nopat")
    totale_nopat<-merge(total_units[c("UNIT")],totale_nopat,by="UNIT",all.x=TRUE)
   } else {
    totale_nopat<-total_units[c("UNIT")]
    totale_nopat[c("N_nopat")]<-0
   }

   totale_prev<-merge(totale_pat,totale_nopat,by="UNIT")
   totale_prev<-totale_prev[totale_prev$UNIT!="",] # erase missing

   totale_prev$N_pat<-ifelse(is.na(totale_prev$N_pat),0,totale_prev$N_pat)
   totale_prev$N_nopat<-ifelse(is.na(totale_prev$N_nopat),0,totale_prev$N_nopat)

   totale_prev$N<-totale_prev$N_pat+totale_prev$N_nopat
   totale_prev$PERC<-(totale_prev$N_pat/totale_prev$N)*100

   if (funnel_group!="") {
    colour_var<-NULL
    totale_prev<-merge(totale_prev,total_units[names_units],by="UNIT")
    if (length(colour_n)>0) {colour_var<-totale_prev$FUNNEL_GROUP}
    colour_values<-c()
    selectolevo<-c()
    for (w in 1:length(colour_n)) {
     colour_values<-c(colour_values,colour_list[colour_n[w]])
     selectolevo<-c(selectolevo,selectlev[colour_n[w]])
    }
   }

   rm(totale,total_units,totale_pat,totale_nopat)

   ########## Reference data

   if (reference!="" & ref_tot_pat>0) { # compute p mean from reference data
    p_mean<-(sum(dataset_ref_disease[,"COUNT"])/ref_totale)*100
   } else { # no reference
    p_mean<-"weighted"
   }

   if (verbose>0) {
    if (language=="italian") {
     cat(paste("Stai elaborando dati di prevalenza per  N= ",nrow(totale_prev)," unita' di ",unit," \r",sep=""))
    } else if (language=="english") {
     cat(paste("Processing prevalence data for N= ",nrow(totale_prev)," units of ",unit," \r",sep=""))
    }
   }

   #print(totale_prev)

   funnel_plot(title=title,
               names=totale_prev$UNIT,
               names_outliers=1,
               colour_var=colour_var,
               colour_values=colour_values,
               rate=totale_prev$PERC,
               unit=100,
               population=totale_prev$N,
               binary=1,
               p_mean=p_mean,
               filename="",
               graph="prevalence",
               pdf_height=3.5,
               pdf_width=6,
               ylab=ylab,
               xlab=xlab,
               selectlab=selectlab,
               selectlev=selectolevo,
               dot_size=dot_size,
               dfout=dfoutput)

   dfout$disease<-as.integer(i)
   if (i>1) {table5_1<-rbind(table5_1,dfout)} else {table5_1<-dfout}
   rm(dfout,totale_prev)

   ###############  Funnel Discharges

   if (tot_pat>0) {

    if (language=="italian") {
     title<-"Ricoveri"
    } else if (language=="english") {
     title<-"Discharges"
    }

    if (funnel_group!="") {

     colour_n<-c()

     paz_ric_1<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," & ",unit,"!='')==TRUE",sep="")))))
     if (nrow(paz_ric_1)>0) {
      paz_ric_1<-paz_ric_1[,c(unit,"SUM_PAZ_RIC","COUNT")]
      names(paz_ric_1) <- c("UNIT","SUM_PAZ_RIC","COUNT")
      paz_ric_1$FUNNEL_GROUP<-1
      colour_n<-c(1)
     } else {
      paz_ric_1<-data.frame(matrix(ncol=4,nrow=0))
      names(paz_ric_1) <- c("UNIT","SUM_PAZ_RIC","COUNT","FUNNEL_GROUP")
     }

     paz_ric_2<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," | ",unit,"=='')==FALSE",sep="")))))
     if (nrow(paz_ric_2)>0) {
      paz_ric_2<-paz_ric_2[,c(unit,"SUM_PAZ_RIC","COUNT")]
      names(paz_ric_2) <- c("UNIT","SUM_PAZ_RIC","COUNT")
      paz_ric_2$FUNNEL_GROUP<-2
      colour_n<-c(colour_n,2)
     } else {
      paz_ric_2<-data.frame(matrix(ncol=4,nrow=0))
      names(paz_ric_2) <- c("UNIT","SUM_PAZ_RIC","COUNT","FUNNEL_GROUP")
     }

     paz_ric<-rbind(paz_ric_1,paz_ric_2)
     rm(paz_ric_1,paz_ric_2)

     output_paz_ric<-aggregate(x=paz_ric[c("SUM_PAZ_RIC","COUNT")],by=paz_ric[c("UNIT","FUNNEL_GROUP")],FUN="sum")
     names(output_paz_ric)<-c("UNIT","FUNNEL_GROUP","SUM_PAZ_RIC","COUNT")

    } else  {

     paz_ric<-dataset_disease[c(unit,"SUM_PAZ_RIC","COUNT")]
     names(paz_ric) <- c("UNIT","SUM_PAZ_RIC","COUNT")

     output_paz_ric<-aggregate(x=paz_ric[c("SUM_PAZ_RIC","COUNT")],by=paz_ric[c("UNIT")],FUN="sum")
     names(output_paz_ric)<-c("UNIT","SUM_PAZ_RIC","COUNT")

    }

    output_paz_ric$PERC<-(output_paz_ric$SUM_PAZ_RIC/output_paz_ric$COUNT)*100
    output_paz_ric<-output_paz_ric[output_paz_ric$UNIT!="",] # erase missing

    if (funnel_group!="") {
     colour_var<-NULL
     if (length(colour_n)>0) {colour_var<-output_paz_ric$FUNNEL_GROUP}
     colour_values<-c()
     selectolevo<-c()
     for (w in 1:length(colour_n)) {
      colour_values<-c(colour_values,colour_list[colour_n[w]])
      selectolevo<-c(selectolevo,selectlev[colour_n[w]])
     }
    }

    if (reference!="" & ref_tot_pat>0) { # compute p mean from reference data
     p_mean<-(sum(dataset_ref_disease[,"SUM_PAZ_RIC"])/sum(dataset_ref_disease[,"COUNT"]))*100
    } else { # no reference
     p_mean<-"weighted"
    }

    if (verbose>0) {
     if (language=="italian") {
      cat(paste("Stai elaborando dati sui ricoveri per N= ",nrow(output_paz_ric)," unita' di ",unit," \r",sep=""))
     } else if (language=="english") {
      cat(paste("Processing discharge data for N= ",nrow(output_paz_ric)," units of ",unit," \r",sep=""))
     }
    }

    # print(output_paz_ric)

    funnel_plot(title=title,
                names=output_paz_ric$UNIT,
                names_outliers=1,
                colour_var=colour_var,
                colour_values=colour_values,
                rate=output_paz_ric$PERC,
                unit=100,
                binary=1,
                p_mean=p_mean,
                population=output_paz_ric$COUNT,
                graph="discharges",
                filename="",
                ylab=ylab,
                xlab=xlab,
                selectlab=selectlab,
                selectlev=selectolevo,
                dot_size=dot_size,
                dfout=dfoutput)

    dfout$disease<-as.integer(i)
    if (i>1) {table5_2<-rbind(table5_2,dfout)} else {table5_2<-dfout}
    rm(dfout,output_paz_ric)

    if (graphs==1) {

     if (language=="italian") {
      if (i==1) {section<-"Sezione 5. Grafici di dispersione"} else {section<-""}
      multititle<-paste(": Prevalenza e Ricoveri per ",unit_name)
     } else if (language=="english") {
      if (i==1) {section<-"Section 5. Dispersion graphs"} else {section<-""}
      multititle<-paste(": Prevalence and Discharges by ",unit_name)
     }

     multigraph_grobs(list_graphs=c("prevalence","discharges"),
                      nrow=2,ncol=1,
                      title=paste(list_patologie_names[i],multititle,sep=""),
                      filename=paste("5_1_2_funnelplot_prev_",list_patologie_names[i],sep=""))
     if (i>1) {
      section<-""
      append<-1
     }

     writeTable(file="report.xml",
      data=NULL,
      append=append,
      vars="",
      headlabs="",
      headwidt="",
      colalign="",
      footlabs="",
      title=multititle,
      section=section,
      graph=paste("5_1_2_funnelplot_prev_",list_patologie_names[i],"_1.pdf",sep=""))

     if (verbose>0) {
      if (language=="italian") {
       cat(paste("Funnel plot sezione 5.1-2 salvati in file: [",workDir,"/5_1_2_funnelplot_prev_",list_patologie_names[i],"_1.pdf] \r",sep=""))
       cat(paste("Funnel plot sezione 5.1-2 salvati in file: [",workDir,"/5_1_2_funnelplot_prev_",list_patologie_names[i],"_1.png] \r",sep=""))
      } else if (language=="english") {
       cat(paste("Funnel plots section 5.1-2 saved in file: [",workDir,"/5_1_2_funnelplot_prev_",list_patologie_names[i],"_1.pdf] \r",sep=""))
       cat(paste("Funnel plots section 5.1-2 saved in file: [",workDir,"/5_1_2_funnelplot_prev_",list_patologie_names[i],"_1.png] \r",sep=""))
      }
     }

     fileConn<-file("report.xml",open="at")
     writeLines('<?custom-pagebreak?>',fileConn)
     close(fileConn)

    } # graphs

    #################### Funnel all Indicators for each chronic disease

    n_indicators<-length(indicatori[[i]])

    list_graphs<-c()

    for (p in 1:n_indicators) {

     if (verbose>0) {
      if (language=="italian") {
       cat(paste(">>> Avviata Elaborazione Indicatore di Processo: ",indicatori_desc_short[[i]][p],sep=""))
      } else if (language=="english") {
       cat(paste(">>> Started Processing Process Indicator: ",indicatori_desc_short[[i]][p],sep=""))
      }
     }

     num_name<-paste("NUM_",indicatori[[i]][p],"_",list_patologie[i],sep="")
     den_name<-paste("DEN_",indicatori[[i]][p],"_",list_patologie[i],sep="")

     if (funnel_group!="") {

      colour_n<-c()

      table_output_1<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," & ",unit,"!='')==TRUE",sep="")))))
      if (nrow(table_output_1)>0) {
       table_output_1<-table_output_1[,c(unit,num_name,den_name,"COUNT")]
       names(table_output_1)<- c("UNIT",num_name,den_name,"COUNT")
       table_output_1$FUNNEL_GROUP<-1
       colour_n<-c(1)
      } else {
       table_output_1<-data.frame(matrix(ncol=4,nrow=0))
       names(table_output_1)<- c("UNIT",num_name,den_name,"COUNT")
      }

      table_output_2<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," | ",unit,"=='')==FALSE",sep="")))))
      if (nrow(table_output_2)>0) {
       table_output_2<-table_output_2[,c(unit,num_name,den_name,"COUNT")]
       names(table_output_2)<- c("UNIT",num_name,den_name,"COUNT")
       table_output_2$FUNNEL_GROUP<-2
       colour_n<-c(colour_n,2)
      } else {
       table_output_2<-data.frame(matrix(ncol=5,nrow=0))
       names(table_output_2)<- c("UNIT",num_name,den_name,"COUNT","FUNNEL_GROUP")
      }

      table_output<-rbind(table_output_1,table_output_2)

      rm(table_output_1,table_output_2)

      table<-aggregate(x=table_output[c(num_name,den_name,"COUNT")],by=table_output[c("UNIT","FUNNEL_GROUP")],FUN="sum")
      names(table)<-c("UNIT","FUNNEL_GROUP",num_name,den_name,"COUNT")

      rm(table_output)

     } else  {

      table<-aggregate(x=dataset_disease[c(num_name,den_name,"COUNT")],by=dataset_disease[c(unit)],FUN="sum")
      names(table)<-c("UNIT",num_name,den_name,"COUNT")

     }

     table$PERC<-ifelse(table[,c(den_name)]>0,((table[,c(num_name)])/(table[,c(den_name)])*100),0)
     table<-table[table$UNIT!="",] # erase missing

     if (funnel_group!="") {
      colour_var<-NULL
      if (length(colour_n)>0) {colour_var<-table$FUNNEL_GROUP}
      colour_values<-c()
      selectolevo<-c()
      for (w in 1:length(colour_n)) {
       colour_values<-c(colour_values,colour_list[colour_n[w]])
       selectolevo<-c(selectolevo,selectlev[colour_n[w]])
      }
     }

     if (reference!="" & ref_tot_pat>0) { # compute p mean from reference data
      p_mean<-weighted.mean(dataset_ref_disease[,num_name]/dataset_ref_disease[,den_name],dataset_ref_disease[,den_name],na.rm=TRUE)*100
     } else { # no reference
      p_mean<-"weighted"
     }

     graph<-paste("fp",p,sep="")
     list_graphs<-c(list_graphs,graph)

     title<-indicatori_desc_short[[i]][p]

     if (verbose>0) {
      if (language=="italian") {
       cat(paste("; N= ",nrow(table)," unita' di ",unit," \r",sep=""))
      } else if (language=="english") {
       cat(paste("; N= ",nrow(table)," units of ",unit," \r",sep=""))
      }
     }

     # print(table)

     funnel_plot(rate=table$PERC,
                 names=table$UNIT,
                 names_outliers=1,
                 unit=100,
                 p_mean=p_mean,
                 colour_var=colour_var,
                 colour_values=colour_values,
                 population=table[,c("COUNT")],
                 title=title,
                 ylab=ylab,
                 xlab=xlab,
                 selectlab=selectlab,
                 selectlev=selectolevo,
                 graph=graph,
                 filename="",
                 dot_size=dot_size+0.2,
                 dfout=dfoutput)

     dfout$disease<-as.integer(i)
     dfout$indicator<-as.numeric(p)
     if (i>1 | p>1) {
      table5_3<-rbind(table5_3,dfout)
     } else {table5_3<-dfout}
     rm(dfout,table)

    } # n indicators

    if (graphs==1) {

     if (language=="italian") {
      title<-paste(list_patologie_names[i],": MEDIE indicatori di processo per ",unit_name,sep="")
     } else if (language=="english") {
      title<-paste(list_patologie_names[i],": AVERAGE process indicators by ",unit_name,sep="")
     }

     multigraph_grobs(list_graphs=list_graphs,
                      nrow=3,ncol=2,
                      title=title,
                      filename=paste("5_3_funnelplot_indicators_",list_patologie_names[i],sep=""))

     n_pages<-ceiling(length(list_graphs)/6)

     for (page in 1:n_pages) {

      writeTable(file="report.xml",
        data=NULL,
        vars="",
        headlabs="",
        headwidt="",
        colalign="",
        footlabs="",
        title=title,
        graph=paste("5_3_funnelplot_indicators_",list_patologie_names[i],"_",page,".pdf",sep=""))

        if (verbose>0) {
         if (language=="italian") {
          cat(paste("Funnel plot sezione 5.3 salvati in file: [",workDir,"/5_3_funnelplot_indicators_",list_patologie_names[i],"_",page,".pdf]\r",sep=""))
          cat(paste("Funnel plot sezione 5.3 salvati in file: [",workDir,"/5_3_funnelplot_indicators_",list_patologie_names[i],"_",page,".png]\r",sep=""))
         } else if (language=="english") {
          cat(paste("Funnel plots section 5.3 saved in file: [",workDir,"/5_3_funnelplot_indicators_",list_patologie_names[i],"_",page,".pdf]\r",sep=""))
          cat(paste("Funnel plots section 5.3 saved in file: [",workDir,"/5_3_funnelplot_indicators_",list_patologie_names[i],"_",page,".png]\r",sep=""))
         }
        }
     }

    }

    #################### Funnel Costs

    costi_names<-c(list_costi_names_reduced,"Totale")
    list_graphs<-c()

    for (h in 1:length(list_costi_ext)) {

     list_graphs<-c(list_graphs,paste("costs_",h,sep=""))

     if (funnel_group!="") {

      colour_n<-c()

      paz_euro_1<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," & ",unit,"!='')==TRUE",sep="")))))
      if (nrow(paz_euro_1)>0) {
       paz_euro_1<-paz_euro_1[,c(unit,list_costi_ext[h],list_costi_ext_2[h],"COUNT")]
       names(paz_euro_1) <- c("UNIT",list_costi_ext[h],list_costi_ext_2[h],"COUNT")
       paz_euro_1$FUNNEL_GROUP<-1
       colour_n<-c(1)
      } else {
       paz_euro_1<-data.frame(matrix(ncol=5,nrow=0))
       names(paz_euro_1)<- c("UNIT",list_costi_ext[h],list_costi_ext_2[h],"COUNT","FUNNEL_GROUP")
      }

      paz_euro_2<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," | ",unit,"=='')==FALSE",sep="")))))
      if (nrow(paz_euro_2)>0) {
       paz_euro_2<-paz_euro_2[,c(unit,list_costi_ext[h],list_costi_ext_2[h],"COUNT")]
       names(paz_euro_2) <- c("UNIT",list_costi_ext[h],list_costi_ext_2[h],"COUNT")
       paz_euro_2$FUNNEL_GROUP<-2
       colour_n<-c(colour_n,2)
      } else {
       paz_euro_2<-data.frame(matrix(ncol=5,nrow=0))
       names(paz_euro_2)<- c("UNIT",list_costi_ext[h],list_costi_ext_2[h],"COUNT","FUNNEL_GROUP")
      }

      paz_euro<-rbind(paz_euro_1,paz_euro_2)

      rm(paz_euro_1,paz_euro_2)

      output_paz_euro<-aggregate(x=paz_euro[c(list_costi_ext[h],list_costi_ext_2[h],"COUNT")],by=paz_euro[c("UNIT","FUNNEL_GROUP")],FUN="sum")
      names(output_paz_euro)<-c("UNIT","FUNNEL_GROUP",list_costi_ext[h],list_costi_ext_2[h],"COUNT")

     } else  {

      output_paz_euro<-aggregate(x=dataset_disease[c(list_costi_ext[h],list_costi_ext_2[h],"COUNT")],by=dataset_disease[c(unit)],FUN="sum")
      names(output_paz_euro)<-c("UNIT",list_costi_ext[h],list_costi_ext_2[h],"COUNT")

     }

     output_paz_euro$EURO<-output_paz_euro[,list_costi_ext[h]]/output_paz_euro$COUNT # Average cost by unit
     output_paz_euro<-output_paz_euro[output_paz_euro$UNIT!="",] # erase missing

     if (funnel_group!="") {
      colour_var<-NULL
      if (length(colour_n)>0) {colour_var<-output_paz_euro$FUNNEL_GROUP}
      colour_values<-c()
      selectolevo<-c()
      for (w in 1:length(colour_n)) {
       colour_values<-c(colour_values,colour_list[colour_n[w]])
       selectolevo<-c(selectolevo,selectlev[colour_n[w]])
      }
     }

     if (reference!="" & ref_tot_pat>0) { # compute p mean from reference data
      p_mean <- weighted.mean(dataset_ref_disease[,list_costi_ext[h]]/dataset_ref_disease$COUNT,dataset_ref_disease$COUNT,na.rm=TRUE) # Weighted by N
      se_euro<-sqrt(   sum(dataset_ref_disease[,list_costi_ext_2[h]])/sum(dataset_ref_disease$COUNT) -
                      (sum(dataset_ref_disease[,list_costi_ext[h]])  /sum(dataset_ref_disease$COUNT))^2
                   ) # standard error formula: mean of square minus square of mean
     } else { # no reference
      p_mean<-"weighted"
      se_euro<-sqrt(   sum(dataset_disease[,list_costi_ext_2[h]])/sum(dataset_disease$COUNT) -
                      (sum(dataset_disease[,list_costi_ext[h]])  /sum(dataset_disease$COUNT))^2
                   ) # standard error formula: mean of square minus square of mean
     }

     if (h==1 & verbose>0) {
      if (language=="italian") {
       cat(paste("Stai elaborando dati sui costi per N= ",nrow(output_paz_euro)," unita' di ",unit," \r",sep=""))
      } else if (language=="english") {
       cat(paste("Processing data on costs for N= ",nrow(output_paz_euro)," units of ",unit," \r",sep=""))
      }
     }

     # print(output_paz_euro)

     funnel_plot(title=costi_names[h],
                 binary=0,
                 names=output_paz_euro$UNIT,
                 names_outliers=1,
                 colour_var=colour_var,
                 colour_values=colour_values,
                 rate=output_paz_euro$EURO,
                 p_mean=p_mean,
                 p_se=se_euro,
                 unit=100,
                 ylab="Euro",
                 xlab=xlab,
                 selectlab=selectlab,
                 selectlev=selectolevo,
                 dot_size=dot_size,
                 population=output_paz_euro$COUNT,
                 graph=paste("costs_",h,sep=""),
                 filename="",
                 dfout=dfoutput)

     dfout$disease<-as.integer(i)
     dfout$cost<-as.integer(h)
     if (i>1 | h>1) {table5_4<-rbind(table5_4,dfout)} else {table5_4<-dfout}
     rm(dfout)

    } # list_costi

    if (graphs==1) {

     if (language=="italian") {
      title<-paste(list_patologie_names[i],": costi pro capite per ",unit_name,sep="")
     } else if (language=="english") {
      title<-paste(list_patologie_names[i],": costs per capita by ",unit_name,sep="")
     }

     multigraph_grobs(list_graphs=list_graphs,
                      nrow=3,ncol=2,
                      title=title,
                      filename=paste("5_4_funnelplot_costs_",list_patologie_names[i],sep=""))

     n_pages<-ceiling(length(list_graphs)/6)

     for (page in 1:n_pages) {

      writeTable(file="report.xml",
        data=NULL,
        vars="",
        headlabs="",
        headwidt="",
        colalign="",
        footlabs="",
        title=title,
        graph=paste("5_4_funnelplot_costs_",list_patologie_names[i],"_",page,".pdf",sep=""))

        if (verbose>0) {
         if (language=="italian") {
          cat(paste("Funnel plot sezione 5.4 salvati in file: [",workDir,"/5_4_funnelplot_costs_",list_patologie_names[i],"_",page,".pdf]\r",sep=""))
          cat(paste("Funnel plot sezione 5.4 salvati in file: [",workDir,"/5_4_funnelplot_costs_",list_patologie_names[i],"_",page,".png]\r",sep=""))
         } else if (language=="english") {
          cat(paste("Funnel plots section 5.4 saved in file: [",workDir,"/5_4_funnelplot_costs_",list_patologie_names[i],"_",page,".pdf]\r",sep=""))
          cat(paste("Funnel plots section 5.4 saved in file: [",workDir,"/5_4_funnelplot_costs_",list_patologie_names[i],"_",page,".png]\r",sep=""))
         }
        }
     }

     fileConn<-file("report.xml",open="at")
     writeLines('<?custom-pagebreak?>',fileConn)
     close(fileConn)

    }

    ############### Clean

    rm(dataset_disease)

    if (reference!="" & ref_tot_pat>0) {
     rm(dataset_ref_disease)
    }

   } # tot_pat

  } # loop over diseases

  if (tot_pat>0) {

   ################################################
   # Percentage of subjects above threshold (60%)
   ################################################

   list_graphs<-c()

   for (i in 1:length(list_percent)) {

    if (i<length(list_percent)) {
     dataset_disease<-input_data[input_data[list_patologie[i]]==1,]
     if (reference!="" & ref_tot_pat>0) {
      dataset_ref_disease<-ref_data[ref_data[list_patologie[i]]==1,]
     }
    } else {
     dataset_disease<-input_data
     if (reference!="" & ref_tot_pat>0) {
      dataset_ref_disease<-ref_data
     }
    }

    num_name<-paste("NUM_HIGHSCORE_",list_percent[i],sep="")
    den_name<-paste("DEN_HIGHSCORE_",list_percent[i],sep="")

    if (funnel_group!="") {

     colour_n<-c()

     table_output_1<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," & ",unit,"!='')==TRUE",sep="")))))
     if (nrow(table_output_1)>0) {
      table_output_1<-table_output_1[,c(unit,num_name,den_name,"COUNT")]
      names(table_output_1)<-c("UNIT",num_name,den_name,"COUNT")
      table_output_1$FUNNEL_GROUP<-1
      colour_n<-c(1)
     } else {
      table_output_1<-data.frame(matrix(ncol=5,nrow=0))
      names(table_output_1)<- c("UNIT",num_name,den_name,"COUNT","FUNNEL_GROUP")
     }

     table_output_2<-do.call(subset,list(x=dataset_disease,subset=(parse(text=paste("(",funnel_group," | ",unit,"=='')==FALSE",sep="")))))
     if (nrow(table_output_2)>0) {
      table_output_2<-table_output_2[,c(unit,num_name,den_name,"COUNT")]
      names(table_output_2) <- c("UNIT",num_name,den_name,"COUNT")
      table_output_2$FUNNEL_GROUP<-2
      colour_n<-c(colour_n,2)
     } else {
      table_output_2<-data.frame(matrix(ncol=5,nrow=0))
      names(table_output_2)<- c("UNIT",num_name,den_name,"COUNT","FUNNEL_GROUP")
     }

     table_output<-rbind(table_output_1,table_output_2)

     rm(table_output_1,table_output_2)

     table<-aggregate(x=table_output[c(num_name,den_name,"COUNT")],by=table_output[c("UNIT","FUNNEL_GROUP")],FUN="sum")
     names(table)<-c("UNIT","FUNNEL_GROUP",num_name,den_name,"COUNT")

     rm(table_output)

    } else  {

     table<-aggregate(x=dataset_disease[c(num_name,den_name,"COUNT")],by=dataset_disease[c(unit)],FUN="sum")
     names(table)<-c("UNIT",num_name,den_name,"COUNT")

    }

    table$PERC<-ifelse(table[,c(den_name)]>0,((table[,c(num_name)])/(table[,c(den_name)])*100),0)
    table<-table[table$UNIT!="",] # erase missing

    if (funnel_group!="") {
     colour_var<-NULL
     if (length(colour_n)>0) {colour_var<-table$FUNNEL_GROUP}
     colour_values<-c()
     selectolevo<-c()
     for (w in 1:length(colour_n)) {
      colour_values<-c(colour_values,colour_list[colour_n[w]])
      selectolevo<-c(selectolevo,selectlev[colour_n[w]])
     }
    }

    if (reference!="" & ref_tot_pat>0) { # compute p mean from reference data
     p_mean<-weighted.mean((dataset_ref_disease[,num_name]/dataset_ref_disease[,den_name])*100,dataset_ref_disease[,den_name],na.rm=TRUE)
    } else { # no reference
     p_mean<-"weighted"
    }

    graph<-paste("fc",i,sep="")
    list_graphs<-c(list_graphs,graph)

    if (i<length(list_percent)) {
     title<-list_patologie_names[i]
    } else {
     if (language=="italian") {
      title<-paste("GLOBALE per ",unit_name,sep="")
     } else if (language=="english") {
      title<-paste("GLOBAL by ",unit_name,sep="")
     }
    }

    if (verbose>0) {
     if (i<length(list_percent)) {
      if (language=="italian") {
       cat(paste("Stai elaborando dati su indicatori compositi per la patologia ",list_patologie_names[i]," su N= ",nrow(table)," unita' di ",unit," \r",sep=""))
      } else if (language=="english") {
       cat(paste("Processing data on composite indicators for disease ",list_patologie_names[i]," on N= ",nrow(table)," units of ",unit," \r",sep=""))
      }
     } else {
      if (language=="italian") {
       cat(paste("Stai elaborando dati su indicatori compositi per la globalita' delle patologie su N= ",nrow(table)," unita' di ",unit," \r",sep=""))
      } else if (language=="english") {
       cat(paste("Processing data on composite indicators for all diseases on N= ",nrow(table)," units of ",unit," \r",sep=""))
      }
     }
    }

    # print(table)

    funnel_plot(rate=table$PERC,
                names=table$UNIT,
                names_outliers=1,
                unit=100,
                p_mean=p_mean,
                colour_var=colour_var,
                colour_values=colour_values,
                population=table[,c("COUNT")],
                title=title,
                ylab=ylab,
                xlab=xlab,
                selectlab=selectlab,
                selectlev=selectolevo,
                graph=graph,
                filename="",
                dot_size=dot_size+0.2,
                dfout=dfoutput)

    dfout$disease<-as.integer(i)
    if (i>1) {table5_5<-rbind(table5_5,dfout)} else {table5_5<-dfout}
    rm(dfout,table,dataset_disease)

    if (reference!="" & ref_tot_pat>0) {
     rm(dataset_ref_disease)
    }

   } # patologie

   if (graphs==1) {

    if (language=="italian") {
     title<-paste("% SOGGETTI PATOLOGICI con oltre 60% di aderenza per ",unit_name,sep="")
    } else if (language=="english") {
     title<-paste("% SUBJECTS w DISEASE with over 60% adherence per ",unit_name,sep="")
    }

    multigraph_grobs(list_graphs=list_graphs,
                     nrow=3,ncol=2,
                     title=title,
                     filename=paste("5_5_funnelplot_global_",list_percent_names[i],sep=""))

    n_pages<-ceiling(length(list_graphs)/6)

    for (page in 1:n_pages) {

     writeTable(file="report.xml",
       data=NULL,
       vars="",
       headlabs="",
       headwidt="",
       colalign="",
       footlabs="",
       title=title,
       graph=paste("5_5_funnelplot_global_",list_percent_names[i],"_",page,".pdf",sep=""))

     if (verbose>0) {
      if (language=="italian") {
       cat(paste("Funnel plot sezione 5.5 salvati in file: [",workDir,"/5_5_funnelplot_global_",list_percent_names[i],"_",page,".pdf]\r",sep=""))
       cat(paste("Funnel plot sezione 5.5 salvati in file: [",workDir,"/5_5_funnelplot_global_",list_percent_names[i],"_",page,".png]\r",sep=""))
      } else if (language=="english") {
       cat(paste("Funnel plots section 5.5 saved in file: [",workDir,"/5_5_funnelplot_global_",list_percent_names[i],"_",page,".pdf]\r",sep=""))
       cat(paste("Funnel plots section 5.5 saved in file: [",workDir,"/5_5_funnelplot_global_",list_percent_names[i],"_",page,".png]\r",sep=""))
      }
     }

    } # pages

   } # graphs

  } # end no diseases in input dataset

  if (xml==1) {
   rm(input_data)
  }

  #################################
  # Output Funnel Plots Raw File
  #################################

  if (verbose>0) {
   cat("\r")
   if (language=="italian") {
    cat(paste("Inizio salvataggio dati Funnel Plot in tabelle csv di output\r",sep=""))
   } else if (language=="english") {
    cat(paste("Started saving data Funnel Plots in csv output tables\r",sep=""))
   }
  }

  table5_1<-table5_1[,c("disease","p","number","p.se","names","outlier")]

  table5_1$Anno<-as.integer(year)
  table5_1$CodiceASL<-operator
  table5_1$CodiceProvider<-as.character(table5_1$names)
  table5_1$CodicePatologia<-as.integer(table5_1$disease)
  table5_1$NumeroAssistiti<-as.integer(table5_1$number)
  table5_1$Percentuale<-suppressWarnings(as.numeric(table5_1$p))
  table5_1$StandardError<-suppressWarnings(as.numeric(table5_1$p.se))
  table5_1$FlagOutlier<-as.integer(table5_1$outlier)

  table5_1<-table5_1[,c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")]

 } # totale>0

 if (tot_pat>0) {

  ##############################

  table5_2<-table5_2[,c("disease","p","number","p.se","names","outlier")]

  table5_2$Anno<-as.integer(year)
  table5_2$CodiceASL<-operator
  table5_2$CodiceProvider<-as.character(table5_2$names)
  table5_2$CodicePatologia<-as.integer(table5_2$disease)
  table5_2$NumeroAssistiti<-as.integer(table5_2$number)
  table5_2$Percentuale<-suppressWarnings(as.numeric(table5_2$p))
  table5_2$StandardError<-suppressWarnings(as.numeric(table5_2$p.se))
  table5_2$FlagOutlier<-as.integer(table5_2$outlier)

  table5_2<-table5_2[,c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")]

  ##############################

  table5_3<-table5_3[,c("disease","indicator","p","number","p.se","names","outlier")]

  table5_3$Anno<-as.integer(year)
  table5_3$CodiceASL<-operator
  table5_3$CodiceProvider<-as.character(table5_3$names)
  table5_3$CodicePatologia<-as.integer(table5_3$disease)
  table5_3$CodiceIndicatore<-as.character(table5_3$indicator)
  table5_3$NumeroAssistiti<-as.integer(table5_3$number)
  table5_3$Percentuale<-suppressWarnings(as.numeric(table5_3$p))
  table5_3$StandardError<-suppressWarnings(as.numeric(table5_3$p.se))
  table5_3$FlagOutlier<-as.integer(table5_3$outlier)

  table5_3<-table5_3[,c("Anno","CodiceASL","CodiceProvider","CodicePatologia","CodiceIndicatore","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")]

  ##############################

  table5_4<-table5_4[,c("disease","cost","p","number","p.se","names","outlier")]

  table5_4$Anno<-as.integer(year)
  table5_4$CodiceASL<-operator
  table5_4$CodiceProvider<-as.character(table5_4$names)
  table5_4$CodicePatologia<-as.integer(table5_4$disease)
  table5_4$CodiceCosto<-as.integer(table5_4$cost)
  table5_4$NumeroAssistiti<-as.integer(table5_4$number)
  table5_4$CostoTotaleProcapite<-suppressWarnings(as.numeric(table5_4$p))
  table5_4$StandardError<-suppressWarnings(as.numeric(table5_4$p.se))
  table5_4$FlagOutlier<-as.integer(table5_4$outlier)

  table5_4<-table5_4[,c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","CostoTotaleProcapite","StandardError","FlagOutlier")]

  ##############################

  table5_5<-table5_5[,c("disease","p","number","p.se","names","outlier")]

  table5_5$Anno<-as.integer(year)
  table5_5$CodiceASL<-operator
  table5_5$CodiceProvider<-as.character(table5_5$names)
  table5_5$CodicePatologia<-as.integer(table5_5$disease)
  table5_5$NumeroAssistiti<-as.integer(table5_5$number)
  table5_5$Percentuale<-suppressWarnings(as.numeric(table5_5$p))
  table5_5$StandardError<-suppressWarnings(as.numeric(table5_5$p.se))
  table5_5$FlagOutlier<-as.integer(table5_5$outlier)

  table5_5<-table5_5[,c("Anno","CodiceASL","CodiceProvider","CodicePatologia","NumeroAssistiti","Percentuale","StandardError","FlagOutlier")]

  #########################################################################################

 }

 if (output!="") {

  data_output<-list(table5_1,table5_2,table5_3,table5_4,table5_5)
  files_output<-c("table5_1","table5_2","table5_3","table5_4","table5_5")
  if (language=="italian") {
   files_desc<-c("Prevalenza","Ricoveri","Indicatori","Costi","Globale")
  } else if (language=="english") {
   files_desc<-c("Prevalence","Discharges","Indicators","Costs","Global")
  }

  for (i in 1:length(data_output)) {
   write.csv(data_output[[i]],paste(output,"/",files_output[i],".csv",sep=""),row.names=FALSE,na="")
   if (verbose>0) {
    if (language=="italian") {
     cat(paste("Tabella dati Funnel Plot ",files_desc[i]," salvata in file: ",workDir,"/",files_output[i],".csv\r",sep=""))
    } else if (language=="english") {
     cat(paste("Table data Funnel Plots ",files_desc[i]," saved in file: ",workDir,"/",files_output[i],".csv\r",sep=""))
    }
   }
  }

 }

 #########################################################################################

 if (xml==0) {

  # Make table available globally for further use
  assign("table5_1",table5_1,envir=.GlobalEnv)
  assign("table5_2",table5_2,envir=.GlobalEnv)
  assign("table5_3",table5_3,envir=.GlobalEnv)
  assign("table5_4",table5_4,envir=.GlobalEnv)
  assign("table5_5",table5_5,envir=.GlobalEnv)

 }

# if (graphs==1) {
#  file.remove("Rplots.pdf")
# }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 5",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 5",sep=""))
  }
  cat("\r")
 }

}

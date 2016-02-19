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

indicator2 <- function(xml=1,graphs=1,output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 2: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 2: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ##########################################
 # Input Dataset
 ##########################################

 if (engine_type=="local")  {
  input_data<-merge_table(c("db_master","db_sources_hyperte","db_sources_diab","db_sources_ihd","db_sources_hf","db_sources_demen","db_sources_allpat"))
  input_data<-make_numeric(input_data,c(list_patologie,list_fonti_vars,"COUNT"))
 } else if (engine_type=="central")  {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 ##########################################
 # Reference Dataset
 ##########################################

 if (reference!="") {

  arrow_up  <-"<graphic fileref='resources/arrow_up.png'></graphic>"
  arrow_down<-"<graphic fileref='resources/arrow_down.png'></graphic>"

  ### Reference population is whole input dataset

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

 if (tot_pat==0) {graphs<-0}

 if (language=="italian") {
  total_label<-"TOTALE PATOLOGIE"
 } else if (language=="english") {
  total_label<-"TOTAL DISEASES"
 }

 ##########################################
 # Tabella 2.1
 ##########################################

 ###############################################################

 if (language=="italian") {
  fonti_label<-"Fonti "
  fonti_atleast<-"Almeno una patologia"
  title<-paste(fonti_label,fonti_atleast)
 } else if (language=="english") {
  fonti_label<-"Sources "
  fonti_atleast<-"at least one disease"
  title<-paste(fonti_label,fonti_atleast)
 }

 ################ Check coerenza fonti

 if (tot_pat>0) {

  chk_fonti_error<-rep(0,nrow(input_data))
  for (i in 1:length(list_patologie)) {
   vars_fonti<-c(paste(list_fonti[i],"_1",sep=""),paste(list_fonti[i],"_2",sep=""),paste(list_fonti[i],"_3",sep=""))
   chk_fonti_error<-ifelse(input_data[list_patologie[i]]>0 & sum(input_data[vars_fonti])==0,1,chk_fonti_error)
  }

  sum_chk_fonti_error<-sum(chk_fonti_error)

 } else {
  sum_chk_fonti_error<-0
 }

 ####################################

 if (sum_chk_fonti_error>0) {

  table2_1<-data.frame(matrix(ncol=2,nrow=1))
  names(table2_1)<-c("error","desc")

  if (language=="italian") {
   title<-"Caricamento Fonti - Errore Rilevato"
   section<-"Sezione 2. Inquadramento complessivo delle patologie"
   labs=c("Tipo Errore","Descrizione")
   table2_1[1,"error"]<-"Dati incoerenti"
   table2_1[1,"desc"] <-paste(sum(chk_fonti_error)," combinazioni di patologie non hanno fonti per almeno una condizione identificata",sep="")
  } else if (language=="english") {
   title="Loading Sources - Error"
   section<-"Section 2. General description of diseases"
   labs=c("Error Type","Description")
   table2_1[1,"error"]<-"Inconsistent Data"
   table2_1[1,"desc"] <-paste(sum(chk_fonti_error)," combinations of diseases have no sources identified for at least one condition",sep="")
  }

  writeTable(file="report.xml",
             data=table2_1,append=append,
             vars=c("error","desc"),headlabs=labs,headwidt=c("140pt","500pt"),
             colalign=c("left","left"),footlabs=labs,title=title,section=section,graph=NULL)

 } else {

  if (tot_pat>0) {
   for (m in 1:length(list_patologie)) {
    read_comb(input_table=input_data,patologia=list_patologie[m],fonti=list_fonti[m],output=paste("input_",list_patologie[m],sep=""),venn=paste("venn_",list_patologie[m],sep=""),title=paste(fonti_label,list_patologie_names[m],sep=""),verbose=verbose,graphs=graphs)
   }
   read_comb(input_table=input_data,patologia=list_patologie,fonti=list_fonti,output="input_allpat",venn="venn_allpat",title=title,verbose=verbose,graphs=graphs)
  }

  ###############################################################
  # Initialize

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Elaborazione dati Tabella 2.1 avviata..",sep=""))
   } else if (language=="english") {
    cat(paste("Processing data Table 2.1 started..",sep=""))
   }
  }

  table2_1<- data.frame(matrix(ncol=6,nrow=7))
  names(table2_1)<-c("desc","hosp","exe","drugs","n","perc")

  table2_1[,"hosp"] <-"0"
  table2_1[,"exe"]  <-"0"
  table2_1[,"drugs"]<-"0"
  table2_1[,"n"]    <-"0"
  table2_1[,"perc"] <-"-"

  ###############################################################

  for (m in 1:length(list_patologie)) {
   table2_1[m,"desc"] <-list_patologie_names[m]
   if (sum(input_data[input_data[,list_patologie[m]]==1,"COUNT"])>0) {
    table2_1[m,"hosp"] <-format(get(paste("input_",list_patologie[m],sep=""))[,c("hosp")],big.mark=bigmark)
    table2_1[m,"exe"]  <-format(get(paste("input_",list_patologie[m],sep=""))[,c("exe")],big.mark=bigmark)
    table2_1[m,"drugs"]<-format(get(paste("input_",list_patologie[m],sep=""))[,c("drugs")],big.mark=bigmark)
    table2_1[m,"n"]    <-format(sum(input_data[input_data[,list_patologie[m]]==1,"COUNT"]),big.mark=bigmark)
    table2_1[m,"perc"] <-format(round((sum(input_data[input_data[,list_patologie[m]]==1,"COUNT"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
   }
  }

  if (language=="italian") {
   table2_1[length(list_patologie)+1,"desc"]<-"Totale soggetti con almeno una patologia"
  } else if (language=="english") {
   table2_1[length(list_patologie)+1,"desc"]<-"Total no. subjects with at least one disease"
  }

  if (tot_pat>0) {
   table2_1[length(list_patologie)+1,"hosp"] <-format(sum(input_data[rowSums(input_data[list_patologie])>0,"FONTI_HOSP_ALLPAT"]),big.mark=bigmark)
   table2_1[length(list_patologie)+1,"exe"]  <-format(sum(input_data[rowSums(input_data[list_patologie])>0,"FONTI_EXE_ALLPAT"]),big.mark=bigmark)
   table2_1[length(list_patologie)+1,"drugs"]<-format(sum(input_data[rowSums(input_data[list_patologie])>0,"FONTI_DRUG_ALLPAT"]),big.mark=bigmark)
   table2_1[length(list_patologie)+1,"n"]    <-format(sum(input_data[rowSums(input_data[list_patologie])>0,"COUNT"]),big.mark=bigmark)
   table2_1[length(list_patologie)+1,"perc"] <-format(round((sum(input_data[rowSums(input_data[list_patologie])>0,"COUNT"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
  }

  if (language=="italian") {
   table2_1[length(list_patologie)+2,"desc"]<-"Totale condizioni patologiche riscontrate"
  } else if (language=="english") {
   table2_1[length(list_patologie)+2,"desc"]<-"Total no. disease conditions identified"
  }

  table2_1[length(list_patologie)+2,"hosp"] <-format(sum(as.integer(gsub(bigmark,"",table2_1[1:length(list_patologie),"hosp"] ,fixed=TRUE))),big.mark=bigmark)
  table2_1[length(list_patologie)+2,"exe"]  <-format(sum(as.integer(gsub(bigmark,"",table2_1[1:length(list_patologie),"exe"]  ,fixed=TRUE))),big.mark=bigmark)
  table2_1[length(list_patologie)+2,"drugs"]<-format(sum(as.integer(gsub(bigmark,"",table2_1[1:length(list_patologie),"drugs"],fixed=TRUE))),big.mark=bigmark)
  table2_1[length(list_patologie)+2,"n"]    <-format(sum(as.integer(gsub(bigmark,"",table2_1[1:length(list_patologie),"n"]    ,fixed=TRUE))),big.mark=bigmark)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("completed\r",sep=""))
   }
  }

  if (xml==1) {

   if (language=="italian") {
    title<-"Tabella 2.1 - Fonte utilizzata per l’identificazione degli assistiti presi in carico per patologia cronica"
    section<-"Sezione 2. Inquadramento complessivo delle patologie"
    labs<-c("","Ricoveri","Esenzioni","Farmaci","N","%")
   } else if (language=="english") {
    title="Table 2.1 - Source used to identify subjects affected by a chronic disease"
    section<-"Section 2. General description of diseases"
    labs<-c("","Discharges","Exemptions","Drugs","N","%")
   }

   writeTable(file="report.xml",
     data=table2_1,
     append=append,
     vars=c("desc","hosp","exe","drugs","n","perc"),
     headlabs=labs,
     headwidt=c("280pt","80pt","80pt","80pt","60pt","60pt"),
     colalign=c("left","right","right","right","right","right"),
     headalign=c("center","center","center","center","center","center"),
     footlabs=labs,
     title=title,
     section=section,
     graph=NULL)

  }

  if (graphs==1) {

   list_venn<-c(list_patologie,"allpat")
   list_venn_names<-c(list_patologie_names,fonti_atleast)

   p<-list()
   replica<-c(2/36)
   i<-3
   for (j in 1:ceiling(length(list_venn)/3)) {
    for (k in 1:3) {
     i<-i+1
     p[[i]]<-textGrob(paste(fonti_label,list_venn_names[3*(j-1)+k],sep=""))
    }
    replica<-c(replica,1/36)
    for (k in 1:3) {
     i<-i+1
     p[[i]]<-grobTree(get(paste("venn_",list_venn[3*(j-1)+k],sep="")))
    }
    replica<-c(replica,(16/36)*(2/ceiling(length(list_venn)/3)))
   }

   p[[1]]<-textGrob("")
   p[[2]]<-textGrob("")
   p[[3]]<-textGrob("")

   if (language=="italian") {
    textgrob<-"Percentuale Identificata da ogni fonte per Patologia"
   } else if (language=="english") {
    textgrob<-"Percentage Identified by each Source by Disease"
   }

   args.list <- c(p,list(nrow=ceiling(length(list_venn)/3)*2+1,ncol=3,
                  main=textGrob(textgrob,gp=gpar(lineheight=4,fontsize=20,font=1)),heights=replica))

   pdf(file = "2_1_venndiagram.pdf",height=10,width=14)
   do.call(grid.arrange,args.list)
   dev.off()

   png(file = "2_1_venndiagram.png",height=600,width=840)
   do.call(grid.arrange,args.list)
   dev.off()

   if (language=="italian") {
    title<-"Fonti patologie"
   } else if (language=="english") {
    title<-"Disease Sources"
   }

   writeTable(file="report.xml",title=title,graph="2_1_venndiagram.pdf")

   if (verbose==1) {
    if (language=="italian") {
     cat(paste("Grafico Venn Fonti salvato in file: [",workDir,"/2_1_venndiagram.pdf]\r",sep=""))
     cat(paste("Grafico Venn Fonti salvato in file: [",workDir,"/2_1_venndiagram.png]\r",sep=""))
    } else if (language=="english") {
     cat(paste("Venn Diagram Sources saved in file: [",workDir,"/2_1_venndiagram.pdf]\r",sep=""))
     cat(paste("Venn Diagram Sources saved in file: [",workDir,"/2_1_venndiagram.png]\r",sep=""))
    }
   }

   rm(p)

  } # end graphs

 } # end chk_fonti_error

 if (xml==1) {

  fileConn<-file("report.xml",open="at")
  writeLines('<?custom-pagebreak?>',fileConn)
  close(fileConn)

 }

 ##########################################
 # Tabella 2.2
 ##########################################

 ###############################################################
 # Initialize

 if (reference=="") {
  table2_2<- data.frame(matrix(ncol=3,nrow=length(list_patologie)+2))
  names(table2_2)<-c("desc","n","perc")
 } else {
  table2_2<- data.frame(matrix(ncol=6,nrow=length(list_patologie)+2))
  names(table2_2)<-c("desc","n","perc","ref_perc","delta","hist")
 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Elaborazione dati Tabella 2.2 avviata..",sep=""))
  } else if (language=="english") {
   cat(paste("Processing data Table 2.2 started..",sep=""))
  }
 }

 table2_2[,"n"]    <-"0"
 table2_2[,"perc"] <-"-"

 if (reference!="") {
  table2_2[,"ref_perc"]<-"-"
  table2_2[,"delta"]   <-"-"
  table2_2[,"hist"]    <-""
  table2_2[,"sig"]     <-0
 }

 ###############################################################

 if (totale>0) {
  input_bypat<-aggregate(x=input_data$COUNT,by=input_data[,list_patologie],FUN="sum")
  names(input_bypat)<-c(list_patologie,"COUNT")
  input_bypat<-make_numeric(input_bypat,c("COUNT"))

  input_bypat$totalpat<-0
  for (j in 1:length(list_patologie)) {
   input_bypat$totalpat<-input_bypat$totalpat+input_bypat[,list_patologie[j]]
  }
 }

 if (reference!="") {

  if (ref_totale>0) {
   ref_bypat<-aggregate(x=ref_data$COUNT,by=ref_data[,list_patologie],FUN="sum")
   names(ref_bypat)<-c(list_patologie,"COUNT")
   ref_bypat<-make_numeric(ref_bypat,c("COUNT"))

   ref_bypat$totalpat<-0
   for (j in 1:length(list_patologie)) {
    ref_bypat$totalpat<-ref_bypat$totalpat+ref_bypat[,list_patologie[j]]
   }

  }

 } # end reference

 for (m in 0:length(list_patologie)) {

  table2_2[m+1,"desc"]     <-as.character(m)

  if (totale>0) {
   table2_2[m+1,"n"]        <-format(sum(input_bypat[input_bypat$totalpat==m,"COUNT"]),big.mark=bigmark)
   table2_2[m+1,"perc"]     <-format(round((sum(input_bypat[input_bypat$totalpat==m,"COUNT"])/totale)*100,2),decimal.mark=decimalmark,nsmall=2)
  }

  if (reference!="") {
   if (ref_totale>0) {
    table2_2[m+1,"ref_n"]   <-format(sum(ref_bypat[ref_bypat$totalpat==m,"COUNT"]),big.mark=bigmark)
    table2_2[m+1,"ref_perc"]<-format(round((sum(ref_bypat[ref_bypat$totalpat==m,"COUNT"])/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
    if (totale>0) {
     table2_2[m+1,"delta"]   <-format(round((sum(input_bypat[input_bypat$totalpat==m,"COUNT"])/totale)*100,2)-round((sum(ref_bypat[ref_bypat$totalpat==m,"COUNT"])/sum(ref_data[,"COUNT"]))*100,2),decimal.mark=decimalmark,nsmall=2)
     minidot(file=paste("d_2_2_t",m+1,sep=""),mean=c(as.numeric(gsub(decimalmark,".",table2_2[m+1,"perc"],fixed=TRUE)),as.numeric(gsub(decimalmark,".",table2_2[m+1,"ref_perc"],fixed=TRUE))),n=c(as.numeric(gsub(bigmark,"",table2_2[m+1,"n"],fixed=TRUE)),as.numeric(gsub(bigmark,"",table2_2[m+1,"ref_n"],fixed=TRUE))))
     table2_2[m+1,"sig"]<-sig
     table2_2[m+1,"hist"]<-paste("<graphic fileref='d_2_2_t",m+1,".pdf'></graphic>",sep="")
    }
   }
  }
 }

 #######################

 if (language=="italian") {
  table2_2[length(list_patologie)+2,"desc"]<-"TOTALE"
 } else if (language=="english") {
  table2_2[length(list_patologie)+2,"desc"]<-"TOTAL"
 }

 table2_2[length(list_patologie)+2,"n"]   <-format(totale,big.mark=bigmark)
 table2_2[length(list_patologie)+2,"perc"]<-format(round(100.00,2),decimal.mark=decimalmark,nsmall=2)

 if (reference!="") {
  table2_2[length(list_patologie)+2,"ref_n"]   <-format(ref_totale,big.mark=bigmark)
  table2_2[length(list_patologie)+2,"ref_perc"]<-format(round(100.00,2),decimal.mark=decimalmark,nsmall=2)
 }

 if (reference!="") {
  table2_2[,"deltarrow"]<-""
  table2_2[,"deltarrow"]<-ifelse(table2_2$sig==2,arrow_up,table2_2[,"deltarrow"])
  table2_2[,"deltarrow"]<-ifelse(table2_2$sig==1,arrow_down,table2_2[,"deltarrow"])
 }

 if (xml==1) {

  if (language=="italian") {
   title<-"Tabella 2.2 - N° patologie per assistito"
   if (reference=="") {
    labs<-c("N°","Numero assistiti","%")
   } else {
    headlabs<-c("N°","Numero assistiti","%","% [P.R.]","Delta %","[0-100%]","")
    footlabs<-c("N°","Numero assistiti","%","%<?custom-linebreak?>[P.R.]","Delta %","[0-100%]","")
    footnote<-"[P.R.]=Popolazione Riferimento"
   }
  } else if (language=="english") {
   title<-"Table 2.2 - N° diseases by subject"
   if (reference=="") {
    labs<-c("N°","No. subjects","%")
   } else {
    headlabs<-c("N°","No. subjects","%","% [R.P.]","Delta %","[0-100%]","")
    footlabs<-c("N°","No. subjects","%","%<?custom-linebreak?>[R.P.]","Delta %","[0-100%]","")
    footnote<-"[R.P.]=Reference Population"
   }
  }

  if (reference=="") {
   writeTable(file="report.xml",
    data=table2_2,
    vars=c("desc","n","perc"),
    headlabs=labs,
    headwidt=c("60pt","120pt","80pt"),
    colalign=c("center","right","right"),
    headalign=c("center","center","center"),
    footlabs=labs,
    title=title,
    graph=NULL)
  } else {
  writeTable(file="report.xml",
    data=table2_2,
    vars=c("desc","n","perc","ref_perc","delta","hist","deltarrow"),
    headlabs=headlabs,
    headwidt=c("60pt","120pt","80pt","80pt","80pt","90pt","15pt"),
    colalign=c("center","right","right","right","right","center","center"),
    headalign=c("center","center","center","center","center","center","center"),
    footlabs=footlabs,
    footnote=footnote,
    title=title,
    graph=NULL)
  }

  fileConn<-file("report.xml",open="at")
  writeLines('<?custom-pagebreak?>',fileConn)
  close(fileConn)

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("completata\r",sep=""))
   } else if (language=="english") {
    cat(paste("completed\r",sep=""))
   }
  }

 }

 ##########################################
 # Tabella 2.3
 ##########################################

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Elaborazione dati Tabella 2.3 avviata..",sep=""))
  } else if (language=="english") {
   cat(paste("Processing data Table 2.3 started..",sep=""))
  }
 }

 if (tot_pat>0) {

  table2_3<-input_bypat[order(-input_bypat$COUNT),]
  table2_3$perc  <-format(round((table2_3$COUNT/sum(table2_3$COUNT))*100, 2),decimal.mark=decimalmark,nsmall=2)

  table2_3<-table2_3[rowSums(table2_3[,list_patologie])>0,] # keep only diseases
  table2_3$rank   <-row(as.matrix(table2_3$COUNT))

  table2_3<-make_numeric(table2_3,c("COUNT"))

 }

 if (reference!="") {

  if (ref_tot_pat>0) {
   ref_table          <-ref_bypat[order(-ref_bypat$COUNT),]
   ref_table$ref_perc <-round((ref_table$COUNT/sum(ref_table$COUNT))*100, 2)
   ref_table          <-ref_table[rowSums(ref_table[,list_patologie])>0,]
   ref_table$ref_rank <-row(as.matrix(ref_table$COUNT))
  }

 }

 if (tot_pat>0) {

  if (reference!="") {
   if (ref_tot_pat>0) {
    table2_3<-merge(table2_3,ref_table[,c(list_patologie,"ref_perc","ref_rank")],by=c(list_patologie),all.x=TRUE,all.y=FALSE)
    # fill empty combinations for small reference populations
    table2_3$ref_perc<-ifelse(is.na(table2_3$ref_perc),"-",table2_3$ref_perc)
    table2_3$ref_rank<-ifelse(is.na(table2_3$ref_rank),"-",table2_3$ref_rank)
   } else {
    table2_3$ref_perc<-"-"
    table2_3$ref_rank<-"-"
   }
  }

  table2_3<-table2_3[order(table2_3$rank),]

  # Create combination of diseases as a label

  table2_3$combination<-""
  for (j in 1:length(list_patologie)) {
   table2_3$combination<-ifelse(table2_3[list_patologie[j]]==1,paste(table2_3$combination," + ",list_patologie_names[j],sep=""),table2_3$combination)
  }
  table2_3$combination<-substr(table2_3$combination,4,nchar(table2_3$combination))

  # Create combination of diseases as a bitcode

  namebits<-c()
  for (j in 1:length(list_patologie)) {
   if (j>1) {
    bits<-cbind(bits,as.character(table2_3[,list_patologie[j]]))
   } else {
    bits<-as.character(table2_3[,list_patologie[j]])
   }
  }
  bits<-as.data.frame(bits)
  names(bits)<-list_patologie

  # Conversion of the bit map of disease combinations to integer
  for (j in 1:nrow(bits)) {
   bits[j,c("bits")]<-BitsToInt(bits[j,list_patologie])
  }

  table2_3$idcomb<-bits[,c("bits")]

  rm(bits)

 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("completata\r",sep=""))
  } else if (language=="english") {
   cat(paste("completed\r",sep=""))
  }
 }

 ############### Venn N

 if (graphs==1) {

  area<- data.frame(matrix(ncol=1,nrow=length(list_patologie)))
  names(area)<-c("area")

  for (j in 1:length(list_patologie)) {
   area$area[j]=sum(table2_3$COUNT[table2_3[,list_patologie[j]]==1])
  }

  if (reference!="" & ref_tot_pat>0) {
   ref_area<- data.frame(matrix(ncol=1,nrow=length(list_patologie)))
   names(ref_area)<-c("area")
   for (j in 1:length(list_patologie)) {
    ref_area$area[j]=sum(ref_table$COUNT[ref_table[,list_patologie[j]]==1])
   }
   main_fontsize<-8
  } else {
   main_fontsize<-10
  }

  n12 =sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1,"COUNT"])

  if (length(list_patologie)>2) {

  n13 =sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[3]]==1,"COUNT"])
  n34 =sum(table2_3[ table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]]==1,"COUNT"])
  n23 =sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]]==1,"COUNT"])
  n123=sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]] ==1,"COUNT"])

  }

  if (length(list_patologie)>3) {

   n14  =sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[4]]==1,"COUNT"])
   n24  =sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[4]]==1,"COUNT"])
   n34  =sum(table2_3[ table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]]==1,"COUNT"])
   n124 =sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[4]] ==1,"COUNT"])
   n134 =sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]] ==1,"COUNT"])
   n234 =sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]] ==1,"COUNT"])
   n1234=sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]] ==1 & table2_3[list_patologie[4]] ==1,"COUNT"])

  }

  if (length(list_patologie)>4) {

   n15  = sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[5]]==1,"COUNT"])
   n25  = sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[5]]==1,"COUNT"])
   n35  = sum(table2_3[ table2_3[list_patologie[3]]==1 & table2_3[list_patologie[5]]==1,"COUNT"])
   n45  = sum(table2_3[ table2_3[list_patologie[4]]==1 & table2_3[list_patologie[5]]==1,"COUNT"])

   n125 = sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n135 = sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n145 = sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[4]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n235 = sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n245 = sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[4]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n345 = sum(table2_3[ table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]]==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n1235= sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]] ==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n1245= sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[4]] ==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n1345= sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]] ==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n2345= sum(table2_3[ table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]] ==1 & table2_3[list_patologie[5]] ==1,"COUNT"])
   n12345=sum(table2_3[ table2_3[list_patologie[1]]==1 & table2_3[list_patologie[2]]==1 & table2_3[list_patologie[3]]==1 & table2_3[list_patologie[4]] ==1 & table2_3[list_patologie[5]] ==1,"COUNT"])

  }

  if (length(list_patologie)==2) {

   venn<-draw.pairwise.venn(
    area1=area$area[1],area2=area$area[2],cross.area=n12,
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",2),
    lwd = rep(2,5),
    fill = c("dodgerblue", "goldenrod1"),
    cat.col = c("dodgerblue", "goldenrod1"),
    alpha = rep(0.5,3),
    label.col = rep("black",2),
    cex = c(rep(0.7,3)),
    fontfamily = rep("serif",3),
    fontface = rep("plain",3),
    cat.cex = 0.7,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

  } else if (length(list_patologie)==3) {

   venn<-draw.triple.venn(
    area1=area$area[1],area2=area$area[2],area3=area$area[3],n12=n12,n13=n13,n23=n23,n123=n123,
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",5),
    lwd = rep(2,5),
    fill = c("dodgerblue", "goldenrod1", "darkorange1"),
    cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
    alpha = rep(0.5,3),
    label.col = rep("black",7),
    cex = c(rep(0.7,7)),
    fontfamily = rep("serif",7),
    fontface = rep("plain",7),
    cat.cex = 0.7,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

  } else if (length(list_patologie)==4) {

   venn<-draw.quad.venn(
    area1=area$area[1],area2=area$area[2],area3=area$area[3],area4=area$area[4],
    n12=n12,n13=n13,n14=n14,n23=n23,n24=n24,n34=n34,n123=n123,n124=n124,n134=n134,n234=n234,n1234=n1234,
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",15),
    lwd = rep(2,15),
    fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
    cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
    alpha = rep(0.3,5),
    label.col = rep("black",15),
    cex = c(rep(0.7,15)),
    fontfamily = rep("serif",15),
    fontface = rep("plain",15),
    cat.cex = 0.7,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

  } else if (length(list_patologie)==5) {

   venn<-draw.quintuple.venn(
     area1=area$area[1],area2=area$area[2],area3=area$area[3],area4=area$area[4],area5=area$area[5],
     n12=n12,n13=n13,n14=n14,n15=n15,n23=n23,n24=n24,n25=n25,n34=n34,n35=n35,n45=n45,
     n123=n123,n124=n124,n125=n125,n134=n134,n135=n135,n145=n145,n234=n234,n235=n235,n245=n245,n345=n345,
     n1234=n1234,n1235=n1235,n1245=n1245,n1345=n1345,n2345=n2345,n12345=n12345,
     category=list_patologie_names,
     col = "black",
     lty = rep("dotted",5),
     lwd = rep(2,5),
     fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
     cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
     alpha = rep(0.3,5),
     label.col = rep("black", 31),
     cex = rep(0.7, 31),
     fontfamily = rep("serif",31),
     fontface = rep("plain",31),
     cat.cex = 0.7,
     cat.pos = c(0,-45,180,180,40),
     cat.fontfamily = "serif",
     ind=FALSE)
  }

  if (language=="italian") {
   textgrob<-"Combinazioni Patologie - Numero soggetti (Popolazione Generale N="
  } else if (language=="english") {
   textgrob<-"Disease Combinations  - No. subjects (Total Population N="
  }

  if (reference!="" & ref_tot_pat>0) {
   pdf(file = "2_3_venndiagram.pdf",width=4.5,height=4.5)
  } else {
   pdf(file = "2_3_venndiagram.pdf",width=5,height=5)
  }

  grid.arrange(textGrob(paste(textgrob,format(totale,big.mark=bigmark),")",sep=""),gp=gpar(fontsize=main_fontsize,font=1)),grobTree(venn),nrow=2,heights=c(1/8,7/8))
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Grafico Venn Patologie salvato in file: [",workDir,"/2_3_venndiagram.pdf] \r",sep=""))
   } else if (language=="english") {
    cat(paste("Venn Diagram Diseases saved in file: [",workDir,"/2_3_venndiagram.pdf] \r",sep=""))
   }
  }
  dev.off()

  png(file = "2_3_venndiagram.png",width=1024,height=1024)
  grid.arrange(textGrob(paste(textgrob,format(totale,big.mark=bigmark),")",sep=""),gp=gpar(fontsize=main_fontsize+4,font=1)),grobTree(venn),nrow=2,heights=c(1/8,7/8))
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Grafico Venn Patologie salvato in file: [",workDir,"/2_3_venndiagram.png] \r",sep=""))
   } else if (language=="english") {
    cat(paste("Venn Diagram Diseases saved in file: [",workDir,"/2_3_venndiagram.png] \r",sep=""))
   }
  }
  dev.off()

  ############## Venn Percentage

  if (reference!="" & ref_tot_pat>0) {cex_venn<-0.7} else {cex_venn<-0.7}

  if (length(list_patologie)==2) {

   areas<-c(area$area[1],area$area[2],n12)

   for (m in 1:3) {
    areas[m]=round((areas[m]/tot_pat)*100)
   }

   venn_percent<-draw.pairwise.venn(
    area1=areas[1],area2=areas[2],cross.area=areas[3],
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",2),
    lwd = rep(2,5),
    fill = c("dodgerblue", "goldenrod1"),
    cat.col = c("dodgerblue", "goldenrod1"),
    alpha = rep(0.5,3),
    label.col = rep("black",2),
    cex = c(rep(cex_venn,3)),
    fontfamily = rep("serif",3),
    fontface = rep("plain",3),
    cat.cex=cex_venn,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

  } else if (length(list_patologie)==3) {

   areas<-c(area$area[1],area$area[2],area$area[3],n12,n13,n23,n123)

   for (m in 1:7) {
    areas[m]=round((areas[m]/tot_pat)*100)
   }

   venn_percent<-draw.triple.venn(
    area1=areas[1],area2=areas[2],area3=areas[3],
    n12=areas[4],n13=areas[5],n23=areas[6],
    n123=areas[7],
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",5),
    lwd = rep(2,5),
    fill = c("dodgerblue", "goldenrod1", "darkorange1"),
    cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
    alpha = rep(0.5,3),
    label.col = rep("black",7),
    cex = c(rep(cex_venn,7)),
    fontfamily = rep("serif",7),
    fontface = rep("plain",7),
    cat.cex=cex_venn,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

  } else if (length(list_patologie)==4) {

   areas<-c(area$area[1],area$area[2],area$area[3],area$area[4],
            n12,n13,n14,n23,n24,n34,n123,n124,n134,n234,n1234)

   for (m in 1:15) {
    areas[m]=round((areas[m]/tot_pat)*100)
   }

   venn_percent<-draw.quad.venn(
    area1=areas[1],area2=areas[2],area3=areas[3],area4=areas[4],
    n12=areas[5],n13=areas[6],n14=areas[7],n23=areas[8],n24=areas[9],n34=areas[10],
    n123=areas[11],n124=areas[12],n134=areas[13],n234=areas[14],
    n1234=areas[15],
    category=list_patologie_names,
    col = "black",
    lty = rep("dotted",15),
    lwd = rep(2,15),
    fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
    cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
    alpha = rep(0.3,5),
    label.col = rep("black",15),
    cex = c(rep(cex_venn,15)),
    fontfamily = rep("serif",15),
    fontface = rep("plain",15),
    cat.cex = cex_venn,
    cat.fontfamily = "serif",
    ind=FALSE,
    main=title)

   } else if (length(list_patologie)==5) {

    areas<-c(area$area[1],area$area[2],area$area[3],area$area[4],area$area[5],
             n12,n13,n14,n15,n23,n24,n25,n34,n35,n45,
             n123,n124,n125,n134,n135,n145,n234,n235,n245,n345,
             n1234,n1235,n1245,n1345,n2345,n12345)

    for (m in 1:31) {
     areas[m]=round((areas[m]/tot_pat)*100)
    }

    venn_percent<-draw.quintuple.venn(
     area1=areas[1],area2=areas[2],area3=areas[3],area4=areas[4],area5=areas[5],
     n12=areas[6],n13=areas[7],n14=areas[8],n15=areas[9],n23=areas[10],n24=areas[11],n25=areas[12],n34=areas[13],n35=areas[14],n45=areas[15],
     n123=areas[16],n124=areas[17],n125=areas[18],n134=areas[19],n135=areas[20],n145=areas[21],n234=areas[22],n235=areas[23],n245=areas[24],n345=areas[25],
     n1234=areas[26],n1235=areas[27],n1245=areas[28],n1345=areas[29],n2345=areas[30],
     n12345=areas[31],
     category=list_patologie_names,
     col = "black",
     lty = rep("dotted",5),
     lwd = rep(2,5),
     fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
     cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
     alpha = rep(0.3,5),
     label.col = rep("black", 31),
     cex = c(rep(cex_venn,31)),
     fontfamily = rep("serif",31),
     fontface = rep("plain",31),
     cat.cex = cex_venn,
     cat.pos = c(0,-45,180,180,40),
     cat.fontfamily = "serif",
     ind=FALSE)
   }

  if (reference!="") {

   if (ref_tot_pat>0) {

    n12_ref =sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]] ==1,"COUNT"])

    if (length(list_patologie)>2) {

    n13_ref =sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[3]] ==1,"COUNT"])
    n34_ref =sum(ref_table[ ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
    n23_ref =sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]] ==1,"COUNT"])
    n123_ref=sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]] ==1 & ref_table[list_patologie[3]] ==1,"COUNT"])

    }

    if (length(list_patologie)>3) {

     n14_ref  =sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n24_ref  =sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n34_ref  =sum(ref_table[ ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n124_ref =sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]] ==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n134_ref =sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n234_ref =sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[4]] ==1,"COUNT"])
     n1234_ref=sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[4]] ==1,"COUNT"])

    }

    if (length(list_patologie)>4) {

     n15_ref  = sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n25_ref  = sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n35_ref  = sum(ref_table[ ref_table[list_patologie[3]]==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n45_ref  = sum(ref_table[ ref_table[list_patologie[4]]==1 & ref_table[list_patologie[5]] ==1,"COUNT"])

     n125_ref = sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n135_ref = sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n145_ref = sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n235_ref = sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n245_ref = sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n345_ref = sum(ref_table[ ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n1235_ref= sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n1245_ref= sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n1345_ref= sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n2345_ref= sum(ref_table[ ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])
     n12345_ref=sum(ref_table[ ref_table[list_patologie[1]]==1 & ref_table[list_patologie[2]]==1 & ref_table[list_patologie[3]]==1 & ref_table[list_patologie[4]] ==1 & ref_table[list_patologie[5]] ==1,"COUNT"])

    }

    if (length(list_patologie)==2) {

     areas<-c(ref_area$area[1],ref_area$area[2],n12_ref)

     for (m in 1:3) {
      areas[m]=round((areas[m]/tot_pat)*100)
     }

     venn_ref_percent<-draw.pairwise.venn(
      area1=areas[1],area2=areas[2],cross.area=areas[3],
      category=list_patologie_names,
      col = "black",
      lty = rep("dotted",2),
      lwd = rep(2,5),
      fill = c("dodgerblue", "goldenrod1"),
      cat.col = c("dodgerblue", "goldenrod1"),
      alpha = rep(0.5,3),
      label.col = rep("black",2),
      cex = c(rep(cex_venn,3)),
      fontfamily = rep("serif",3),
      fontface = rep("plain",3),
      cat.cex = cex_venn,
      cat.fontfamily = "serif",
      ind=FALSE,
      main=title)

    } else if (length(list_patologie)==3) {

     areas<-c(ref_area$area[1],ref_area$area[2],ref_area$area[3],n12_ref,n13_ref,n23_ref,n123_ref)

     for (m in 1:7) {
      areas[m]=round((areas[m]/tot_pat)*100)
     }

     venn_ref_percent<-draw.triple.venn(
      area1=areas[1],area2=areas[2],area3=areas[3],
      n12=areas[4],n13=areas[5],n23=areas[6],
      n123=areas[7],
      category=list_patologie_names,
      col = "black",
      lty = rep("dotted",5),
      lwd = rep(2,5),
      fill = c("dodgerblue", "goldenrod1", "darkorange1"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
      alpha = rep(0.5,3),
      label.col = rep("black",7),
      cex = c(rep(cex_venn,7)),
      fontfamily = rep("serif",7),
      fontface = rep("plain",7),
      cat.cex = 1.2,
      cat.fontfamily = "serif",
      ind=FALSE,
      main=title)

    } else if (length(list_patologie)==4) {

     areas<-c(ref_area$area[1],ref_area$area[2],ref_area$area[3],ref_area$area[4],
              n12_ref,n13_ref,n14_ref,n23_ref,n24_ref,n34_ref,n123_ref,n124_ref,n134_ref,n234_ref,n1234_ref)

     for (m in 1:15) {
      areas[m]=round((areas[m]/tot_pat)*100)
     }

     venn_ref_percent<-draw.quad.venn(
      area1=areas[1],area2=areas[2],area3=areas[3],area4=areas[4],
      n12=areas[5],n13=areas[6],n14=areas[7],n23=areas[8],n24=areas[9],n34=areas[10],
      n123=areas[11],n124=areas[12],n134=areas[13],n234=areas[14],n1234=areas[15],
      category=list_patologie_names,
      col = "black",
      lty = rep("dotted",15),
      lwd = rep(2,15),
      fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
      alpha = rep(0.3,5),
      label.col = rep("black",15),
      cex = c(rep(cex_venn,15)),
      fontfamily = rep("serif",15),
      fontface = rep("plain",15),
      cat.cex = cex_venn,
      cat.fontfamily = "serif",
      ind=FALSE,
      main=title)

     } else if (length(list_patologie)==5) {

      areas<-c(ref_area$area[1],ref_area$area[2],ref_area$area[3],ref_area$area[4],ref_area$area[5],
               n12_ref,n13_ref,n14_ref,n15_ref,n23_ref,n24_ref,n25_ref,n34_ref,n35_ref,n45_ref,
               n123_ref,n124_ref,n125_ref,n134_ref,n135_ref,n145_ref,n234_ref,n235_ref,n245_ref,n345_ref,
               n1234_ref,n1235_ref,n1245_ref,n1345_ref,n2345_ref,n12345_ref)

      for (m in 1:31) {
       areas[m]=round((areas[m]/ref_tot_pat)*100)
      }

      venn_ref_percent<-draw.quintuple.venn(
       area1=areas[1],area2=areas[2],area3=areas[3],area4=areas[4],area5=areas[5],
       n12=areas[6],n13=areas[7],n14=areas[8],n15=areas[9],n23=areas[10],n24=areas[11],n25=areas[12],n34=areas[13],n35=areas[14],n45=areas[15],
       n123=areas[16],n124=areas[17],n125=areas[18],n134=areas[19],n135=areas[20],n145=areas[21],n234=areas[22],n235=areas[23],n245=areas[24],n345=areas[25],
       n1234=areas[26],n1235=areas[27],n1245=areas[28],n1345=areas[29],n2345=areas[30],n12345=areas[31],
       category=list_patologie_names,
       col = "black",
       lty = rep("dotted",5),
       lwd = rep(2,5),
       fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
       cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
       alpha = rep(0.3,5),
       label.col = rep("black", 31),
       cex = c(rep(cex_venn,31)),
       fontfamily = rep("serif",31),
       fontface = rep("plain",31),
       cat.cex = cex_venn,
       cat.pos = c(0,-45,180,180,40),
       cat.fontfamily = "serif",
       ind=FALSE)

    }

   } # end ref_tot_pat

  } # end reference

  if (language=="italian") {
   main_textgrob<-"Combinazioni Patologie - Percentuali su Totale Patologici"
   if (reference!="") {
    textgrob_1<-paste("Popolazione Selezionata (N=",format(tot_pat,big.mark=bigmark),")",sep="")
    textgrob_2<-"Popolazione Riferimento"
   }
  } else if (language=="english") {
   main_textgrob<-"Disease Combinations - Percentage of Total w Disease"
   if (reference!="") {
    textgrob_1<-paste("Selected Population (N=",format(tot_pat,big.mark=bigmark),")",sep="")
    textgrob_2<-"Reference Population"
   }
  }

  if (reference!="" & ref_tot_pat>0) {
   pdf(file = "2_3_venndiagram_percent.pdf",width=9,height=5)
  } else {
   pdf(file = "2_3_venndiagram_percent.pdf",width=4.5,height=4.2)
  }

  if (reference=="") {
   grid.arrange(textGrob(main_textgrob,,gp=gpar(fontsize=main_fontsize,font=1)),grobTree(venn_percent),nrow=2,heights=c(1/8,7/8))
  } else {
   if (ref_tot_pat>0) {
    grid.arrange(main=textGrob(main_textgrob,gp=gpar(fontsize=14,font=1)),
                 textGrob(textgrob_1,gp=gpar(fontsize=12,font=1)),textGrob(textgrob_2,gp=gpar(fontsize=12,font=1)),
                 grobTree(venn_percent),grobTree(venn_ref_percent),nrow=2,heights=c(1/8,7/8))
   } else {
    grid.arrange(textGrob(main_textgrob,,gp=gpar(fontsize=main_fontsize,font=1)),grobTree(venn_percent),nrow=2,heights=c(1/8,7/8))
   }
  }

  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Grafico Venn Percentuali Patologie salvato in file: [",workDir,"/2_3_venndiagram_percent.pdf]\r",sep=""))
   } else if (language=="english") {
    cat(paste("Venn Diagram Diseases Percentage saved in file: [",workDir,"/2_3_venndiagram_percent.pdf]\r",sep=""))
   }
  }

  dev.off()

  png(file = "2_3_venndiagram_percent.png",width=1024,height=1024)
  if (reference=="") {
   grid.arrange(textGrob(main_textgrob,,gp=gpar(fontsize=main_fontsize+4,font=1)),grobTree(venn_percent),nrow=2,heights=c(1/8,7/8))
  } else {
   if (ref_tot_pat>0) {
    grid.arrange(main=textGrob(main_textgrob,gp=gpar(fontsize=14,font=1)),textGrob(textgrob_1,gp=gpar(fontsize=12,font=1)),textGrob(textgrob_2,gp=gpar(fontsize=12,font=1)),grobTree(venn_percent),grobTree(venn_ref_percent),nrow=2,heights=c(1/8,7/8))
   } else {
    grid.arrange(textGrob(main_textgrob,,gp=gpar(fontsize=main_fontsize+4,font=1)),grobTree(venn_percent),nrow=2,heights=c(1/8,7/8))
   }
  }
  if (verbose>0) {
   if (language=="italian") {
    cat(paste("Grafico Venn Percentuali Patologie salvato in file: [",workDir,"/2_3_venndiagram_percent.png]\r",sep=""))
   } else if (language=="english") {
    cat(paste("Venn Diagram Diseases Percentage saved in file: [",workDir,"/2_3_venndiagram_percent.png]\r",sep=""))
   }
  }
  dev.off()

 } # end graphs

 ################### Print Tables

 # Total line

 #########################
 # Initialize

 tot2_3<- data.frame(matrix(ncol=1,nrow=1))
 names(tot2_3)<-c("perc")
 tot2_3$COUNT<-"0"
 tot2_3$perc<-"-"
 tot2_3$rank<-""
 if (reference!="") {
  tot2_3$ref_rank<-""
  tot2_3$ref_perc<-"-"
 }

 #########################

 tot2_3$combination<-total_label
 tot2_3$idcomb<-0
 if (tot_pat>0) {
  tot2_3$COUNT      <-format(sum(as.numeric(gsub(bigmark,"",table2_3$COUNT,fixed=TRUE))),big.mark=bigmark)
  tot2_3$perc       <-format(round((sum(as.numeric(gsub(bigmark,"",table2_3$COUNT,fixed=TRUE)))/totale)*100, 2),decimal.mark=decimalmark,nsmall=2)
 }

 if (reference!="") {
  if (ref_tot_pat>0) {
   tot2_3$ref_perc  <-format(round((sum(ref_table$COUNT)/ref_totale)*100,2),decimal.mark=decimalmark,nsmall=2)
  }
 }

 if (tot_pat>0) {
  table2_3<-data.frame(rbindlist(list(table2_3,tot2_3),fill=TRUE))
 } else {
  table2_3<-tot2_3
 }

 rm(tot2_3)

 ############################

 if (reference!="") {
  table2_3[,"delta"]<-""
  if (ref_tot_pat>0) {
   table2_3[,"delta"]<-ifelse(table2_3$rank<table2_3$ref_rank & table2_3$ref_rank!="-",arrow_up,table2_3[,"delta"])
   table2_3[,"delta"]<-ifelse(table2_3$rank>table2_3$ref_rank & table2_3$ref_rank!="-",arrow_down,table2_3[,"delta"])
  }
 }

 if (tot_pat>0) {
  table2_3$COUNT    <-format(table2_3$COUNT,big.mark=bigmark)
  if (reference!="") {
   table2_3$ref_perc<-format(table2_3$ref_perc,decimal.mark=decimalmark,nsmall=2)
  }
 }

 if (xml==1) {

  if (language=="italian") {
   title<-"Tabella 2.3 - Combinazioni patologie"
   if (reference=="") {
    labs=c("Rango","Combinazione","N","%")
   } else {
    headlabs=c("Rango","Combinazione","N","%","Rango [P.R.]","Rango [P.R.]","")
    footlabs=c("Rango","Combinazione","N","%","%<?custom-linebreak?>[P.R.]","Rango<?custom-linebreak?>[P.R.]","")
    footnote<-"[P.R.]=Popolazione Riferimento"
   }
  } else if (language=="english") {
   title<-"Table 2.3 - Disease combinations"
   if (reference=="") {
    labs=c("Rank","Combination","N","%")
   } else {
    headlabs=c("Rank","Combination","N","%","% [R.P.]","Rank [R.P.]","")
    footlabs=c("Rank","Combination","N","%","%<?custom-linebreak?>[R.P.]","Rank<?custom-linebreak?>[R.P.]","")
    footnote<-"[R.P.]=Reference Population"
   }
  }

  if (reference=="") {

   writeTable(file="report.xml",
    data=table2_3[as.numeric(gsub(bigmark,"",table2_3$COUNT,fixed=TRUE))>0 | table2_3$idcomb==0,],
    vars=c("rank","combination","COUNT","perc"),
    headlabs=labs,
    headwidt=c("30pt","440pt","50pt","50pt"),
    colalign=c("center","left","right","right"),
    headalign=c("center","left","center","center"),
    footlabs=labs,
    title=title,
    graph=NULL)

  } else {

   writeTable(file="report.xml",
    data=table2_3[as.numeric(gsub(bigmark,"",table2_3$COUNT,fixed=TRUE))>0  | table2_3$idcomb==0,],
    vars=c("rank","combination","COUNT","perc","ref_perc","ref_rank","delta"),
    headlabs=headlabs,
    headwidt=c("30pt","440pt","50pt","50pt","50pt","40pt","20pt"),
    colalign=c("center","left","right","right","right","center","center"),
    headalign=c("center","left","center","center","center","center","center"),
    footlabs=footlabs,
    footnote=footnote,
    title=title,
    graph=NULL)

  }

  ################### Print Graphs

  if (graphs==1) {

   fileConn<-file("report.xml",open="at")
   writeLines('<?custom-pagebreak?>',fileConn)
   close(fileConn)

   writeTable(file="report.xml",title=title,graph="2_3_venndiagram.pdf")

   if (language=="italian") {
    title<-"Combinazioni patologie in percentuale"
   } else if (language=="english") {
    title<-"Disease combinations in percentage"
   }

   writeTable(file="report.xml",title=title,graph="2_3_venndiagram_percent.pdf")

   rm(venn,venn_percent)

  }

 }

 #####################################################
 # Output oracle
 #####################################################

 table2_1<-table2_1[,c("desc","hosp","exe","drugs","n","perc")]

 table2_1$Anno<-as.integer(year)
 table2_1$CodiceASL<-operator
 table2_1$NumeroRicoveri <-as.integer(gsub(bigmark,"",table2_1$hosp,fixed=TRUE))
 table2_1$NumeroEsenzioni<-as.integer(gsub(bigmark,"",table2_1$exe,fixed=TRUE))
 table2_1$NumeroFarmaci  <-as.integer(gsub(bigmark,"",table2_1$drugs,fixed=TRUE))
 table2_1$NumeroTotale   <-as.integer(gsub(bigmark,"",table2_1$n,fixed=TRUE))
 table2_1$CodiceRiga     <-as.numeric(rownames(table2_1))
 table2_1$Percentuale<-suppressWarnings(ifelse(table2_1$perc!="-",as.numeric(gsub(decimalmark,".",table2_1$perc,fixed=TRUE)),""))

 table2_1<-table2_1[,c("Anno","CodiceASL","CodiceRiga","NumeroRicoveri","NumeroEsenzioni","NumeroFarmaci","NumeroTotale","Percentuale")]

 #####################################################

 table2_2<-table2_2[,c("desc","n","perc")]

 table2_2$Anno           <-as.integer(year)
 table2_2$CodiceASL      <-operator
 table2_2$NumeroAssistiti<-as.integer(gsub(bigmark,"",table2_2$n,fixed=TRUE))
 table2_2$CodiceRiga     <-as.numeric(rownames(table2_2))
 table2_2$Percentuale<-suppressWarnings(ifelse(table2_2$perc!="-",as.numeric(gsub(decimalmark,".",table2_2$perc,fixed=TRUE)),""))

 table2_2<-table2_2[,c("Anno","CodiceASL","CodiceRiga","NumeroAssistiti","Percentuale")]

 ############################

 table2_3<-table2_3[,c("combination","idcomb","COUNT","perc")]

 table2_3$Anno           <-as.integer(year)
 table2_3$CodiceASL      <-operator
 table2_3$desc           <-table2_3$combination
 table2_3$CodiceRiga     <-table2_3$idcomb
 table2_3$NumeroAssistiti<-as.integer(gsub(bigmark,"",table2_3$COUNT,fixed=TRUE))
 table2_3$Percentuale<-suppressWarnings(ifelse(table2_3$perc!="-",as.numeric(gsub(decimalmark,".",table2_3$perc,fixed=TRUE)),""))

 table2_3<-table2_3[,c("Anno","CodiceASL","CodiceRiga","NumeroAssistiti","Percentuale")]

 #########################################################################################
 # Coding conventions
 #########################################################################################

 coding2_1<- data.frame(matrix(ncol=4,nrow=length(list_patologie)+2))
 names(coding2_1)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 for (i in 1:length(list_patologie)) {
  coding2_1[i,"Descrizione"]<-list_patologie_names[i]
 }
 coding2_1[i+1,"Descrizione"]<-"Totale soggetti con almeno una patologia"
 coding2_1[i+2,"Descrizione"]<-"Totale condizioni patologiche riscontrate"

 coding2_1$SezioneTabella<-2
 coding2_1$CodiceTabella<-1
 coding2_1$CodiceRiga<-as.numeric(rownames(coding2_1))

 ######

 coding2_2<- data.frame(matrix(ncol=4,nrow=length(list_patologie)+2))
 names(coding2_2)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")

 for (i in 1:(length(list_patologie)+1)) {
  coding2_2[i,"Descrizione"]<-as.character(i-1)
 }
 coding2_2[i+1,"Descrizione"]<-"Totale"

 coding2_2$SezioneTabella<-2
 coding2_2$CodiceTabella<-2
 coding2_2$CodiceRiga<-as.numeric(rownames(coding2_2))

 ######

 coding2_3<- data.frame(matrix(ncol=4+length(list_patologie),nrow=2**length(list_patologie)-1))
 names(coding2_3)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione",list_patologie)

 for (j in 1:(2**length(list_patologie)-1)) {
  combination<-IntToBits(j)
  if ( length(IntToBits(j)) < length(list_patologie) ) {
   for (k in (length(IntToBits(j))+1):(length(list_patologie))) {
    combination[k]<-0
   }
  }
  if ( length(IntToBits(j)) > length(list_patologie) ) {
   combination<-combination[1:(length(IntToBits(j))-1)]
  }
  for (p in 1:length(list_patologie)) {
   coding2_3[j,list_patologie[p]]<-combination[p]
  }
 }

 coding2_3$Descrizione<-""
 for (j in 1:length(list_patologie)) {
  coding2_3$Descrizione<-ifelse(coding2_3[list_patologie[j]]==1,paste(coding2_3$Descrizione," + ",list_patologie_names[j],sep=""),coding2_3$Descrizione)
 }
 coding2_3$Descrizione<-substr(coding2_3$Descrizione,4,nchar(coding2_3$Descrizione))

 coding2_3$SezioneTabella<-2
 coding2_3$CodiceTabella<-3
 coding2_3$CodiceRiga<-as.numeric(rownames(coding2_3))

 coding2_3<-coding2_3[,c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")]

 coding_totale<-data.frame(matrix(ncol=4,nrow=1))
 names(coding_totale)<-c("SezioneTabella","CodiceTabella","CodiceRiga","Descrizione")
 coding_totale$SezioneTabella<-2
 coding_totale$CodiceTabella<-3
 coding_totale$Descrizione<-"TOTALE PATOLOGIE"

 coding2_3<-rbind(coding2_3,coding_totale)

 ######

 coding2<-rbind(coding2_1,coding2_2,coding2_3)
 rm(coding2_1,coding2_2,coding2_3)

 #########################################################################################

 if (output!="") {

  data_output<-list(table2_1,table2_2,table2_3)
  files_output<-c("table2_1","table2_2","table2_3")
  files_desc<-c("Tabella 2.1","Tabella 2.2","Tabella 2.3")

  for (i in 1:length(data_output)) {
   write.csv(data_output[[i]],paste(output,"/",files_output[i],".csv",sep=""),row.names=FALSE,na="")
   if (verbose==1) {
    if (language=="italian") {
     cat(paste("Tabella risultati ",files_desc[i]," salvata in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    } else if (language=="english") {
     cat(paste("Table Results ",files_desc[i]," saved in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    }
   }
  }

  write.csv(coding2,paste(output,"/CodificheRighe.csv",sep=""),row.names=FALSE,na="")
  if (verbose==1) {
   if (language=="italian") {
    cat(paste("Tabella Codifiche Righe salvata in file: [",workDir,"/CodificheRighe.csv]\r",sep=""))
   } else if (language=="english") {
    cat(paste("Table Coding Rows saved in file: [",workDir,"/CodificheRighe.csv]\r",sep=""))
   }
  }

 }

 #########################################################################################

 if (xml==1) {
  if (engine_type!="") {rm(input_data)}
  rm(table2_1,table2_2,table2_3,coding2)
 } else {

  # Make table available globally
  assign("table2_1",table2_1,envir=.GlobalEnv)
  assign("table2_2",table2_2,envir=.GlobalEnv)
  assign("table2_3",table2_3,envir=.GlobalEnv)
  assign("coding2",coding2,envir=.GlobalEnv)

 }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 2",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 2",sep=""))
  }
  cat("\r")
 }

} # end Indicator 2

read_comb <- function(input_table,patologia,fonti,output,venn,title,verbose=1,graphs=1) {

 # Set the working directory
 setwd(workDir)

 # select only needed columns
 vars_pat<-NULL
 vars_fonti_1<-NULL
 vars_fonti_2<-NULL
 vars_fonti_3<-NULL
 vars_fonti_12<-NULL
 vars_fonti_13<-NULL
 vars_fonti_23<-NULL
 vars_fonti_123<-NULL
 for (i in 1:length(patologia)) {
  vars_pat<-c(vars_pat,patologia[i])
 }
 for (i in 1:length(fonti)) {
  vars_fonti_1<-c(vars_fonti_1,paste(fonti[i],"_1",sep=""))
  vars_fonti_2<-c(vars_fonti_2,paste(fonti[i],"_2",sep=""))
  vars_fonti_3<-c(vars_fonti_3,paste(fonti[i],"_3",sep=""))
  vars_fonti_12<-c(vars_fonti_12,paste(fonti[i],"_12",sep=""))
  vars_fonti_13<-c(vars_fonti_13,paste(fonti[i],"_13",sep=""))
  vars_fonti_23<-c(vars_fonti_23,paste(fonti[i],"_23",sep=""))
  vars_fonti_123<-c(vars_fonti_123,paste(fonti[i],"_123",sep=""))
 }
 input_data<-input_table[,c(vars_pat,vars_fonti_1,vars_fonti_2,vars_fonti_3,vars_fonti_12,vars_fonti_13,vars_fonti_23,vars_fonti_123,"COUNT")]

 if (length(patologia)>1) {
  input_data<-input_data[rowSums(input_data[,patologia])>0,] # at least one disease recorded
 } else {
  input_data<-input_data[as.numeric(input_data[,patologia])>0,] # at least one disease recorded
 }

 output_data <- data.frame(matrix(ncol=7,nrow=1))
 names(output_data)<-c("hosp","exe","drugs","n12","n13","n23","n123")

 output_data$hosp    <-0
 output_data$exe     <-0
 output_data$drugs   <-0
 output_data$hosppct <-0
 output_data$exepct  <-0
 output_data$drugspct<-0
 output_data$n12     <-0
 output_data$n13     <-0
 output_data$n23     <-0
 output_data$n123    <-0

 if (nrow(input_data)>0) {
  output_data$hosp<-sum(input_data[vars_fonti_1])
  output_data$exe<-sum(input_data[vars_fonti_2])
  output_data$drugs<-sum(input_data[vars_fonti_3])

  output_data$hosppct<-round(sum(input_data[vars_fonti_1])/sum(input_data$COUNT)*100)
  output_data$exepct<-round(sum(input_data[vars_fonti_2])/sum(input_data$COUNT)*100)
  output_data$drugspct<-round(sum(input_data[vars_fonti_3])/sum(input_data$COUNT)*100)

  output_data$n12<-round(sum(input_data[vars_fonti_12])/sum(input_data$COUNT)*100)
  output_data$n13<-round(sum(input_data[vars_fonti_13])/sum(input_data$COUNT)*100)
  output_data$n23<-round(sum(input_data[vars_fonti_23])/sum(input_data$COUNT)*100)
  output_data$n123<-round(sum(input_data[vars_fonti_123])/sum(input_data$COUNT)*100)

 }

 if (language=="italian") {
  category=c("Ricoveri","Esenzioni","Farmaci")
 } else if (language=="english") {
  category=c("Discharges","Exemptions","Drugs")
 }

 if (graphs==1) {
   output_venn<-draw.triple.venn(
     area1=output_data$hosppct,
     area2=output_data$exepct,
     area3=output_data$drugspct,
     n12=output_data$n12,
     n13=output_data$n13,
     n23=output_data$n23,
     n123=output_data$n123,
     category=category,
     cat.pos = c(-30,30,180),
     col = "black",
     lty = rep("dotted",3),
     lwd = rep(2,3),
     fill = c("dodgerblue", "goldenrod1",  "seagreen3"),
     cat.col = c("dodgerblue", "goldenrod1",  "seagreen3"),
     alpha = rep(0.5,3),
     label.col = rep("black",7),
     cex = c(1.2,1.2,1.2,1.2,1.2,1.2,1.2),
     fontfamily = rep("serif",7),
     fontface = rep("plain",7),
     cat.cex = 1.2,
     cat.fontfamily = "serif",
     ind=FALSE,
     main=title)
 } else {
   output_venn<-NULL
 }
 assign(output,output_data,envir=.GlobalEnv)
 assign(venn,output_venn,envir=.GlobalEnv)

 rm(input_data,output_data,output_venn)

} # end read_comb

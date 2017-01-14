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

indicator4 <- function(xml=1,graphs=1,output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 4: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 4: started processing\r")
  }
  cat("########################################\r")
 }

 # Set the working directory
 setwd(workDir)

 ###############################################################

 if (engine_type=="local")  {
  input_data<-merge_table(c("db_master","db_aveind_hyperte","db_aveind_diab","db_aveind_ihd","db_aveind_hf","db_aveind_demen","db_pctind_hyperte","db_pctind_diab","db_pctind_ihd","db_pctind_hf","db_pctind_demen","db_cost","db_discharge"))
  input_data<-make_numeric(input_data,c(list_patologie,list_costi_ext,list_costi_ext_2,list_numvars,list_ricoveri,"COUNT"))
 } else if (engine_type=="central") {
  input_data<-createCentralData(input_files=input_files,list_numvars=list_numvars)
 }

 ## Input data for current section

 ##########################################
 # Reference Dataset
 ##########################################

 if (reference!="") {

  arrow_up  <-"<graphic fileref='resources/arrow_up_reversed.png'></graphic>"
  arrow_down<-"<graphic fileref='resources/arrow_down_reversed.png'></graphic>"

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

 ##########################################

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

 if (tot_pat==0) {graphs<-0}

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

 # select only diseases
 if (totale>0) {
  input_data<-input_data[rowSums(input_data[list_patologie])>0,]
 }

 if (reference=="") {
  list_target_data<-list(input_data)
  target_totale<-c(totale)
  target_tot_pat<-c(tot_pat)
 } else {
  list_target_data<-list(input_data,ref_data)
  target_totale<-c(totale,ref_totale)
  target_tot_pat<-c(tot_pat,ref_tot_pat)
 }

 if (language=="italian") {
  total_label<-"TOTALE PATOLOGIE"
  total_cost_label<-"TOTALE"
 } else if (language=="english") {
  total_label<-"TOTAL DISEASES"
  total_cost_label<-"TOTAL"
 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Elaborazione dati Tabella 4.1 avviata..\r",sep=""))
  } else if (language=="english") {
   cat(paste("Processing data Table 4.1 started..\r",sep=""))
  }
 }

 ##########################################
 # Tabella 4.1
 ##########################################

 for (q in 1:length(list_target_data)) {

  target_data<-list_target_data[[q]]

  if (target_tot_pat[q]>0) {

   input_bypat<-aggregate(x=target_data$COUNT,by=target_data[,list_patologie],FUN="sum")
   names(input_bypat)<-c(list_patologie,"COUNT")

   table4_1<-input_bypat[order(-input_bypat$COUNT),]

   table4_1$freq <- format(round((table4_1$COUNT/sum(table4_1$COUNT))*100, 2),decimal.mark=decimalmark,nsmall=2)

   table4_1<-table4_1[rowSums(table4_1[,list_patologie])>0,]
   table4_1$rank <- row(as.matrix(table4_1$COUNT))

   table4_1$combination<-""
   for (j in 1:length(list_patologie)) {
    table4_1$combination<-ifelse(table4_1[list_patologie[j]]=="1",paste(table4_1$combination," + ",list_patologie_names[j],sep=""),table4_1$combination)
   }
   table4_1$combination<-substr(table4_1$combination,4,nchar(table4_1$combination))

   # Score Patologie

   target_data$score_gq<-0
   target_data$max_score_gq<-0

   options(width=160)
   options(max.print=1000)

   score_vars_score_gq<-c()
   score_vars_max_score_gq<-c()
   score_vars<-c()

   for (j in 1:length(list_patologie)) {
    score_vars_score_gq<-c(score_vars_score_gq,paste("score_gq_",list_patologie[j],sep=""))
    score_vars_max_score_gq<-c(score_vars_max_score_gq,paste("max_score_gq_",list_patologie[j],sep=""))
   }
   score_vars<-c(score_vars_score_gq,score_vars_max_score_gq)

   scores<-data.frame(matrix(ncol=3+length(list_patologie)+length(score_vars),nrow=nrow(target_data)))
   names(scores)<-c("SESSO","AGE_RANGE",score_vars,list_patologie,"COUNT")

   scores[,c("SESSO","AGE_RANGE",list_patologie,"COUNT")]<-target_data[,c("SESSO","AGE_RANGE",list_patologie,"COUNT")]

   for (j in 1:length(list_patologie)) {

    list_num_numvars<-c()
    list_den_numvars<-c()
    for (k in 1:length(indicatori_global[[j]])) {
     list_num_numvars<-c(list_num_numvars,paste("NUM_",indicatori_global[[j]][k],"_",list_patologie[j],sep=""))
     list_den_numvars<-c(list_den_numvars,paste("DEN_",indicatori_global[[j]][k],"_",list_patologie[j],sep=""))
    }

    ind_num<- target_data[list_num_numvars]
    ind_den<- target_data[list_den_numvars]

    for (k in 1:length(indicatori_global[[j]])) {
     num_name<-paste("NUM_",indicatori_global[[j]][k],"_",list_patologie[j],sep="")
     den_name<-paste("DEN_",indicatori_global[[j]][k],"_",list_patologie[j],sep="")
    }

    scores[,paste("score_gq_",list_patologie[j],sep="")]<-rowSums(ind_num,na.rm=TRUE)
    scores[,paste("max_score_gq_",list_patologie[j],sep="")]<-rowSums(ind_den,na.rm=TRUE)

    rm(ind_num,ind_den)

   }
   scores$score_gq<-rowSums(scores[score_vars_score_gq])
   scores$max_score_gq<-rowSums(scores[score_vars_max_score_gq])

   # Global Scores
   scores$score_gq<-ifelse(scores$max_score_gq>0,(scores$score_gq/scores$max_score_gq)*100,0)
   scores$score_gq<-scores$score_gq*scores$COUNT

   table_ <- aggregate(x=scores$score_gq,by=scores[list_patologie],FUN="sum")
   names(table_) <- c(list_patologie,"score_gq")

   table4_1<-merge(table4_1,table_,by=list_patologie)

   #####
   table4_1_2<- data.frame(matrix(ncol=1,nrow=length(list_patologie)))
   names(table4_1_2)<-c("score_gq")
   for (j in 1:length(list_patologie)) {
    table4_1_2[j,"rank"]<-j
    table4_1_2[j,"score_gq"]<-round(sum(table4_1$score_gq[table4_1[list_patologie[j]]==1])/sum(table4_1$COUNT[table4_1[list_patologie[j]]==1]),2)
    table4_1_2[j,"combination"]<-list_patologie_names[j]
   }
   #####

   # Overall Average Global Score
   n_grandtotal<-sum(table4_1$COUNT)
   score_grandtotal<-format(round(sum(table4_1$score_gq)/sum(table4_1$COUNT),2),decimal.mark=decimalmark,nsmall=2)

   tot4_1<- data.frame(matrix(ncol=1,nrow=1))
   names(tot4_1)<-c("score_gq")
   tot4_1$score_gq<-score_grandtotal
   tot4_1$combination<-total_label
   tot4_1$rank<-""
   tot4_1$idcomb<-0

   table4_1$score_gq_num<-round(table4_1$score_gq/table4_1$COUNT)
   table4_1$score_gq<-format(round(table4_1$score_gq/table4_1$COUNT,2),decimal.mark=decimalmark,nsmall=2)
   table4_1<-table4_1[order(-table4_1$COUNT),]

  } else { # target tot_pat zero
   tot4_1<- data.frame(matrix(ncol=1,nrow=1))
   names(tot4_1)<-c("score_gq")
   tot4_1$combination<-total_label
   tot4_1$score_gq<-"-"
   tot4_1$rank<-"-"
   tot4_1$idcomb<-0
  }

  # check input data

  ################ Graphical outputs

  if (graphs==1 & target_tot_pat[q]>0) {

   table4_1$label.col<-"aquamarine4"
   table4_1$label.col<-ifelse(table4_1$score_gq_num>=35 & table4_1$score_gq_num<=50,"lightsalmon",table4_1$label.col)
   table4_1$label.col<-ifelse(table4_1$score_gq_num<35,"firebrick1",table4_1$label.col)

   area<-list()
   for (j in 1:length(list_patologie)) {
    area[[j]]=sum(table4_1$score_gq_num[table4_1[list_patologie[j]]==1])
   }

   n12   =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]] == 1])

   if (length(list_patologie)>2) {

    n13   =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[3]] == 1])
    n23   =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[3]] == 1])
    n123  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]] & table4_1[list_patologie[3]] == 1])

   }

   if (length(list_patologie)>3) {

    n14   =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[4]] == 1])
    n24   =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[4]] == 1])
    n34   =sum(table4_1$score_gq_num[ table4_1[list_patologie[3]]== 1 & table4_1[list_patologie[4]] == 1])
    n124  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]] == 1 & table4_1[list_patologie[4]] == 1])
    n134  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]] == 1])
    n234  =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]] == 1])
    n1234 =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]]==1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]] == 1])

   }

   if (length(list_patologie)>4) {

    n125  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]] == 1 & table4_1[list_patologie[5]] == 1])
    n135  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[3]] == 1 & table4_1[list_patologie[5]] == 1])
    n15   =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[5]] == 1])
    n25   =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[5]] == 1])
    n35   =sum(table4_1$score_gq_num[ table4_1[list_patologie[3]]== 1 & table4_1[list_patologie[5]] == 1])
    n45   =sum(table4_1$score_gq_num[ table4_1[list_patologie[4]]== 1 & table4_1[list_patologie[5]] == 1])
    n145  =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n235  =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[5]] == 1])
    n245  =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n345  =sum(table4_1$score_gq_num[ table4_1[list_patologie[3]]== 1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n1235 =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]]==1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[5]] == 1])
    n1245 =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]]==1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n1345 =sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n2345 =sum(table4_1$score_gq_num[ table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])
    n12345=sum(table4_1$score_gq_num[ table4_1[list_patologie[1]]== 1 & table4_1[list_patologie[2]]== 1 & table4_1[list_patologie[3]]==1 & table4_1[list_patologie[4]]==1 & table4_1[list_patologie[5]] == 1])

   }

   ########### label.col

   ################## create and merge with all possible combinations for Venn Diagrams
   ################## THIS IS REQUIRED IF NOT COMBINATIONS EXIST FOR COLORS

   table_ghost<- data.frame(matrix(0,ncol=length(list_patologie),nrow=2^length(list_patologie)))
   names(table_ghost)<-c(list_patologie)

   names <- sprintf("%d", 1:5)
   n <- length(names)
   stopifnot(n <= 32); # Theoretical upper limit
   x <- matrix(intToBits(1:(2^n-1)), ncol=2^n-1)
   x <- x[1:n,,drop=FALSE]
   keys <- apply(x, MARGIN=2, FUN=function(z) paste(names[as.logical(z)]))

   for (j in 1:(nrow(table_ghost)-1)) {
    for (k in 1:length(keys[[j]])) {
     table_ghost[j,as.numeric(keys[[j]][k])]<-1
    }
   }

   table_ghost<-merge(table4_1[c(list_patologie,"label.col")],table_ghost,by=list_patologie,all.y=TRUE)
   table_ghost<-table_ghost[rowSums(table_ghost[list_patologie])>0,]

   # use color transparency to avoid showing zeros
   table_ghost$label.col<-ifelse(is.na(table_ghost$label.col),"#0000ff00",table_ghost$label.col)

   ###########################################################

   if (length(list_patologie)==2) {

    label.col<-
     c(
      table_ghost[table_ghost[list_patologie[1]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 1
      table_ghost[table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 2
      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 12
      )

    venn<-draw.pairwise.venn(
     area1=area[[1]],area2=area[[2]],cross.area=n12,
     category=list_patologie_names,
     col = "black",
     lty = rep("dotted",2),
     lwd = rep(2,5),
     fill = c("dodgerblue", "goldenrod1"),
     cat.col = c("dodgerblue", "goldenrod1"),
     alpha = rep(0.5,3),
     label.col = label.col,
     cex = c(1.2,1.2,1.2),
     fontfamily = rep("serif",3),
     fontface = rep("plain",3),
     cat.cex = 1.2,
     cat.fontfamily = "serif",
     ind=FALSE,
     main=title)

   } else if (length(list_patologie)==3) {

   label.col<-
    c(
     table_ghost[table_ghost[list_patologie[1]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 1
     table_ghost[table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 2
     table_ghost[table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 3

     table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 12
     table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 23
     table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 13

     table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 123
    )

    venn<-draw.triple.venn(
     area1=area[[1]],area2=area[[2]],area3=area[[3]],n12=n12,n13=n13,n23=n23,n123=n123,
     category=list_patologie_names,
     col = "black",
     lty = rep("dotted",5),
     lwd = rep(2,5),
     fill = c("dodgerblue", "goldenrod1", "darkorange1"),
     cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
     alpha = rep(0.5,3),
     label.col = label.col,
     cex = c(1.2,1.2,1.2,1.2,1.2,1.2,1.2),
     fontfamily = rep("serif",7),
     fontface = rep("plain",7),
     cat.cex = 1.2,
     cat.fontfamily = "serif",
     ind=FALSE,
     main=title)

   } else if (length(list_patologie)==4) {

    label.col<-
     c(
      table_ghost[table_ghost[list_patologie[1]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 1
      table_ghost[table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 2
      table_ghost[table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 3
      table_ghost[table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 4

      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 14
      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 12
      table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 23
      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 13
      table_ghost[table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 34
      table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 24

      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 124
      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 123
      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 134
      table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 234

      table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 1234

      )

    venn<-draw.quad.venn(
     area1=area[[1]],area2=area[[2]],area3=area[[3]],area4=area[[4]],
     n12=n12,n13=n13,n14=n14,n23=n23,n24=n24,n34=n34,n123=n123,n124=n124,n134=n134,n234=n234,n1234=n1234,
     category=list_patologie_names,
     col = "black",
     lty = rep("dotted",15),
     lwd = rep(2,15),
     fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
     cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
     alpha = rep(0.3,5),
     label.col = label.col,
     cex = c(rep(1.2,15)),
     fontfamily = rep("serif",15),
     fontface = rep("plain",15),
     cat.cex = 1.2,
     cat.fontfamily = "serif",
     ind=FALSE,
     main=title)

    } else if (length(list_patologie)==5) {

     label.col<-
      c(
       table_ghost[table_ghost[list_patologie[1]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 1
       table_ghost[table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 2
       table_ghost[table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 3
       table_ghost[table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 4
       table_ghost[table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==1,c("label.col")], # 5

       table_ghost[table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 35
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 15
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 14
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 12
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 25
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 23
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 13
       table_ghost[table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 34
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 24
       table_ghost[table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==2,c("label.col")], # 45

       table_ghost[table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 345
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 135
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 145
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 124
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 125
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 235
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 123
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 134
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 234
       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==3,c("label.col")], # 245

       table_ghost[table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 2345
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 1345
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 1245
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 1235
       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & rowSums(table_ghost[list_patologie])==4,c("label.col")], # 1234

       table_ghost[table_ghost[list_patologie[1]]==1 & table_ghost[list_patologie[2]]==1 & table_ghost[list_patologie[3]]==1 & table_ghost[list_patologie[4]]==1 & table_ghost[list_patologie[5]]==1 & rowSums(table_ghost[list_patologie])==5,c("label.col")] # 12345

      )

     venn<-draw.quintuple.venn(
      area1=area[[1]],area2=area[[2]],area3=area[[3]],area4=area[[4]],area5=area[[5]],
      n12=n12,n13=n13,n14=n14,n15=n15,n23=n23,n24=n24,n25=n25,n34=n34,n35=n35,n45=n45,
      n123=n123,n124=n124,n125=n125,n134=n134,n135=n135,n145=n145,n234=n234,n235=n235,n245=n245,n345=n345,
      n1234=n1234,n1235=n1235,n1245=n1245,n1345=n1345,n2345=n2345,n12345=n12345,
      category=list_patologie_names,
      col = "black",
      lty = rep("dotted",5),
      lwd = rep(2,5),
      fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      alpha = rep(0.1,5),
      label.col = label.col,
      cex = 1.4,
      fontfamily = rep("serif",31),
      fontface = rep("bold",31),
      cat.cex = 1.3,
      cat.pos = c(0,-45,180,180,40),
      cat.fontfamily = "serif",
      ind=TRUE)

   }

   # blankPanel<-grid.rect(gp=gpar(col="white"),draw=FALSE)

   if (language=="italian") {
    if (reference=="" | q==1) {
     textgrob1<-paste("Indicatore Composito di Qualita' della Cura della Multimorbidita' (N=",format(n_grandtotal,big.mark=bigmark,decimal.mark=decimalmark),")",sep="")
    } else {
     textgrob1<-paste("Indicatore Composito di Qualita' della Cura della Multimorbidita' (N=",format(n_grandtotal,big.mark=bigmark,decimal.mark=decimalmark),") [P.R.]",sep="")
    }
    textgrob2<-"Percentuali medie processi osservati su totale processi attesi per profilo individuale"
    textgrob3<-paste("(Media Totale=",score_grandtotal,")",sep="")
    legend_title<-"Livelli raggiunti"
   } else {
    if (reference=="" | q==1) {
     textgrob1<-paste("Composite Indicator of Quality of Care of Multimorbidity (N=",format(n_grandtotal,big.mark=bigmark,decimal.mark=decimalmark),")",sep="")
    } else {
     textgrob1<-paste("Composite Indicator of Quality of Care of Multimorbidity (N=",format(n_grandtotal,big.mark=bigmark,decimal.mark=decimalmark),") [R.P.]",sep="")
    }
    textgrob2<-"Mean percentage observed processes on total no. expected by individual profile"
    textgrob3<-paste("(Total Average=",score_grandtotal,")",sep="")
    legend_title<-"Scores achieved"
   }

   legend<-grid_legend(0.8,0.9,
                       pch=c(20,20,20),
                       col=c("firebrick1","lightsalmon","aquamarine4"),
                       label=c("<35","35-50",">50"),
                       title=legend_title,
                       frame=FALSE,
                       gp=gpar(cex=1.3),
                       draw=FALSE)

   pdf(file = paste("4_1_",q,"_venndiagram.pdf",sep=""),height=7.5,width=10)
   grid.arrange(top=textGrob(textgrob1,gp=gpar(fontsize=18,font=1)),
                textGrob(textgrob2,gp=gpar(fontsize=15,font=1)),textGrob(textgrob3,gp=gpar(fontsize=15,font=1)),
                grobTree(venn),legend,nrow=2,ncol=2,heights=c(1/16,7/8),widths=c(25/32,7/32))
   if (verbose==1) {
    if (q==1) {
     if (language=="italian") {
      cat(paste("Grafico Venn Indicatore Composito salvato in file: [",workDir,"/4_1_",q,"_venndiagram.pdf]\r",sep=""))
     } else if (language=="english") {
      cat(paste("Venn Diagram Composite Indicator saved in file: [",workDir,"/4_1_",q,"_venndiagram.pdf]\r",sep=""))
     }
    } else {
      if (language=="italian") {
       cat(paste("Grafico Venn Indicatore Composito Riferimento salvato in file: [",workDir,"/4_1_",q,"_venndiagram.pdf]\r",sep=""))
      } else if (language=="english") {
       cat(paste("Venn Diagram Composite Indicator saved in file: [",workDir,"/4_1_",q,"_venndiagram.pdf]\r",sep=""))
      }
    }
   }
   dev.off()

   png(file = paste("4_1_",q,"_venndiagram.png",sep=""),height=600,width=800)
   grid.arrange(top=textGrob(textgrob1,gp=gpar(fontsize=18,font=1)),textGrob(textgrob2,gp=gpar(fontsize=15,font=1)),textGrob(textgrob3,gp=gpar(fontsize=15,font=1)),grobTree(venn),legend,nrow=2,ncol=2,heights=c(1/16,7/8),widths=c(25/32,7/32))
   if (verbose==1) {
    if (q==1) {
     if (language=="italian") {
      cat(paste("Grafico Venn Indicatore Composito salvato in file: [",workDir,"/4_1_",q,"_venndiagram.png]\r",sep=""))
     } else if (language=="english") {
      cat(paste("Venn Diagram Composite Indicator saved in file: [",workDir,"/4_1_",q,"_venndiagram.png]\r",sep=""))
     }
    } else {
      if (language=="italian") {
       cat(paste("Grafico Venn Indicatore Composito Riferimento salvato in file: [",workDir,"/4_1_",q,"_venndiagram.png]\r",sep=""))
      } else if (language=="english") {
       cat(paste("Venn Diagram Composite Indicator saved in file: [",workDir,"/4_1_",q,"_venndiagram.png]\r",sep=""))
      }
    }
   }
   dev.off()

   rm(venn)

  } # graphs=1

  ##########################
  # Create disease coding

  if (target_tot_pat[q]>0) {

   namebits<-c()
   for (j in 1:length(list_patologie)) {
    if (j>1) {
     bits<-cbind(bits,as.character(table4_1[,list_patologie[j]]))
    } else {
     bits<-as.character(table4_1[,list_patologie[j]])
    }
   }
   bits<-as.data.frame(bits)
   names(bits)<-list_patologie

   # Conversion of the bit map of disease combinations to integer
   for (j in 1:nrow(bits)) {
    bits[j,c("bits")]<-BitsToInt(bits[j,list_patologie])
   }

   table4_1$idcomb<-bits[,c("bits")]

   rm(bits)

  }

  ######################

  if (reference!="") {

   if (q==1) { # input

    if (tot_pat>0) {
     table_input4_1<-table4_1[,c("rank","combination","idcomb","score_gq","freq",list_patologie)]
     table_input4_1$base<-1 # allow selecting only base observations
     table_input4_1_2<-table4_1_2[,c("rank","combination","score_gq")]
     table_input4_1_2$base<-1 # allow selecting only base observations
     rm(table4_1)
    } else {
     table_input4_1<-data.frame(matrix(ncol=5+length(list_patologie),nrow=1)) # empty dataset
     names(table_input4_1)<-c("rank","combination","idcomb","score_gq","freq",list_patologie)
     table_input4_1_2<-data.frame(matrix(ncol=3,nrow=1)) # empty dataset
     names(table_input4_1_2)<-c("rank","combination","score_gq")
    }
    table_input_tot4_1<-tot4_1[,c("rank","combination","idcomb","score_gq")]
    table_input_tot4_1$base<-1 # allow selecting only base observations
    rm(tot4_1)

   } else { # reference

    if (ref_tot_pat>0) {
     table4_1<-table4_1[,c("rank","combination","score_gq")]
     names(table4_1)<-c("ref_rank","combination","ref_score_gq") # rename
     table4_1_2<-table4_1_2[,c("rank","combination","score_gq")]
     names(table4_1_2)<-c("ref_rank","combination","ref_score_gq") # rename
     tot4_1<-tot4_1[,c("rank","combination","score_gq")]
     names(tot4_1)<-c("ref_rank","combination","ref_score_gq")
    } else {
     table4_1<-data.frame(matrix(ncol=3,nrow=1)) # empty dataset
     names(table4_1)<-c("ref_rank","combination","ref_score_gq")
     table4_1_2<-data.frame(matrix(ncol=3,nrow=1)) # empty dataset
     names(table4_1_2)<-c("ref_rank","combination","ref_score_gq")
     tot4_1<-data.frame(matrix(ncol=3,nrow=1)) # empty dataset
     names(tot4_1)<-c("ref_rank","combination","ref_score_gq")
     table4_1$combination<-""
     table4_1$ref_rank<-"-"
     table4_1$ref_score_gq<-"-"
     table4_1_2$combination<-""
     table4_1_2$ref_rank<-""
     table4_1_2$ref_score_gq<-"-"
     tot4_1$combination<-total_label
     tot4_1$ref_rank<-"-"
     tot4_1$ref_score_gq<-"-"
    }
    table4_1<-merge(table_input4_1,table4_1,by="combination",all.x=TRUE)
    table4_1$ref_rank<-ifelse(is.na(table4_1$ref_rank),"-",table4_1$ref_rank)
    table4_1$ref_score_gq<-ifelse(is.na(table4_1$ref_score_gq),"-",table4_1$ref_score_gq)
    table4_1_2<-merge(table_input4_1_2,table4_1_2,by="combination",all.x=TRUE)
    table4_1_2$ref_score_gq<-ifelse(is.na(table4_1_2$ref_score_gq),"-",table4_1_2$ref_score_gq)
    tot4_1<-merge(table_input_tot4_1,tot4_1,by="combination")
    rm(table_input4_1,table_input_tot4_1)
   } # end reference loop
  } # end reference condition

 } # loop over input and reference

 if (tot_pat>0) {
  table4_1<-table4_1[order(table4_1$rank),]
  table4_1_2<-table4_1_2[order(table4_1_2$rank),]
  table4_1_2$score_gq<-format(table4_1_2$score_gq,decimal.mark=decimalmark,nsmall=2)
  if (reference!="") {
   table4_1_2$ref_score_gq<-format(table4_1_2$ref_score_gq,decimal.mark=decimalmark,nsmall=2)
  }
 }

 if (xml==1) {

  if (tot_pat>0) {
   table4_1<-data.frame(rbindlist(list(table4_1,tot4_1),fill=TRUE))
  } else {
   table4_1<-tot4_1
  }

  if (language=="italian") {
   if (reference=="") {
    headlabs<-c("Rango","Combinazione","Score")
    footlabs<-c("Rango","Combinazione","Score")
   } else {
    headlabs<-c("Rango","Combinazione","Score","Rango [P.R.]","Rif.Score","")
    footlabs<-c("Rango","Combinazione","Score","Rango<?custom-linebreak?>[P.R.]","Score<?custom-linebreak?>[P.R.]","")
   }
   title_labs<-"Tabella 4.1.1 - Indicatore Composito della Qualita' della Cura"
   section<-"Sezione 4. Indicatori Compositi"
  } else if (language=="english") {
   if (reference=="") {
    headlabs<-c("Rank","Combination","Score")
    footlabs<-c("Rank","Combination","Score")
   } else {
    headlabs<-c("Rank","Combination","Score","Ref.Rank","Ref.Score","")
    footlabs<-c("Rank","Combination","Score","Rank<?custom-linebreak?>[R.P.]","Score<?custom-linebreak?>[R.P.]","")
   }
   title_labs<-"Table 4.1.1 - Quality of Care Composite Indicator"
   section<-"Section 4. Composite Indicators"
  }

  if (reference=="") {

   writeTable(file="report.xml",
    data=table4_1,
    append=append,
    vars=c("rank","combination","score_gq"),
    headlabs=headlabs,
    headwidt=c("40pt","480pt","60pt"),
    colalign=c("center","left","right"),
    headalign=c("center","left","center"),
    footlabs=footlabs,
    title=title_labs,
    section=section,
    graph=)

  } else {

   table4_1[,"deltarrow"]<-""
   if (ref_tot_pat>0) {
    table4_1[,"deltarrow"]<-ifelse(table4_1$score_gq>table4_1$ref_score_gq & table4_1$score_gq!="-",arrow_up,table4_1[,"deltarrow"])
    table4_1[,"deltarrow"]<-ifelse(table4_1$score_gq<table4_1$ref_score_gq & table4_1$score_gq!="-",arrow_down,table4_1[,"deltarrow"])
   }

   writeTable(file="report.xml",
    data=table4_1,
    append=append,
    vars=c("rank","combination","score_gq","ref_rank","ref_score_gq","deltarrow"),
    headlabs=headlabs,
    headwidt=c("50pt","460pt","50pt","50pt","50pt","15pt"),
    colalign=c("center","left","right","center","right","center"),
    headalign=c("center","left","center","center","center","center"),
    footlabs=footlabs,
    title=title_labs,
    section=section,
    graph=)

  }

  if (language=="italian") {
   if (reference=="") {
    headlabs<-c("Patologia","Score")
    footlabs<-c("Patologia","Score")
   } else {
    headlabs<-c("Patologia","Score","Rif.Score","")
    footlabs<-c("Patologia","Score","Score<?custom-linebreak?>[P.R.]","")
    footnote<-"[P.R.]=Popolazione Riferimento"
   }
   title_labs<-"Tabella 4.1.2 - Indicatore Composito della Qualita' della Cura"
  } else if (language=="english") {
   if (reference=="") {
    headlabs<-c("Disease","Score")
    footlabs<-c("Disease","Score")
   } else {
    headlabs<-c("Disease","Score","Ref.Score","")
    footlabs<-c("Disease","Score","Score<?custom-linebreak?>[R.P.]","")
    footnote<-"[R.P.]=Reference Population"
   }
   title_labs<-"Table 4.1.2 - Quality of Care Composite Indicator"
  }

  if (reference=="") {

   writeTable(file="report.xml",
    data=table4_1_2,
    vars=c("combination","score_gq"),
    headlabs=headlabs,
    headwidt=c("240pt","60pt"),
    colalign=c("left","right"),
    headalign=c("left","center"),
    footlabs=footlabs,
    title=title_labs,
    section=,
    graph=)

  } else {

   table4_1_2[,"deltarrow"]<-""
   if (ref_tot_pat>0) {
    table4_1_2[,"deltarrow"]<-ifelse(table4_1_2$score_gq>table4_1_2$ref_score_gq & table4_1_2$score_gq!="-",arrow_up,table4_1_2[,"deltarrow"])
    table4_1_2[,"deltarrow"]<-ifelse(table4_1_2$score_gq<table4_1_2$ref_score_gq & table4_1_2$score_gq!="-",arrow_down,table4_1_2[,"deltarrow"])
   }

   writeTable(file="report.xml",
    data=table4_1_2,
    vars=c("combination","score_gq","ref_score_gq","deltarrow"),
    headlabs=headlabs,
    headwidt=c("240pt","50pt","50pt","15pt"),
    colalign=c("left","right","right","center"),
    headalign=c("left","center","center","center"),
    footlabs=footlabs,
    footnote=footnote,
    title=title_labs,
    section=,
    graph=)

   table4_1_2<-table4_1_2[table4_1_2$base==1,] # select base observations

  }

  if (reference=="") {

    if (graphs>0) {
     fileConn<-file("report.xml",open="at")
     writeLines('<?custom-pagebreak?>',fileConn)
     close(fileConn)
     writeTable(file="report.xml",title=title,graph="4_1_1_venndiagram.pdf")
    }

  } else {

   if (graphs>0) {
    fileConn<-file("report.xml",open="at")
    writeLines('<?custom-pagebreak?>',fileConn)
    close(fileConn)
    writeTable(file="report.xml",title=title,graph=c("4_1_1_venndiagram.pdf","4_1_2_venndiagram.pdf"),graph_width="80")
   }

   table4_1<-table4_1[table4_1$base==1,] # select base observations

  }

  fileConn<-file("report.xml",open="at")
  writeLines('<?custom-pagebreak?>',fileConn)
  close(fileConn)

 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("..completata\r",sep=""))
  } else if (language=="english") {
   cat(paste("..completed\r",sep=""))
  }
 }

 ###### Costi #####################
 # Tabella 4.2
 ##################################

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("Elaborazione dati Tabella 4.2 avviata..",sep=""))
  } else if (language=="english") {
   cat(paste("Processing data Table 4.2 avviata..",sep=""))
  }
 }

 align<-c("align_1","align_2")

 if (tot_pat>0) {

  d1<-input_data[rowSums(input_data[list_patologie])>0,]

  table_comb<-aggregate(x=d1[c("COUNT",list_costi)],by=d1[list_patologie],FUN="sum")
  names(table_comb)<-c(list_patologie,"COUNT",list_costi)

  table4_2<-table4_1[,c("combination","rank","freq",list_patologie)]
  table4_2<-merge(table4_2,table_comb,by=list_patologie)
  table4_2<-table4_2[order(-table4_2$COUNT),]
  table4_2[,"COSTO_TOTALE"]<-rowSums(table4_2[,list_costi])

  # Total
  tot4_2<-data.frame(t(colSums(d1[c("COUNT",list_costi)])))
  tot4_2$COSTO_TOTALE<-sum(tot4_2)
  tot4_2$combination<-total_label
  tot4_2$rank<-""
  table4_2<-data.frame(rbindlist(list(table4_2,tot4_2),fill=TRUE))

  table4_2[,"align_1"]<-"center"
  table4_2[,"align_2"]<-"left"

  list_costi<-c(list_costi,"COSTO_TOTALE")
  list_costi_names_reduced<-c(list_costi_names_reduced,total_cost_label)
  for (j in 1:length(list_costi)) {
   table4_2[,list_costi[j]]<-ifelse(round(table4_2[,list_costi[j]]/table4_2$COUNT)>0,format(round(table4_2[,list_costi[j]]/table4_2$COUNT),big.mark=bigmark,decimal.mark=decimalmark),"0")
   table4_2[,paste("align_",j+2,sep="")]<-"right"
   align<-c(align,paste("align_",j+2,sep=""))
  }

  # Code Combination

  namebits<-c()
  for (j in 1:length(list_patologie)) {
   if (j>1) {
    bits<-cbind(bits,as.character(table4_2[,list_patologie[j]]))
   } else {
    bits<-as.character(table4_2[,list_patologie[j]])
   }
  }
  bits<-as.data.frame(bits)
  names(bits)<-list_patologie

  # Conversion of the bit map of disease combinations to integer
  for (j in 1:nrow(bits)) {
   bits[j,c("bits")]<-BitsToInt(bits[j,list_patologie])
  }

  table4_2$idcomb<-bits[,c("bits")]

  rm(bits,d1)

  ####################################

  print_table_tot4_2<-table4_2[table4_2$combination==total_label,]
  print_table4_2<-table4_2[table4_2$combination!=total_label,]
  print_table4_2<-suppressWarnings(print_table4_2[order(-as.numeric(gsub(bigmark,"",print_table4_2$COSTO_TOTALE,fixed=TRUE))),])
  table4_2<-rbind(print_table4_2,print_table_tot4_2)

  rm(print_table4_2,print_table_tot4_2)

 } else {

  list_costi<-c(list_costi,"COSTO_TOTALE")
  list_costi_names_reduced<-c(list_costi_names_reduced,total_cost_label)

  table4_2<-data.frame(matrix(ncol=3+length(list_costi),nrow=1))
  names(table4_2)<-c("rank","combination","idcomb",list_costi)

  table4_2$rank<-""
  table4_2$combination<-total_label
  table4_2$COSTO_TOTALE<-0
  for (f in 1:length(list_costi)) {
   table4_2[,list_costi[f]]<-0
  }

 }

 if (xml==1) {

  if (language=="italian") {
   labs<-c("Rango","Combinazione",list_costi_names_reduced)
   title_labs<-"Tabella 4.2 - Indicatore Composito delle Valorizzazioni Tariffarie (Euro Procapite)"
  } else if (language=="english") {
   labs<-c("Rank","Combination",list_costi_names_reduced)
   title_labs<-"Table 4.2 - Expenditure Composite Indicator (Euro Per capita)"
  }

  writeTable(file="report.xml",
   data=table4_2,
   vars=c("rank","combination",list_costi),
   headlabs=labs,
   headwidt=c("30pt","360pt",rep("50pt",length(list_costi))),
   colalign=c("center","left",rep("center",length(list_costi))),
   varcolalign=align,
   footlabs=labs,
   title=title_labs,
   graph=NULL)

 }

 if (verbose>0) {
  if (language=="italian") {
   cat(paste("completata\r",sep=""))
  } else if (language=="english") {
   cat(paste("completed\r",sep=""))
  }
 }

 ##################################

 if (tot_pat>0) {

  table4_1<-table4_1[,c("idcomb","score_gq")]

  table4_1$Anno<-as.integer(year)
  table4_1$CodiceASL<-operator
  table4_1$CodiceRiga<-table4_1$idcomb
  table4_1$Score<-suppressWarnings(ifelse(table4_1$score_gq!="-",gsub(decimalmark,".",table4_1$score_gq,fixed=TRUE),""))

  table4_1<-table4_1[,c("Anno","CodiceASL","CodiceRiga","Score")]

  ###

  table4_1_2<-table4_1_2[,c("rank","score_gq")]

  table4_1_2$Anno<-as.integer(year)
  table4_1_2$CodiceASL<-operator
  table4_1_2$CodiceRiga<-table4_1_2$rank
  table4_1_2$Score<-suppressWarnings(ifelse(table4_1_2$score_gq!="-",gsub(decimalmark,".",table4_1_2$score_gq,fixed=TRUE),""))

  table4_1_2<-table4_1_2[,c("Anno","CodiceASL","CodiceRiga","Score")]

 } else {

  table4_1_2<-data.frame(matrix(ncol=4,nrow=1))
  names(table4_1_2)<-c("Anno","CodiceASL","CodiceRiga","Score")

  table4_1_2$Anno<-as.integer(year)
  table4_1_2$CodiceASL<-operator
  table4_1_2$CodiceRiga<-""
  table4_1_2$Score<-""

 }

 #################################

 table4_2<-table4_2[,c("combination","idcomb",list_costi)]

 table4_2$Anno<-as.integer(year)
 table4_2$CodiceASL<-operator
 table4_2$CodiceRiga<-table4_2$idcomb

 for (f in 1:length(list_costi_names_reduced)) {
  table4_2[,list_costi_names_reduced[f]]<-suppressWarnings(as.numeric(gsub(".","",table4_2[,list_costi[f]],fixed=TRUE)))
 }

 table4_2<-table4_2[,c("Anno","CodiceASL","CodiceRiga",list_costi_names_reduced)]

 ###################################################################################################

 if (output!="") {
  data_output<-list(table4_1,table4_1_2,table4_2)
  files_output<-c("table4_1","table4_1_2","table4_2")
  files_desc<-c("Tabella 4.1.1","Tabella 4.1.2","Tabella 4.2")
  for (i in 1:length(data_output)) {
   write.csv(data_output[[i]],paste(output,"/",files_output[i],".csv",sep=""),row.names=FALSE,na="")
   if (verbose==1) {
    if (language=="italian") {
     cat(paste("Tabella dati ",files_desc[i]," salvata in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    } else if (language=="english") {
     cat(paste("Table data ",files_desc[i]," saved in file: [",workDir,"/",files_output[i],".csv]\r",sep=""))
    }
   }
  }
 }

 if (graphs==1) {
  file.remove("Rplots.pdf")
 }

 if (xml==1) {
  if (engine_type!="") {rm(input_data)}
  rm(table4_1,table4_1_2,table4_2)
 } else {

  # Make table available globally
  assign("table4_1",table4_1,envir=.GlobalEnv)
  assign("table4_1_2",table4_1_2,envir=.GlobalEnv)
  assign("table4_2",table4_2,envir=.GlobalEnv)

 }

  if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 4",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 4",sep=""))
  }
  cat("\r")
 }

}

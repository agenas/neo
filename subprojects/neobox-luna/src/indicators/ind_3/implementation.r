indicator3 <- function(xml=1,graphs=1,output=".",append=0,verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatori: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicators: started processing\r")
  }
  cat("########################################\r")
 }

 # xml=0 indicator in a loop xml=1 indicator directly to report

 # Set the working directory
 setwd(workDir)

 # Lettura dati generati da neobox
 input_data <- read.table(paste(workDir, "/input.csv", sep=""), header=TRUE, sep=",", colClasses="character")

 # Conversione delle colonne in numerico
 input_data <- make_numeric(input_data,c(list_numvars,"COUNT"))

 ########################
 # Criteri

 for (j in 1:length(list_criteri)) {

  for (k in 1:length(indicatori[[j]])) {

   if (j==1 & k==1) {section<-"Sezione 2. Indicatori"} else {section<-""}

   if (k==1) {subtitle<-list_criteri[j]} else {subtitle<-""}

   table<- data.frame(matrix(ncol=2,nrow=8))
   names(table)<-c("indicator","desc")

   describe<-indicatori_desc[[j]][k]

   for (h in 1:8) {
    table[h,"indicator"]<-indicatori_explanatory[h]
    table[h,"desc"]<-get(paste("indicatori_",indicatori_explanatory_vars[h],sep=""))[[j]][k]
   }

   if (language=="italian") {
    labs<-c("Caratteristiche","Descrizione")
   } else if (language=="english") {
    labs<-c("Caratteristiche","Description")
   }

   writeTable(file="report.xml",
              data=table,
              vars=c("indicator","desc"),
              headlabs=labs,
              footlabs=labs,
              headwidt=c("190pt","490pt"),
              colalign=c("left","left"),
              title=paste(toupper(indicatori[[j]][k]),". ",indicatori_desc[[j]][k],sep=""),
              section=section,
              subtitle=subtitle,
              graph=NULL)

   rm(table)

   # Preparazione tabella
   table2_1 <- data.frame(matrix(ncol=8,nrow=3))
   names(table2_1) <- c("zona","num","den","valore","align_1","align_2","align_3","align_4")
   table2_1[,"align_1"] <- "left"
   table2_1[,"align_2"] <- "right"
   table2_1[,"align_3"] <- "right"
   table2_1[,"align_4"] <- "center"

   # Inizializzazione tabella
   table2_1[1,"zona"]  <- "Zona"
   table2_1[2,"zona"]  <- "ASL"
   table2_1[3,"zona"]  <- "Regione"
  
   zona_data<-input_data[input_data$LIV1_ZONA==select_unit,]
   asl<-zona_data$LIV2_ASL[1]
   regione<-zona_data$LIV3_REGIONE[1]

   table2_1[1,"num"]   <- format(sum(input_data[input_data$LIV1_ZONA==select_unit,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")]),big.mark=bigmark)
   table2_1[1,"den"]   <- format(sum(input_data[input_data$LIV1_ZONA==select_unit,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]),big.mark=bigmark)
   table2_1[1,"valore"]<- format(round((sum(input_data[input_data$LIV1_ZONA==select_unit,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")])/sum(input_data[input_data$LIV1_ZONA==select_unit,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]))*100,2),decimal.mark=decimalmark,nsmall=2)

   table2_1[2,"num"]   <- format(sum(input_data[input_data$LIV2_ASL==asl,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")]),big.mark=bigmark)
   table2_1[2,"den"]   <- format(sum(input_data[input_data$LIV2_ASL==asl,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]),big.mark=bigmark)
   table2_1[2,"valore"]<- format(round((sum(input_data[input_data$LIV2_ASL==asl,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")])/sum(input_data[input_data$LIV2_ASL==asl,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]))*100,2),decimal.mark=decimalmark,nsmall=2)

   table2_1[3,"num"]   <- format(sum(input_data[input_data$LIV3_REGIONE==regione,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")]),big.mark=bigmark)
   table2_1[3,"den"]   <- format(sum(input_data[input_data$LIV3_REGIONE==regione,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]),big.mark=bigmark)
   table2_1[3,"valore"]<- format(round((sum(input_data[input_data$LIV3_REGIONE==regione,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")])/sum(input_data[input_data$LIV3_REGIONE==regione,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")]))*100,2),decimal.mark=decimalmark,nsmall=2)

   # Scrittura file report.xml
   writeTable(file = "report.xml",
      data  = table2_1,
      append = 1,
      vars = c("zona","num","den","valore"),
      headlabs = c("Zona", "Numeratore", "Denominatore","Indicatore %"),
      headwidt = c("140pt", "100pt", "100pt", "100pt"),
      colalign = c("left", "right", "right", "center"),
      headalign = c("left", "center", "center", "center"),
      varcolalign = c("align_1", "align_2", "align_3", "align_4"),
      footlabs = c("","Numeratore","Denominatore","Indicatore %"),
      title=" ",
      section="",
      graph="")

   #####################################

   agg_data_num<-aggregate(x=input_data[,paste(toupper(indicatori[[j]][k]),"_NUM",sep="")],by=list(input_data$LIV1_ZONA,input_data$LIV2_ASL),FUN="sum")
   agg_data_den<-aggregate(x=input_data[,paste(toupper(indicatori[[j]][k]),"_DEN",sep="")],by=list(input_data$LIV1_ZONA),FUN="sum")
   names(agg_data_num)<-c("LIV1_ZONA","LIV2_ASL","NUM")
   names(agg_data_den)<-c("LIV1_ZONA","DEN")
   
   agg_data<-merge(agg_data_num,agg_data_den,by=c("LIV1_ZONA"))
   agg_data$valore<-(agg_data$NUM/agg_data$DEN)*100

   agg_data$ZONA<-3
   agg_data$ZONA<-ifelse(agg_data$LIV1_ZONA==select_unit,1,agg_data$ZONA)
   agg_data$ZONA<-ifelse(agg_data$LIV1_ZONA!=select_unit & agg_data$LIV2_ASL==asl,2,agg_data$ZONA)

   selectlab<-"Selezione"
   selectlev<-c("Tua Zona","Zone della ASL","Zone")
 
   agg_data<-agg_data[order(-agg_data$valore),]
   agg_data$pos<-row(as.matrix(agg_data$valore))

   agg_data$sd<-sqrt((agg_data$valore/100*(1-(agg_data$valore/100)))/(agg_data$DEN))
   agg_data$lcl<-agg_data$valore-1.96*agg_data$sd
   agg_data$ucl<-agg_data$valore+1.96*agg_data$sd

   weimean<-weighted.mean(agg_data$valore,agg_data$DEN)

   if (!is.nan(weimean)==TRUE) {

    pdf(file=paste(workDir,"/","2_1_caterpillar_",j,"_",k,".pdf",sep=""),width=6,height=4)
 
    # create a new column with values 1 to 3 and same length as Zona 
    modulus <- function(x) (x - 1) %% 3 + 1
    indeces <- 1:length(agg_data$ZONA)
    dim(indeces) <- length(agg_data$ZONA)
    agg_data$Shape <- apply(indeces, 1, modulus)
    agg_data$Shape <- factor(agg_data$Shape, levels=unique(agg_data$Shape))

    wrapper <- function(x, ...) 
     {
      paste(strwrap(x, ...), collapse = "\n")
     }

    p<-ggplot(agg_data,aes(as.factor(pos),valore,fill=as.factor(ZONA),shape=as.factor(ZONA),colour=as.factor(ZONA)))

    #Added horizontal line at y=weighted average, error bars to points and points with size two
    p<-p+geom_hline(yintercept=weimean,color="blue",size=0.3)+
         geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.1,color="black") + 
         geom_point(size=1.2) +
         annotate("text",0.1,weimean,colour="blue",label="Media\nRegione",size=2.5,hjust=0)+
         scale_colour_manual(name="one",values=c("red","gray","black")[as.numeric(agg_data$Shape)],labels=selectlev)+
         scale_fill_manual(name="one",values=c("red","gray","black")[as.numeric(agg_data$Shape)],labels=selectlev)+
         scale_shape_manual(name="one",values=c(17,15,16)[as.numeric(agg_data$Shape)],labels=selectlev)+
         ylab("Valore dell'indicatore (%)")+
         theme_bw()+
         ggtitle(wrapper(paste(title="Grafico 1: ",describe,sep=""),width = 80))+
         theme(legend.position="bottom",
               legend.background=element_rect(colour='black',size=0.3),
               legend.key=element_rect(colour="white"),
               legend.margin=unit(0, "line"),
               legend.key.height=unit(0,"line"),
               legend.text=element_text(size=9),
               plot.margin=unit(c(0,0,0,0),"lines"),
               plot.title=element_text(family="Helvetica",face="bold",size=11),
               legend.title=element_blank(),
               panel.background=element_blank(),
               panel.border=element_blank(),
               axis.line.x=element_blank(),
               axis.line=element_line(colour='black',size=0.3),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_text(size=rel(1)),
               axis.ticks=element_blank(),
               panel.grid.minor.x=element_blank(),
               panel.grid.major.x=element_blank())
 
    #To put levels on y axis you just need to use coord_flip()
    # p <- p+ coord_flip()

    print(p)

    dev.off()

    if (graphs>0) {

     fileConn<-file("report.xml",open="at")
     writeLines('<?custom-pagebreak?>',fileConn)
     close(fileConn)

     writeTable(file="report.xml",
                title=title,
                graph=paste(workDir,"/","2_1_caterpillar_",j,"_",k,".pdf",sep=""))

     fileConn<-file("report.xml",open="at")
     writeLines(paste('<para><emphasis role="small">',wrapper("Il grafico riporta il valore dell’indicatore con i relativi Intervalli di Confidenza al 95% per ogni zona. La zona in esame e' in rosso. Le altre zone afferenti alla stessa ASL sono rappresentate con un quadrato grigio. Le zone di altre ASL sono rappresentate con un cerchio nero. La linea orizzontale individua la media di riferimento per l’indicatore.",width=80),"</emphasis></para>",sep=""),fileConn)
     writeLines('<?custom-pagebreak?>',fileConn)
     close(fileConn)

    }

   }

  }

 }

 if (verbose>0) {
  cat("\r")
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatori",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicators",sep=""))
  }
  cat("\r")
 }

}

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
# Utility common functions
# =====================================================

createCentralData<- function(input_files,list_numvars=NULL) {

 for (i in 1:length(input_files)) {
  file_a<-read.table(unzip(input_files[i],"neobox_output.csv",exdir="./tmp"),header=TRUE,sep=",",colClasses="character")
  file.remove("./tmp/neobox_output.csv")
  if (i>1) {output_data<-rbind(output_data,file_a)} else {output_data<-file_a}
  rm(file_a)
  if (i==length(input_files)) {unlink("./tmp",recursive=TRUE)}
 }

 if (!is.null(list_numvars)==TRUE) {
  output_data<-make_numeric(output_data,c(list_patologie,list_fonti_vars,list_costi_ext,list_costi_ext_2,list_numvars,list_highscore,list_ricoveri,"SUM_STRANIERI","COUNT"))
 }

 output_data

}

writeZipDescriptorFor <- function(zipfilename, fileslist, cleanup=TRUE) {
  descfilename <- paste(zipfilename, ".yml", sep="")
  fileConn<-file(descfilename)
  writeLines("# ZIP FILE DESCRIPTOR", fileConn)
  close(fileConn)
  fileConn<-file(descfilename, open="at")
  writeLines(paste('file: ', zipfilename, sep=""), fileConn)
  writeLines(paste('cleanup: ', ifelse(cleanup, 'true', 'false'), sep=""), fileConn)
  writeLines('files:', fileConn)

  for (i in 1:length(fileslist)) {
    writeLines(paste('- ', fileslist[i], sep=""), fileConn)
  }
  close(fileConn)
}

merge_table<- function(tables) {

 for (i in 1:length(tables)) {
  input<-read.table(paste(workDir,"/../",tables[i],"/input.csv",sep=""),header=TRUE,sep=",",colClasses="character")
  if (i==1) {
   output<-input
  } else {
   output<-merge(output,input[,names(input)!="COUNT"],by=list_merge,all.x=TRUE,all.y=TRUE)
   rm(input)
  }
 }
 return(output)

}

minidot<- function(file,mean,n,se=NULL,min=NULL,max=NULL,i_std=2,sig_column="sig") {

  x<-data.frame(matrix(ncol=3,nrow=length(mean)))
  names(x)<-c("mean","n","se")

  for (i in 1:length(mean)) {
   x[i,"mean"]<-mean[i]
   x[i,"n"]   <-n[i]
  }

  if (is.null(se)) {
    x$n<-ifelse(x$n==0,0.5,x$n)
    x$se<-sqrt((x$mean*(1-x$mean/100))/x$n)
    x$LL <- x$mean-1.96*x$se
    x$UL <- x$mean+1.96*x$se
    min<-0
    max<-100
  } else {
    x$se<-se
    x$LL <- x$mean-1.96*x$se
    x$UL <- x$mean+1.96*x$se
    if (is.null(min)) {
     min<-min(x$LL)-0.5*(max(x$UL)-min(x$LL))
     max<-max(x$UL)+0.5*(max(x$UL)-min(x$LL))
    }
  }

  x$color<-"blue"          # all values
  x$pch<-20                # all values
  x$order<-1

  if (i_std>0) {
    x$color[i_std]<-"red"   # reference standard
    x$pch[i_std]<-18        # reference standard
    x$order[i_std]<-0
    lower_ci<-x$LL
    upper_ci<-x$UL
    i_std<-1
  }

  x<-x[order(x$order),]

  if (i_std>0) {
   x$sig_col<-ifelse(x$LL>upper_ci,1,0)
   x$sig_col<-ifelse(x$UL<lower_ci,2,x$sig_col)
   sig_col<-x[row.names(x)>1,"sig_col"]
   assign(sig_column,sig_col,envir=.GlobalEnv)
  }

  bars<-c(min+0.1,min+((max-min)/5),(min+((max-min)/5)*2),min+((max-min)/5)*3,min+((max-min)/5)*4,max-0.1)
  ######################

  png(file=paste(file,".png",sep=""),width=240,height=60)
  par(mar=c(0,0,0,0))
  dotchart(x$mean,cex=1.5,pch=x$pch,xlim=c(min,max),main="",xlab=,gcolor="black",color=x$color)

  for (i in 1:nrow(x)){lines(x=c(x$LL[i],x$UL[i]), y=c(i,i),lwd=0.5,lend=1)}
  for (i in 1:6) {
   lines(x=c(bars[i],bars[i]),y=c(0,100),lwd=0.1,lend=1)
  }

  if (i_std>0) {
    polygon(c(x$LL[i_std],x$LL[i_std],x$UL[i_std],x$UL[i_std]),c(0,100,100,0),col=rgb(0.12,0.30,1,0.3),border=FALSE)
    polygon(c(0,x$LL[i_std],x$LL[i_std],0),c(0,0,100,100),col=rgb(0.12,0.56,1,0),border=FALSE)
    polygon(c(100,x$UL[i_std],x$UL[i_std],100),c(0,0,100,100),col=rgb(0.12,0.56,1,0),border=FALSE)
  }

  dev.off()

  ######################

  pdf(file=paste(file,".pdf",sep=""),width=0.8,height=0.12)
  par(mar=c(0,0,0,0))
  dotchart(x$mean,cex=0.5,pch=x$pch,xlim=c(min,max),main="",xlab=,gcolor="black",color=x$color)

  for (i in 1:nrow(x)){lines(x=c(x$LL[i],x$UL[i]), y=c(i,i),lwd=0.5,lend=1)}
  for (i in 1:6) {
   lines(x=c(bars[i],bars[i]),y=c(0,100),lwd=0.1,lend=1)
  }

  if (i_std>0) {
    polygon(c(x$LL[i_std],x$LL[i_std],x$UL[i_std],x$UL[i_std]),c(0,100,100,0),col=rgb(0.12,0.30,1,0.3),border=FALSE)
    polygon(c(0,x$LL[i_std],x$LL[i_std],0),c(0,0,100,100),col=rgb(0.12,0.56,1,0),border=FALSE)
    polygon(c(100,x$UL[i_std],x$UL[i_std],100),c(0,0,100,100),col=rgb(0.12,0.56,1,0),border=FALSE)
  }

  dev.off()

  rm(x)

}

# Write a docbook table
writeTable <- function(file,
                       append=1,
                       data=NULL,
                       vars,
                       headlabs,
                       headwidt,
                       colalign="",
                       varcolalign="",
                       headalign=NULL,
                       footlabs,
                       title="",
                       section="",
                       subtitle="",
                       footnote=NULL,
                       graph=NULL,
                       graph_width=100) {

 if (append==0) {

  fileConn<-file(file)
  writeLines("",fileConn)
  close(fileConn)
 }

 fileConn<-file(file,open="at")

 if (section!="") {
  writeLines(paste("<title>",section,"</title>",sep=""),fileConn)
 }

 if (!is.null(data)==TRUE) {

  table <- data

  # Write report section in xml
  if (subtitle!="") {
   writeLines('<para>',fileConn)
   writeLines(paste('<emphasis role="red">',subtitle,'</emphasis>',sep=""),fileConn)
   writeLines('</para>',fileConn)
  }
  writeLines('<para>',fileConn)
  writeLines("",fileConn)
  writeLines(' <table frame="all">',fileConn)
  writeLines("",fileConn)
  writeLines(paste('  <title>',title,'</title>',sep=""),fileConn)
  writeLines("",fileConn)
  writeLines(paste('  <tgroup cols="',length(vars),'" align="left" colsep="1" rowsep="1">',sep=""),fileConn)

  writeLines("",fileConn)
  for(i in 1:length(headlabs)){
   if (colalign[1]!="") {
    writeLines(paste('     <colspec colname="',headlabs[i],'"',' colwidth="',headwidt[i],'"',' align="',colalign[i],'"/>',sep=""),fileConn)
   } else {
    writeLines(paste('     <colspec colname="',headlabs[i],'"',' colwidth="',headwidt[i],'"/>',sep=""),fileConn)
   }
  }
  writeLines("",fileConn)

  writeLines('   <thead>',fileConn)
  writeLines('    <row>',fileConn)

  for(i in 1:length(footlabs)){
   if (!is.null(headalign)==TRUE) {
    writeLines(paste('     <entry align="',headalign[i],'">',footlabs[i],'</entry>',sep=""),fileConn)
   } else {
    writeLines(paste('     <entry>',footlabs[i],'</entry>',sep=""),fileConn)
   }
  }

  writeLines('    </row>',fileConn)
  writeLines('   </thead>',fileConn)

  writeLines("",fileConn)
  writeLines('   <tbody>',fileConn)

  for(j in 1:nrow(table)){
   writeLines("",fileConn)
   writeLines('    <row>',fileConn)
   for(k in 1:length(vars)){
     if (varcolalign[1]!="") {
      writeLines(paste('     <entry align="',table[j,varcolalign[k]],'">',table[j,vars[k]],'</entry>',sep=""),fileConn)
     } else {
      writeLines(paste('     <entry>',table[j,vars[k]],'</entry>',sep=""),fileConn)
     }
   }
   writeLines('    </row>',fileConn)
  }

  writeLines("",fileConn)
  writeLines('   </tbody>',fileConn)
  writeLines('  </tgroup>',fileConn)
  writeLines(' </table>',fileConn)
  writeLines("",fileConn)
  writeLines('</para>',fileConn)

 }

 if (!is.null(footnote)==TRUE) {
  writeLines(paste("<para>",footnote,"</para>",sep=""),fileConn)
 }

 if (!is.null(graph)==TRUE) {
  for (q in 1:length(graph)) {
   writeLines('<para>',fileConn)
   writeLines(' <mediaobject>',fileConn)
   writeLines('  <imageobject>',fileConn)
   writeLines(paste('   <imagedata align="center" fileref="',graph[q],'" scalefit="1" width="',graph_width,'%" contentdepth="100%" format="PDF"/>',sep=""),fileConn)
   writeLines('  </imageobject>',fileConn)
   writeLines(' </mediaobject>',fileConn)
   writeLines('</para>',fileConn)
   writeLines('<para></para>',fileConn)
  }
 }

 close(fileConn)

}

funnel_plot<- function(
                       title="",
                       rate,
                       rate_se=NULL,
                       names=NULL,
                       names_outliers=0,
                       plot_names=0,
                       colour_var=NULL,
                       colour_values=c("indianred2","grey"),
                       unit=100,
                       population,
                       binary=1,
                       p_mean="weighted",
                       p_se="weighted",
                       alpha_1=0.05,
                       alpha_2=0.002,
                       filename="",
                       graph="",
                       pdf_height=4.5,
                       pdf_width=7,
                       dot_size=0.9,
                       low=NULL,
                       high=NULL,
                       ylab="%",
                       xlab="Totale assistiti",
                       selectlab="Selezione",
                       selectlev=c("Si","No"),
                       dfout=""
                       ) {
 number<-population # X axis

 if (binary==1) {

  p<-rate/unit       # Y axis
  p.se <- sqrt((p*(1-p))/(number))

 } else {

  p<-rate

  if (p_se=="unweighted") {p.se <- mean(rate_se)}                               # Unweighted
  else if (p_se=="weighted") {p.se <- weighted.mean(rate_se,number,na.rm=TRUE)} # Weighted by N
  else {p.se<-p_se}                                                             # External Value

 }

 if (!is.null(colour_var)==TRUE) {
  cyl<-colour_var
 }

 if (!is.null(names)==TRUE) {
  if (!is.null(colour_var)==TRUE) {
   df <- data.frame(p,number,p.se,names,cyl)
   names(df)<-c("p","number","p.se","names","cyl")
   df<-df[order(-df$cyl),]  # put selection in foreground
  } else {
   df <- data.frame(p,number,p.se,names)
   names(df)<-c("p","number","p.se","names")
  }
 } else {
  if (!is.null(colour_var)==TRUE) {
   df <- data.frame(p,number,p.se,cyl)
   df<-df[order(-df$cyl),]  # put selection in foreground
  } else {
   df <- data.frame(p,number,p.se)
  }
 }

 # common effect (fixed effect model)
 if (p_mean=="unweighted") {p.fem <- mean(p)}                               # Unweighted
 else if (p_mean=="weighted") {p.fem <- weighted.mean(p,number,na.rm=TRUE)} # Weighted by N
 else if (p_mean=="meta") {p.fem <- weighted.mean(p,1/p.se^2,na.rm=TRUE)}   # Meta-Analysis
 else {
  p.fem<-as.numeric(p_mean)
  if (binary==1) {p.fem<-p.fem/unit}
 }                                                       # External Value

 # standard error (fixed effect model)
 if (binary==1) {
  se.fem<-sqrt(p.fem*(1-p.fem))
 } else {
  se.fem<-as.numeric(p_se)
 }

 p.fem<-ifelse(!is.na(p.fem),p.fem,0)
 se.fem<-ifelse(!is.na(se.fem),se.fem,0)

 # lower and upper limits for 95% and 99.8% CI, based on FEM estimator

 z_value1<-qnorm(1-alpha_1/2)
 z_value2<-qnorm(1-alpha_2/2)

 ##### Plot CIs

 max_step<-round((max(number)-min(number))*0.005)
 if (max_step==0) {max_step<-1}
 number.seq <- seq(min(number),max(number),max_step)
 number.ll95  <- p.fem - z_value1 * (se.fem / sqrt(number.seq))
 number.ul95  <- p.fem + z_value1 * (se.fem / sqrt(number.seq))
 number.ll99  <- p.fem - z_value2 * (se.fem / sqrt(number.seq))
 number.ul99  <- p.fem + z_value2 * (se.fem / sqrt(number.seq))

 dfCI <- data.frame(number.ll95, number.ul95, number.ll99, number.ul99, number.seq, p.fem)

 ## Determine outliers

 df$outlier<-0
 if (names_outliers>0) {
  df$outlier<-ifelse(df$p<(p.fem-z_value2*(se.fem/sqrt(df$number))),-1,df$outlier)
  df$outlier<-ifelse(df$p>(p.fem+z_value2*(se.fem/sqrt(df$number))),1,df$outlier)
  value_outlier<-1
 } else {
  value_outlier<-0
 }

 ## Rescale

 if (binary==1) {

  df$p<-df$p*unit

  dfCI$number.ll95<-dfCI$number.ll95*unit
  dfCI$number.ul95<-dfCI$number.ul95*unit
  dfCI$number.ll99<-dfCI$number.ll99*unit
  dfCI$number.ul99<-dfCI$number.ul99*unit
  dfCI$p.fem<-dfCI$p.fem*unit

  p.fem<-p.fem*unit

 }

 if (is.null(low)==TRUE)  {low <-min(dfCI$number.ll99,df$p) }
 if (is.null(high)==TRUE) {high<-max(dfCI$number.ul99,df$p) }

 ## Plot

 fp <- ggplot(df,aes(number,p),na.rm=TRUE)+
        labs(title=title)+
        geom_line(aes(x=number.seq,y=number.ll95,linetype=as.factor(c(2)),inherit.aes=FALSE),data=dfCI,na.rm=TRUE) +
        geom_line(aes(x=number.seq,y=number.ul95,linetype=as.factor(c(2)),inherit.aes=FALSE),data=dfCI,na.rm=TRUE) +
        geom_line(aes(x=number.seq,y=number.ll99,linetype=as.factor(c(1)),inherit.aes=FALSE),data=dfCI,na.rm=TRUE) +
        geom_line(aes(x=number.seq,y=number.ul99,linetype=as.factor(c(1)),inherit.aes=FALSE),data=dfCI,na.rm=TRUE) +
        geom_hline(aes(yintercept=p.fem),data = dfCI,na.rm=TRUE) +
        scale_linetype_manual("Prob.",values=c(1,2),labels=c(eval(1-alpha_2),eval(1-alpha_1))) +
        scale_y_continuous(limits=c(low,high))+xlab(xlab)+ylab(ylab)+theme_bw()

 colour_values<-colour_values
 selectlev<-selectlev

 if (plot_names!=0 & !is.null(names)) {  # Names outside and colour/dots inside

  # colour points INSIDE borders
  if (nrow(df[abs(df$outlier)!=value_outlier,])>0) {
   if (!is.null(colour_var)==TRUE) { # Dot in colour
    fp<-fp+geom_point(data=df[abs(df$outlier)!=value_outlier,],aes(colour=factor(cyl)),shape=20,size=dot_size,na.rm=TRUE)
   } else {                          # Dots in black
    fp<-fp+geom_point(data=df[abs(df$outlier)!=value_outlier,],shape=20,size=dot_size,na.rm=TRUE)
  }
   fp<-fp+scale_colour_manual(selectlab,values=colour_values,labels=selectlev)
  }

  # put names (optionally coloured) OUTSIDE borders
  if (nrow(df[abs(df$outlier)==value_outlier,])>0) {
   if (!is.null(colour_var)==TRUE) { # Names in colour
    fp<-fp+geom_text(data=df[abs(df$outlier)==value_outlier,],aes(number,p,label=names,colour=factor(cyl)),size=dot_size,inherit.aes=FALSE,na.rm=TRUE)
   } else {                          # Names in black
    fp<-fp+geom_text(data=df[abs(df$outlier)==value_outlier,],aes(number,p,label=names),size=dot_size,inherit.aes=FALSE,na.rm=TRUE)
   }
  }

 } else {  # black/coloured dots everywhere
  if (!is.null(colour_var)==TRUE) { # Dot in colour
   fp<-fp+geom_point(data=df,aes(colour=factor(cyl)),shape=20,size=dot_size,na.rm=TRUE)
   fp<-fp+scale_colour_manual(selectlab,values=colour_values,labels=selectlev)
  } else {                          # Dots in black
   fp<-fp+geom_point(data=df,shape=20,size=dot_size,na.rm=TRUE)
  }
 }

 if (filename!="") {
  pdf(paste(filename,".pdf",sep=""),width=pdf_width,height=pdf_height)
  print(fp)
  dev.off()
  png(paste(filename,".png",sep=""),width=pdf_width*60,height=pdf_height*60)
  print(fp)
  dev.off()
 }

 if (graph!="") {
  assign(graph,fp,envir=.GlobalEnv)
 }

 rm(p,number,p.se,names,number.seq,dfCI,fp)

 if (dfout!="") {
  assign(dfout,df,envir=.GlobalEnv)
 } else {
  rm(df)
 }

 if (!is.null(colour_var)==TRUE) {
  rm(cyl)
 }

}

make_numeric<- function(dataset,vars) {
 for (i in 1:length(vars)) {
  dataset[,vars[i]]<-as.numeric(dataset[,vars[i]])
  dataset[,vars[i]]<-ifelse(is.na(dataset[,vars[i]]),0,dataset[,vars[i]])
 }
 dataset
}

make_character<- function(dataset,vars) {
 for (i in 1:length(vars)) {
  dataset[,vars[i]]<-as.character(dataset[,vars[i]])
  dataset[,vars[i]]<-ifelse(is.na(dataset[,vars[i]]),"",dataset[,vars[i]])
 }
 dataset
}

make_numeric_logical<- function(dataset,vars) {
 for (i in 1:length(vars)) {
  dataset[,vars[i]]<-as.numeric(as.logical(toupper(dataset[,vars[i]])))
 }
 dataset
}

multigraph_grobs<- function(list_graphs,nrow=3,ncol=2,
                   title=paste("Funnel Plots Indicatori di Processo ",patologie_names[i],sep=""),
                   pdf_height=12,
                   pdf_width=10,
                   filename=paste("4.1-indicators_",patologie_names[i],sep="")) {

 max_per_page<-nrow*ncol
 n_pages<-ceiling(length(list_graphs)/max_per_page)

 for (page in 1:n_pages) {

  lower_limit<-page*max_per_page-max_per_page+1
  upper_limit<-min(length(list_graphs),page*max_per_page)

  page_list_graphs<-list_graphs[lower_limit:upper_limit]

  p_g<-list()
  replica<-c(2/36)
  g<-ncol
  cur_ind<-lower_limit

  for (k in 1:ncol) {
   p_g[[k]]<-textGrob("")
  }

  for (j in 1:nrow) {
   for (k in 1:ncol) {
    g<-g+1
    p_g[[g]]<-textGrob("")
   }
   replica<-c(replica,1/36)
   for (k in 1:ncol) {
    g<-g+1
    if (cur_ind<=length(list_graphs)) {p_g[[g]]<-ggplotGrob(get(page_list_graphs[ncol*(j-1)+k]))}
    else { p_g[[g]]<-textGrob("")}
    cur_ind<-cur_ind+1

   }
   replica<-c(replica,(16/36)*(2/nrow))
  }

  args.list <- c(p_g,
                 list(nrow=nrow*2+1,ncol=ncol,
                      main=textGrob(title,gp=gpar(lineheight=4,fontsize=20,font=1)),
                      heights=replica)
                 )

  pdf(file=paste(filename,"_",page,".pdf",sep=""),height=pdf_height,width=pdf_width)
  do.call(grid.arrange,args.list)
  dev.off()

  png(file=paste(filename,"_",page,".png",sep=""),height=pdf_height*120,width=pdf_width*120)
  do.call(grid.arrange,args.list)
  dev.off()

 }

}

IntToBits <- function(x) {
	if(x==0) { return(0); }
	n <- 1 + floor(log(x)/log(2));
	bits <- sapply(0:n,function(i) floor(1e-9 + x/2^i)%%2);
	return(bits);
}

BitsToInt <- function(bits) {
	on <- (0:(length(bits)-1))[bits==1]
	return(sum(2^on))
}

getModified <- function(dframe,value) {
  assign(dframe,value,envir=.GlobalEnv)
}

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
# Program to create sample datasets for the Neobox working example
# Author: Fabrizio Carinci, AGENAS, carinci@agenas.it
# February 2014
####################################################################

####################################################################
## H2 commands to access the dataset after creation
#
## program directory:
# cd  [neobox-homedir]/subprojects/neobox-app
## java client:
# java -cp staging/h2-1.3.170.jar org.h2.tools.Server
## to dump from client:
# call CSVWRITE ( '<put dataset name here>', 'SELECT * FROM THEMATRIX' ) ;
## examples of valid SQL commands:
# SHOW COLUMNS FROM THEMATRIX;
# SELECT MMG,COUNT(*) as COUNT FROM THEMATRIX GROUP BY MMG;
# SELECT MMG,sum(ifnull(NUM_RIC,0)) as SUM_RIC,sum(NUM_RIC is not null) as SUM_PAZ_RIC FROM THEMATRIX GROUP BY MMG;
####################################################################

indicator7 <- function(verbose=1) {

 if (verbose>0) {
  cat("########################################\r")
  if (language=="italian") {
   cat("Indicatore 7: avvio elaborazione\r")
  } else if (language=="english") {
   cat("Indicator 7: started processing\r")
  }
  cat("########################################\r")
 }
 
 # xml=0 indicator in a loop xml=1 indicator directly to report

 # Set the working directory
 setwd(workDir)

 ####################################################################
 # Change main parameters to create a dataset of the desired size and location
 n<-175000          # small number
# n<-2500000          # high number
 id_province<-"PPP" # id province
 id_lha<-"LLL"      # id local health authority
 n_comune<-206      # no. councils in local health authority
 ####################################################################
 # Other parameters are automatically adjusted to:
 #  1 GP every 1,200 citizens on average
 #  1 district every 15 GPs
 #  1 general practice group level A every 10 GPs
 #  1 general practice group level B every 8 GPs
 #  1 general practice group level C every 4 GPs
 #  1 general practice group level B every 2 GPs
 # Probabilities for frequencies in the report can be changed in the program
 ####################################################################

 # Set the working directory
 setwd(workDir)

 options(scipen=999)

 list_patologie<-c("HYPERTE","DIAB","IHD","HF","DEMEN")
 
 x<-data.frame(matrix(ncol=0,nrow=n))

 x[,"ID_SOGGETTO"]<-as.character(rownames(x))
 x[,"ID"]<-as.character(rownames(x))

 n_mmg<-ceiling(n/1200)
 x[,"MMG"]<-sample(1:n_mmg,size=n,replace=TRUE,prob=c(rep(1/n_mmg,n_mmg)))

 # Demographics

 set.seed(42); 

 random_date<-function(n=1000,st="2012-01-01", et="2012-12-31") {
  st<-as.POSIXct(as.Date(st))
  et<-as.POSIXct(as.Date(et))
  dt<-as.numeric(difftime(et,st,unit="sec"))
  ev<-sort(runif(n,0,dt))
  rt<-st+ev
  x_random_date<-format(rt,'%Y-%m-%d')
  x_random_order<-runif(n,0,1)
  x_random<-as.data.frame(cbind(x_random_date,x_random_order))
  x_random<-x_random[order(x_random$x_random_order),]
  x_random["x_random_date"]
 }

 x[,"DATA"]<-random_date(n=n,st="2012-01-01",et="2012-12-31")
 x[,"DATA"]<-as.character(x[,"DATA"])

 x[,"DATA_NASCITA"]<-random_date(n=n,st="1910-01-01",et="2011-12-31")
 x[,"DATA_NASCITA"]<-as.character(x[,"DATA_NASCITA"])

 x[,"DEAD"]<-sample(c(0,1),size=n,replace=TRUE,prob=c(0.99,0.01))
 x[,"DATA_MORTE"]<-random_date(n=n,st="2012-01-01",et="2012-12-31")
 x[,"DATA_MORTE"]<-ifelse(x[,"DEAD"]==1,as.character(x[,"DATA_MORTE"]),"")

 x[,"IN_CARICO"]<-sample(c(0,1),size=n,replace=TRUE,prob=c(0.01,0.99))
 x[,"IN_CARICO_12M"]<-sample(c(0,1),size=n,replace=TRUE,prob=c(0.01,0.99))

 x[,"COMUNE"]<-sample(1:n_comune,size=n,replace=TRUE,prob=c(rep(n/n_comune,n_comune)))   # number of councils
 x[,"COMUNE"]<-as.character(x[,"COMUNE"])
 x[,"COMUNE"]<-ifelse(nchar(x[,"COMUNE"])==1,paste("00",x[,"COMUNE"],sep=""),x[,"COMUNE"])
 x[,"COMUNE"]<-ifelse(nchar(x[,"COMUNE"])==2,paste("0",x[,"COMUNE"],sep=""),x[,"COMUNE"])
 ####################################################################
 # Change this to create a dataset with different location
 x[,"COMUNE"]<-paste(id_lha,x[,"COMUNE"],sep="")
 ####################################################################

 x[,"SESSO"]<-sample(c("M","F"),size=n,replace=TRUE,prob=c(1-0.5075,0.5075))
 x[,"CITTADINANZA"]<-sample(c(1,2),size=n,replace=TRUE,prob=c(0.88,0.12))

 # Diseases

 comb_patologie<-c("00000", # Sano
                   "10000", # Ipertensione
                   "01000", # Diabete
                   "11000", # Ipertensione + Diabete
                   "00100", # Cardiopatia Ischemica
                   "10100", # Ipertensione + Cardiopatia Ischemica
                   "00110", # Cardiopatia Ischemica + Scompenso Cardiaco
                   "01100", # Diabete + Cardiopatia Ischemica
                   "00010", # Scompenso Cardiaco
                   "11100", # Ipertensione + Diabete + Cardiopatia Ischemica
                   "10110", # Ipertensione + Cardiopatia Ischemica + Scompenso Cardiaco
                   "01110", # Diabete + Cardiopatia Ischemica + Scompenso Cardiaco
                   "10010", # Ipertensione + Scompenso Cardiaco
                   "11110", # Ipertensione + Diabete + Cardiopatia Ischemica + Scompenso Cardiaco
                   "00001", # Demenza
                   "01010", # Diabete + Scompenso Cardiaco
                   "11010", # Ipertensione + Diabete + Scompenso Cardiaco
                   "01001", # Diabete + Demenza
                   "10001", # Ipertensione + Demenza
                   "00101", # Cardiopatia Ischemica + Demenza
                   "11001", # Ipertensione + Diabete + Demenza
                   "10101", # Ipertensione + Cardiopatia Ischemica + Demenza
                   "01101", # Diabete + Cardiopatia Ischemica + Demenza
                   "00011", # Scompenso Cardiaco + Demenza
                   "00111", # Cardiopatia Ischemica + Scompenso Cardiaco + Demenza
                   "11101", # Ipertensione + Diabete + Cardiopatia Ischemica + Demenza
                   "01111", # Diabete + Cardiopatia Ischemica + Scompenso Cardiaco + Demenza
                   "01011", # Diabete + Scompenso Cardiaco + Demenza
                   "10111", # Ipertensione + Cardiopatia Ischemica + Scompenso Cardiaco + Demenza
                   "10011", # Ipertensione + Scompenso Cardiaco + Demenza
                   "11111", # Ipertensione + Diabete + Cardiopatia Ischemica + Scompenso Cardiaco + Demenza
                   "11011") # Ipertensione + Diabete + Scompenso Cardiaco + Demenza

 ####################################################################
 # Change this to create a dataset with different impact of diseases
 list_comb_patologie_prob<-c(4.49,2.42,1.07,1.06,0.55,0.34,0.30,0.27,0.26,0.13,0.12,0.11,0.10,0.08,0.07,0.06,0.02,0.02,0.01,0.01,0.00003539,0.000030672,0.000029885,0.000022414,0.000013763,0.00001101,0.000009831,0.000009831,0.000007078,0.000006292,0.000002753)
 ####################################################################

 for (j in 1:length(list_comb_patologie_prob)) {
  list_comb_patologie_prob[j]<-list_comb_patologie_prob[j]/100
 }

 prob_sano<-1-sum(list_comb_patologie_prob)
 list_comb_patologie_prob<-c(prob_sano,list_comb_patologie_prob)

 vector_comb_patologie<-sample(comb_patologie,size=n,replace=TRUE,prob=list_comb_patologie_prob)

 for (j in 1:length(list_patologie)) {
  x[,list_patologie[j]]<-as.numeric(substr(vector_comb_patologie,j,j))
 }

 x[,"NUM_RIC"]<-ifelse(rowSums(x[list_patologie])>0,rpois(n,0.5),rpois(n,0.25))
 x[,"NUM_RIC"]<-round(x[,"NUM_RIC"])
 x[,"NUM_RIC"]<-ifelse(x[,"NUM_RIC"]<0,0,x[,"NUM_RIC"])
 x[,"NUM_RIC"]<-as.character(x[,"NUM_RIC"])
 x[,"NUM_RIC"]<-ifelse(x[,"NUM_RIC"]=="0","",x[,"NUM_RIC"])

 # Sources
 
 comb_fonti<-c("HOSP","EXE","DRUGS","HOSP EXE","HOSP DRUGS","EXE DRUGS","HOSP EXE DRUGS")
 x[,"FONTI_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(comb_fonti),size=n,replace=TRUE,prob=c(0,1,0,0,0,0,0)),"")
 x[,"FONTI_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(comb_fonti,size=n,replace=TRUE,prob=c(0.03,0.18,0.14,0.02,0.05,0.31,0.27)),"")
 x[,"FONTI_IHD"]<-ifelse(x[,"IHD"]==1,sample(comb_fonti,size=n,replace=TRUE,prob=c(0.38,0.09,0.13,0.15,0.16,0.01,0.08)),"")
 x[,"FONTI_HF"]<-ifelse(x[,"HF"]==1,sample(comb_fonti,size=n,replace=TRUE,prob=c(0.89,0.06,0,0.05,0,0,0)),"")
 x[,"FONTI_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(comb_fonti,size=n,replace=TRUE,prob=c(0,0,1,0,0,0,0)),"")

 # Set numerators, denominators

 x[,"NUM_CREATININ_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5650,0.5650)),0)
 x[,"DEN_CREATININ_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,1,0)
 x[,"NUM_PROFLIP_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4242,0.4242)),0)
 x[,"DEN_PROFLIP_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,1,0)
 x[,"NUM_ECG_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.2642,0.2642)),0)
 x[,"DEN_ECG_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,1,0)
 x[,"NUM_MICROALB_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.1445,0.1445)),0)
 x[,"DEN_MICROALB_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,1,0)
 x[,"NUM_GLICEM_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5713,0.5713)),0)
 x[,"DEN_GLICEM_HYPERTE"]<-ifelse(x[,"HYPERTE"]==1,1,0)

 x[,"NUM_EMOGLIC_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.3749,0.3749)),0)
 x[,"DEN_EMOGLIC_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)
 x[,"NUM_PROFLIP_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5477,0.5477)),0)
 x[,"DEN_PROFLIP_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)
 x[,"NUM_MICROALB_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4198,0.4198)),0)
 x[,"DEN_MICROALB_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)
 x[,"NUM_FUNDOC_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.2521,0.2521)),0)
 x[,"DEN_FUNDOC_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)
 x[,"NUM_STATINE_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4008,0.4008)),0)
 x[,"DEN_STATINE_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)
 x[,"NUM_CREATININ_DIAB"]<-ifelse(x[,"DIAB"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.6703,0.6703)),0)
 x[,"DEN_CREATININ_DIAB"]<-ifelse(x[,"DIAB"]==1,1,0)

 x[,"NUM_PROFLIP_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4640,0.4640)),0)
 x[,"DEN_PROFLIP_IHD"]<-ifelse(x[,"IHD"]==1,1,0)
 x[,"NUM_GLICEM_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.6489,0.6489)),0)
 x[,"DEN_GLICEM_IHD"]<-ifelse(x[,"IHD"]==1,1,0)
 x[,"DEN_ANTIAGGREG_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.240129829,0.240129829)),0)
 x[,"NUM_ANTIAGGREG_IHD"]<-ifelse(x[,"DEN_ANTIAGGREG_IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.6293,0.6293)),0)
 x[,"DEN_BETABLOC_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.240129829,0.240129829)),0)
 x[,"NUM_BETABLOC_IHD"]<-ifelse(x[,"DEN_BETABLOC_IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4489,0.4489)),0)
 x[,"NUM_ACEINIB_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5926,0.5926)),0)
 x[,"DEN_ACEINIB_IHD"]<-ifelse(x[,"IHD"]==1,1,0)
 x[,"DEN_STATINE_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.240129829,0.240129829)),0)
 x[,"NUM_STATINE_IHD"]<-ifelse(x[,"DEN_STATINE_IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4079,0.4079)),0)
 x[,"NUM_ECG_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4075,0.4075)),0)
 x[,"DEN_ECG_IHD"]<-ifelse(x[,"IHD"]==1,1,0)
 x[,"NUM_ERGOMETR_IHD"]<-ifelse(x[,"IHD"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.04,0.04)),0)
 x[,"DEN_ERGOMETR_IHD"]<-ifelse(x[,"IHD"]==1,1,0)

 x[,"NUM_CREATNAK_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5320,0.5320)),0)
 x[,"DEN_CREATNAK_HF"]<-ifelse(x[,"HF"]==1,1,0)
 x[,"NUM_ACEINIB_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.5490,0.5490)),0)
 x[,"DEN_ACEINIB_HF"]<-ifelse(x[,"HF"]==1,1,0)
 x[,"NUM_ECG_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4310,0.4310)),0)
 x[,"DEN_ECG_HF"]<-ifelse(x[,"HF"]==1,1,0)
 x[,"NUM_BETABLOC_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4334,0.4334)),0)
 x[,"DEN_BETABLOC_HF"]<-ifelse(x[,"HF"]==1,1,0)
 x[,"DEN_NAKDIUR_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.951683606,0.951683606)),0)
 x[,"NUM_NAKDIUR_HF"]<-ifelse(x[,"DEN_NAKDIUR_HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.7419,0.7419)),0)
 x[,"NUM_WALKTEST_HF"]<-ifelse(x[,"HF"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0113,0.0113)),0)
 x[,"DEN_WALKTEST_HF"]<-ifelse(x[,"HF"]==1,1,0)

 x[,"NUM_VISNEURO_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0829,0.0829)),0)
 x[,"DEN_VISNEURO_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"NUM_TESTMEM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0101,0.0101)),0)
 x[,"DEN_TESTMEM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"NUM_EMATOCHIM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.4086,0.4086)),0)
 x[,"DEN_EMATOCHIM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"NUM_ECG_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.3105,0.3105)),0)
 x[,"DEN_ECG_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"NUM_NEUROLATIP_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0692,0.0692)),0)
 x[,"DEN_NEUROLATIP_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"NUM_NEUROLTIP_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0064,0.0064)),0)
 x[,"DEN_NEUROLTIP_DEMEN"]<-ifelse(x[,"DEMEN"]==1,1,0)
 x[,"DEN_NEUROLATIVSTI_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.998675146,0.998675146)),0)
 x[,"NUM_NEUROLATIVSTI_DEMEN"]<-ifelse(x[,"DEN_NEUROLATIVSTI_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0692,0.0692)),0)
 x[,"DEN_ASSDOM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.339162692,0.339162692)),0)
 x[,"NUM_ASSDOM_DEMEN"]<-ifelse(x[,"DEN_ASSDOM_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0,0)),0)
 x[,"DEN_ASSRES_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.339162692,0.339162692)),0)
 x[,"NUM_ASSRES_DEMEN"]<-ifelse(x[,"DEN_ASSRES_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0789,0.0789)),0)
 x[,"DEN_ASSREDEM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.339162692,0.339162692)),0)
 x[,"NUM_ASSREDEM_DEMEN"]<-ifelse(x[,"DEN_ASSREDEM_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0,0)),0)
 x[,"DEN_ASSSEMIRES_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.339162692,0.339162692)),0)
 x[,"NUM_ASSSEMIRES_DEMEN"]<-ifelse(x[,"DEN_ASSSEMIRES_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0,0)),0)
 x[,"DEN_ASSSEMIREDEM_DEMEN"]<-ifelse(x[,"DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.339162692,0.339162692)),0)
 x[,"NUM_ASSSEMIREDEM_DEMEN"]<-ifelse(x[,"DEN_ASSSEMIREDEM_DEMEN"]==1,sample(c(0,1),size=n,replace=TRUE,prob=c(1-0.0375,0.0375)),0)

 # Resources
 
 x[,"TARIFFA_LORDO_VISITE"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=40,sd=10),rnorm(n,mean=20,sd=9))
 x[,"TARIFFA_LORDO_VISITE"]<-ifelse(x[,"TARIFFA_LORDO_VISITE"]<0,0,x[,"TARIFFA_LORDO_VISITE"])

 x[,"TARIFFA_LORDO_DIA_IMM"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=97,sd=20),rnorm(n,mean=30,sd=12))
 x[,"TARIFFA_LORDO_DIA_IMM"]<-ifelse(x[,"TARIFFA_LORDO_DIA_IMM"]<0,0,x[,"TARIFFA_LORDO_DIA_IMM"])

 x[,"TARIFFA_LORDO_DIA_LAB"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=88,sd=18),rnorm(n,mean=26,sd=10))
 x[,"TARIFFA_LORDO_DIA_LAB"]<-ifelse(x[,"TARIFFA_LORDO_DIA_LAB"]<0,0,x[,"TARIFFA_LORDO_DIA_LAB"])

 x[,"TARIFFA_LORDO_FARMACI"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=582,sd=210),rnorm(n,mean=85,sd=351))
 x[,"TARIFFA_LORDO_FARMACI"]<-ifelse(x[,"TARIFFA_LORDO_FARMACI"]<0,0,x[,"TARIFFA_LORDO_FARMACI"])

 x[,"TARIFFA_LORDO_OSPACDRGMED"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=313,sd=90),rnorm(n,mean=120,sd=90))
 x[,"TARIFFA_LORDO_OSPACDRGMED"]<-ifelse(x[,"TARIFFA_LORDO_OSPACDRGMED"]<0,0,x[,"TARIFFA_LORDO_OSPACDRGMED"])

 x[,"TARIFFA_LORDO_OSPACDRGCHIR"]<-ifelse(rowSums(x[list_patologie])>0,rnorm(n,mean=600,sd=120),rnorm(n,mean=210,sd=180))
 x[,"TARIFFA_LORDO_OSPACDRGCHIR"]<-ifelse(x[,"TARIFFA_LORDO_OSPACDRGCHIR"]<0,0,x[,"TARIFFA_LORDO_OSPACDRGCHIR"])

 # clean diseases
 
 x[,"STADIO_HYPERTE_12M"]<-ifelse(x[,"HYPERTE"]==1,sample(1:4,size=n,replace=TRUE,prob=c(0.50,0.20,0.20,0.10)),0)
 x[,"STADIO_DIAB_12M"]<-ifelse(x[,"DIAB"]==1,sample(1:4,size=n,replace=TRUE,prob=c(0.50,0.20,0.20,0.10)),0)
 x[,"STADIO_IHD_12M"]<-ifelse(x[,"IHD"]==1,sample(1:4,size=n,replace=TRUE,prob=c(0.50,0.20,0.20,0.10)),0)
 x[,"STADIO_HF_12M"]<-ifelse(x[,"HF"]==1,sample(1:4,size=n,replace=TRUE,prob=c(0.50,0.20,0.20,0.10)),0)

 x[,"STADIO_HYPERTE_12M"]<-as.character(x[,"STADIO_HYPERTE_12M"])
 x[,"STADIO_DIAB_12M"]<-as.character(x[,"STADIO_DIAB_12M"])
 x[,"STADIO_IHD_12M"]<-as.character(x[,"STADIO_IHD_12M"])
 x[,"STADIO_HF_12M"]<-as.character(x[,"STADIO_HF_12M"])
 x[,"ANTICOLIN_12M"]<-as.character(x[,"DEMEN"])

 x$HYPERTE<-NULL
 x$DIAB<-NULL
 x$IHD<-NULL
 x$HF<-NULL
 x$DEMEN<-NULL

 write.csv(x,paste(workDir,"/testdata_thematrix.csv",sep=""),quote=FALSE,row.names=FALSE,na="")

 # Dataset distretti

 n_mmg<-ceiling(n/1200)        # one GP every 1,200 citizens on average
 n_dist<-ceiling(n_mmg/15)     # one district every 15 GPs
 n_groups_A<-ceiling(n_mmg/10) # a general practice group level A every 10 GPs
 n_groups_B<-ceiling(n_mmg/8)  # a general practice group level B every 8 GPs
 n_groups_C<-ceiling(n_mmg/4)  # a general practice group level C every 4 GPs
 n_groups_D<-ceiling(n_mmg/2)  # a general practice group level B every 2 GPs

 y<-data.frame(matrix(ncol=0,nrow=n_mmg))

 ####################################################################
 # Change this to create a different operator
 y[,"ASL"]<-paste(id_province,id_lha,sep="")
 ####################################################################

 y[,"MMG"]<-sample(1:n_mmg,size=n_mmg,replace=FALSE,prob=c(rep(1/n_mmg,n_mmg)))
 y[,"MMG"]<-as.character(y[,"MMG"])

 y[,"GRUPPO_A"]<-sample(1:n_groups_A,size=n_mmg,replace=TRUE,prob=c(rep(1/n_groups_A,n_groups_A)))
 y[,"GRUPPO_A"]<-as.character(y[,"GRUPPO_A"])

 y[,"GRUPPO_B"]<-sample(1:n_groups_B,size=n_mmg,replace=TRUE,prob=c(rep(1/n_groups_B,n_groups_B)))
 y[,"GRUPPO_B"]<-as.character(y[,"GRUPPO_B"])

 y[,"GRUPPO_C"]<-sample(1:n_groups_C,size=n_mmg,replace=TRUE,prob=c(rep(1/n_groups_C,n_groups_C)))
 y[,"GRUPPO_C"]<-as.character(y[,"GRUPPO_C"])

 y[,"GRUPPO_D"]<-sample(1:n_groups_D,size=n_mmg,replace=TRUE,prob=c(rep(1/n_groups_D,n_groups_D)))
 y[,"GRUPPO_D"]<-as.character(y[,"GRUPPO_D"])

 y[,"DISTRETTO"]<-sample(1:n_dist,size=n_mmg,replace=TRUE,prob=c(rep(1/n_dist,n_dist)))

 y[,"DATA_INIZIO"]<-as.Date("2000-01-01")
 y[,"DATA_FINE"]<-""
 y[,"MOT_FINE"]<-""

 write.csv(y,paste(workDir,"/testdata_distretti.csv",sep=""),quote=FALSE,row.names=FALSE,na="")
 
 if (verbose>0) {
  cat("\r") 
  if (language=="italian") {
   cat(paste("Fine elaborazione Indicatore 7",sep=""))
  } else if (language=="english") {
   cat(paste("End processing Indicator 7",sep=""))
  }
  cat("\r")
 }
 
}
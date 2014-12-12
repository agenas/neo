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

liste_opzioni<-function(installed=1)

{

 # Print all warnings
 options(warn=1)

 # Suppress scientific notation
 options(scipen=999)

 PkgsNeed<-c("VennDiagram","gridExtra","gdata","vcd","scales","gtable","ggplot2","plyr","chron","data.table","utils")

 for (i in 1:length(PkgsNeed)) {
  if (installed==1) {
   suppressMessages(library(PkgsNeed[i],character.only=TRUE))
  }
 }

 ############## impostate da neobox al momento del lancio

# language<-"it"
# engine_type<-"local"
# operator<-"Fab"
# year<-"2012"
# select_unit=""
# reference<-""

#####################################
# Alcuni valori prova:
#####################################
# input_files<-c("/home/fabrizio/Desktop/neobox_run/data/tracciato_record_centrale_arezzo.zip","/home/fabrizio/Desktop/neobox_run/data/tracciato_record_centrale_brescia.zip")
# select_unit="DIST_MMG=='004'" (locale)
# select_unit="(ASL=='030302' & DIST_MMG %in% c('001','002','012')) | ASL=='108'" (centrale)
# funnel_group=""
# funnel_group="MMG=='30743' | MMG=='15774' | MMG=='33722' | MMG=='30788' | MMG=='04616'" (locale)
# funnel_group="ASL=='030302' & DIST_MMG=='012'" (centrale)
#####################################

 if (language=="it") {
  language="italian"
 } else if (language=="en") {
  language="english"
 }

 if (select_unit=="" & reference=="_internal_") {reference=""}

 output_file<-"tracciato_record_centrale"

 ###################################################

 assign("language",language,envir=.GlobalEnv)
 assign("engine_type",engine_type,envir=.GlobalEnv)
 assign("operator",operator,envir=.GlobalEnv)
 assign("year",year,envir=.GlobalEnv)
 assign("funnel_group",funnel_group,envir=.GlobalEnv)
 assign("select_unit",select_unit,envir=.GlobalEnv)
 assign("reference",reference,envir=.GlobalEnv)

 assign("output_file",output_file,envir=.GlobalEnv)
 assign("input_files",input_files,envir=.GlobalEnv)

 ############## fisse, specifiche per applicazione Matrice

 if (language=="italian") {
  bigmark="."
  decimalmark=","
 } else  if (language=="english") {
  bigmark=","
  decimalmark="."
 }

 global_system_levels<-c("ASL")
 pop_levels<-c("DIST_MMG")
 local_system_levels<-c("MMG")
 adjusters<-c("SESSO","AGE_RANGE")
 list_patologie<-c("HYPERTE","DIAB","IHD","HF","DEMEN")

 list_merge=c(pop_levels,local_system_levels,adjusters,list_patologie)

 target<-"DIST_MMG"  # output target for funnel plot data
 if (language=="italian") {
  target_name<-"Distretto"
 } else if (language=="english") {
  target_name<-"District"
 }

 if (engine_type=="local") {
  funnel_unit<-"MMG"
  dot_size<-0.9
  if (language=="italian") {
   funnel_unit_name<-"Medico"
  } else if (language=="english") {
   funnel_unit_name<-"GP"
  }
 } else if (engine_type=="central") {
  funnel_unit<-"DIST_MMG"
  dot_size<-1.8
  if (language=="italian") {
   funnel_unit_name<-"Distretto Medico"
  } else if (language=="english") {
   funnel_unit_name<-"GP District"
  }
 }

 list_ricoveri<-c("SUM_RIC","SUM_PAZ_RIC","SUM_RIC_2")

 list_costi<-  c("COSTO_VISITE","COSTO_DIAIMM","COSTO_DIALIB","COSTO_FARMACI","COSTO_OSP")
 list_costi_2<-c("COSTO_VISITE_2","COSTO_DIAIMM_2","COSTO_DIALIB_2","COSTO_FARMACI_2","COSTO_OSP_2")

 list_costi_ext<-c("COSTO_VISITE","COSTO_DIAIMM","COSTO_DIALIB","COSTO_FARMACI","COSTO_OSP","COSTO")
 list_costi_ext_2<-c("COSTO_VISITE_2","COSTO_DIAIMM_2","COSTO_DIALIB_2","COSTO_FARMACI_2","COSTO_OSP_2","COSTO_2")

 indicatori<-list(
  c("CREATININ","PROFLIP","ECG","MICROALB","GLICEM"),
  c("EMOGLIC","PROFLIP","MICROALB","FUNDOC","STATINE","CREATININ"),
  c("PROFLIP","GLICEM","ANTIAGGREG","BETABLOC","ACEINIB","STATINE","ECG","ERGOMETR"),
  c("CREATNAK","ACEINIB","ECG","BETABLOC","NAKDIUR","WALKTEST"),
  c("VISNEURO","TESTMEM","EMATOCHIM","ECG","NEUROLATIP","NEUROLTIP","NEUROLATIVSTI","ASSDOM","ASSRES","ASSREDEM","ASSSEMIRES","ASSSEMIREDEM")
 )

 indicatori_global<-list(
  c("CREATININ","PROFLIP","ECG","MICROALB","GLICEM"),
  c("EMOGLIC","PROFLIP","MICROALB","FUNDOC","STATINE","CREATININ"),
  c("PROFLIP","GLICEM","ANTIAGGREG","BETABLOC","ACEINIB","STATINE","ECG","ERGOMETR"),
  c("CREATNAK","ACEINIB","ECG","BETABLOC","NAKDIUR","WALKTEST"),
  c("VISNEURO","TESTMEM","EMATOCHIM","ECG")
 )

 if (language=="italian") {

  list_patologie_names<-c("Ipertensione","Diabete","Cardiopatia Ischemica","Scompenso Cardiaco","Demenza")

  list_patologie_algo<-c("ESENZIONI: presenza di uno dei codici 000,401, 402, 403, 404, 405",
                         "ESENZIONI: presenza di codice 250; RICOVERI: presenza in una qualsiasi delle diagnosi di dimissione di un codice ICD9CM 250*; FARMACI: presenza di almeno due prescrizioni in date distinte in uno stesso anno con un codice ATC A10*",
                         "ESENZIONI: presenza di codice 414; RICOVERI: presenza in una qualsiasi delle diagnosi di dimissione di un codice ICD9CM 410*-414*; FARMACI: presenza di almeno due prescrizioni in date distinte in uno stesso anno con un codice ATC C01DA* (nitrati)",
                         "ESENZIONI: presenza di codice 428; RICOVERI: presenza in una qualsiasi delle diagnosi di dimissione, di uno dei seguenti codici: 428*, 3981, 40201, 40211, 40291, 40401, 40403, 40411, 40413, 40491, 40493",
                         "FARMACI: soggetto con almeno due prescrizioni di anticolinesterasici (ATC N06DA) in un qualsiasi anno precedente distanziate almeno 6 mesi l’una dall’altra ed almeno una prescrizione di anticolinesterasici nell’anno precedente")

  list_costi_names<-c("Assistenza Specialistica",
                      "Diagnostica Immagini",
                      "Diagnostica Laboratorio",
                      "Farmaci",
                      "Ricoveri")

  list_costi_names_reduced<-c("Specialistica",
                              "Diagn. Immagine",
                              "Diagn. Lab.",
                              "Farmaci",
                              "Ricoveri")

  indicatori_desc<-list(
   c("Pazienti con almeno una misurazione della creatinina o clearance creatinina nell'anno",
     "Pazienti con almeno una misurazione del profilo lipidico (col.tot.,HDL,LDL,trig.) nell'anno",
     "Pazienti con almeno una valutazione ECG all’anno",
     "Pazienti con almeno un monitoraggio della microalbuminuria nell’anno",
     "Pazienti con almeno una misurazione della glicemia o dell’emoglobina glicata nell’anno"),
   c("Pazienti che hanno eseguito almeno 2 test per il dosaggio dell’HbA1c nell’anno",
     "Pazienti con almeno una valutazione del profilo lipidico nell’anno",
     "Pazienti con almeno un monitoraggio della microalbuminuria nell’anno",
     "Pazienti con almeno un esame dell'occhio nell’anno (per lo stadio 1a, da effettuare ogni 2 anni)",
     "Pazienti in trattamento con statine",
     "Pazienti con almeno un test del filtrato glomerulare o creatinina o clearance creatinina nell'anno"),
   c("Pazienti con almeno una valutazione del profilo lipidico nell'anno",
     "Pazienti con una misurazione della glicemia o dell’emoglobina glicata nell'anno",
     "Pazienti in trattamento con aspirina o terapia alternativa con anticoagulanti o antiaggreganti",
     "Pazienti in trattamento con betabloccanti",
     "Pazienti con pregresso IMA in terapia con ACE inibitori o sartani",
     "Pazienti con pregresso IMA in trattamento con statine",
     "Pazienti con almeno un ECG all’anno",
     "Pazienti con un test ergometrico ogni 2 anni"),
   c("Pazienti con almeno un monitoraggio di creatinina o clearance creatinina, Na e K nell'anno",
     "Pazienti in trattamento con ACE inibitori o sartani",
     "Pazienti con valutazione attraverso esame ecocardiografico ogni 2 anni",
     "Pazienti in trattamento con betabloccanti",
     "Pazienti in trattamento con diuretici con almeno un dosaggio elettroliti (Na/K) negli ultimi 6 mesi",
     "Pazienti che hanno eseguito un '6 Minute Walking Test' al follow up"),
   c("Pazienti con almeno una visita neurologica o geriatrica nell’ultimo anno",
     "Pazienti con almeno un esame memoria multi test (codpres Z00211) nell’ultimo anno",
     "Pazienti con esami ematochimici (glicemia, creatinina, transaminasi e elettroliti) nell’ultimo anno",
     "Pazienti con almeno un ECG nell'ultimo anno",
     "Pazienti con neurolettici atipici (quetiapina, olanzapina, rispiridone)",
     "Pazienti in terapia con neurolettici tipici (aloperidolo, promazapina)",
     "Pazienti in trattamento con neurolettici a cui sono stati prescritti i neurolettici atipici",
     "Pazienti presi in carico in percorsi di assistenza domiciliare",
     "Pazienti presi in carico in percorsi di assistenza residenziale",
     "Pazienti presi in carico in percorsi di assistenza residenziale con specifica per demenza",
     "Pazienti presi in carico in percorsi di assistenza semiresidenziale",
     "Pazienti presi in carico in percorsi di assistenza semiresidenziale specifica per demenza")
  )

  indicatori_desc_short<-list(
   c("Creatinina",
     "Profilo lipidico",
     "ECG",
     "Microalbuminuria",
     "Glicemia o HbA1c"),
   c("HbA1c",
     "Profilo lipidico",
     "Microalbuminuria",
     "Fundus Oculi",
     "Statine",
     "Creatinina"),
   c("Profilo lipidico",
     "Glicemia o HbA1c",
     "Aspirina o anticoag./antiagg.",
     "Betabloccanti",
     "ACE inibitori o sartani",
     "Statine in post-IMA",
     "ECG",
     "Test ergometrico"),
   c("Creatinina",
     "ACE inibitori o sartani",
     "Ecocardiogramma",
     "Betabloccanti",
     "Na/K in trattamento con diuretici",
     "Walking Test"),
   c("Visita neurologica o geriatrica",
     "Esame memoria multi test",
     "Esami ematochimici",
     "ECG",
     "Neurolettici atipici",
     "Neurolettici tipici",
     "Neurolettici+Atipici",
     "Ass. Domiciliare",
     "Ass. Residenziale",
     "Ass. Residenziale specifica",
     "Ass. Semiresidenziale",
     "Ass. Semiresidenziale specifica")
  )

 } else if (language=="english") {

  list_patologie_names<-c("Hypertension","Diabetes","Ischaemic heart disease","Heart failure","Dementia")

  list_patologie_algo<-c("EXEMPTIONS: presence of any code equal to 000,401, 402, 403, 404, 405",
                         "EXEMPTIONS: presence of any code 250; DISCHARGES: presence of any diagnosis code ICD9CM 250*; DRUGS: presence of at least two prescriptions at different dates in the same year with an ATC code A10*",
                         "EXEMPTIONS: presence of any code 414; DISCHARGES: presence of any diagnosis code ICD9CM 410*-414*; DRUGS: presence of at least two prescription at different dates in the same year with an ATC code C01DA* (nitrates)",
                         "EXEMPTIONS: presence of any code 428; DISCHARGES: presence of any diagnosis code ICD9CM 428*, 3981, 40201, 40211, 40291, 40401, 40403, 40411, 40413, 40491, 40493",
                         "DRUGS: subject with at least two prescriptions of anticolinesterasics (ATC N06DA) in any previous year with a time lag of 6 months and at least one prescription of anticolinesterasics in the previous year")

  list_costi_names<-c("Expenditure for specialist services",
                      "Expenditure for diagnostic imaging",
                      "Expenditure for diagnostic laboratory",
                      "Pharmaceutical Expenditure",
                      "Discharges")

  list_costi_names_reduced<-c("Specialist Visits",
                              "Diagnostic Imaging",
                              "Diagnostic Tests",
                              "Prescriptions",
                              "Discharges")

  indicatori_desc<-list(
   c("Patients with at least one test of creatinine or clearance creatinine during the last 12 months",
     "Patients with at least one lipid profile measurement (tot.chol.,HDL,LDL,trig.) during the last 12 months",
     "Patients with at least one ECG measurement during the last 12 months",
     "Patients with at least one test of microalbuminuria during the last 12 months",
     "Patients with at least one glycaemic or HbA1c test during the last 12 months"),
   c("Patients with at least two HbA1c measurement during the last 12 months",
     "Patients with at least one lipid profile measurement during the last 12 months",
     "Patients with at least one test of microalbuminuria during the last 12 months",
     "Patients with at least one eye examination in the same year(for stadium 1a, due every two years)",
     "Pazients under treatment with statins",
     "Patients with at least one glomerular filter or creatinine or clearance creatinine measurement during the last 12 months"),
   c("Patients with at least one lipid profile measurement during the last 12 months",
     "Patients with at least one glycaemic or HbA1c test during the last 12 months",
     "Pazients under treatment with aspirin or alternative therapy with anticoagulants or antiaggregants",
     "Patients under treatment with beta-blockers",
     "Patients with previous AMI under treatment with ACE inhibitors or sartans",
     "Patients with previous AMI under treatment with statins",
     "Patients with at least one ECG measurement during the last 12 months",
     "Patients with an ergometric test every two years"),
   c("Patients with at least one test of creatinine or clearance creatinina, Na/K during the last 12 months",
     "Patients under treatment with ACE inhibitors or sartans",
     "Patients with an ECG measurement every two years",
     "Patients under treatment with betablockers",
     "Patients under treatment with diuretics with at least one dosage of electrolytes (Na/K) during the last 6 months",
     "Patients with a '6 Minute Walking Test' at follow up"),
   c("Patients with at least one neurologic or geriatric visit during the last 12 months",
     "Patients with at least one multi test memory examination (codpres Z00211) during the last 12 months",
     "Patients with blood tests (glycaemic level, creatinine, transaminasis and electrolites) during the last 12 months",
     "Patients with at least one ECG during the last 12 months",
     "Patients under treatment with atypical neurolectics (quetiapina, olanzapina, rispiridone)",
     "Pazients under treatment with typical neurolectics (aloperidolo, promazapina)",
     "Pazients under treatment with neurolectics with a prescription of atypical neurolectics",
     "Pazients assigned to home care",
     "Patients assigned to community care",
     "Patients assigned to community care (assistenza semiresidenziale) specific for dementia",
     "Patients assigned to outpatient community care (assistenza semiresidenziale)",
     "Patients assigned to outpatient community care (assistenza semiresidenziale) specific for dementia")
  )

  indicatori_desc_short<-list(
   c("Creatinine",
     "Lipid Profile",
     "ECG",
     "Microalbuminuria",
     "Glycemic level / HbA1c"),
   c("HbA1c",
     "Lipid Profile",
     "Microalbuminuria",
     "Fundus Oculi",
     "Statins",
     "Creatinine"),
   c("Lipid Profile",
     "Glycemic level /HbA1c",
     "Aspirine anticoag./antiagg.",
     "Betablockers",
     "ACE inhibitors / sartans",
     "Statins post-AMI",
     "ECG",
     "Ergometric Test"),
   c("Creatinine",
     "ACE inhibitors / sartans",
     "ECG",
     "Betablockers",
     "Diuretics",
     "Walking Test"),
   c("Neurologic/Geriatric Visit",
     "Memory exam multi test",
     "Blood tests",
     "ECG",
     "Atypical Neurolectics",
     "Typical Neurolectics",
     "Neurolectics+Atypical",
     "Home Care",
     "Nursing Home",
     "Specific Nursing Home",
     "Other Nursing Home",
     "Specific Other Nursing Home")
    )

 }

 ############### ulteriori variabili necessarie per parametri matrice

 list_numvars<-c()
 for (j in 1:length(list_patologie)) {
  for (k in 1:length(indicatori[[j]])) {
  list_numvars<-c(list_numvars,paste("NUM_",indicatori[[j]][k],"_",list_patologie[j],sep=""),paste("DEN_",indicatori[[j]][k],"_",list_patologie[j],sep=""))
  }
 }

 list_percent<-c(list_patologie,"GLOBAL")
 list_percent_names<-c(list_patologie,"GLOBAL")

 list_highscore<-c()
 for (j in 1:length(list_percent)) {
  list_highscore<-c(list_highscore,paste("NUM_HIGHSCORE_",list_percent[j],sep=""),paste("DEN_HIGHSCORE_",list_percent[j],sep=""))
 }

 list_fonti_vars<-c()
 list_fonti<-c()

 for (i in 1:length(list_patologie)) {
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_1",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_2",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_3",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_12",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_13",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_23",sep=""))
  list_fonti_vars<-c(list_fonti_vars,paste("FONTI_",list_patologie[i],"_123",sep=""))
  list_fonti_vars<-c(list_fonti_vars,"FONTI_HOSP_ALLPAT","FONTI_EXE_ALLPAT","FONTI_DRUG_ALLPAT")
  list_fonti<-c(list_fonti,paste("FONTI_",list_patologie[i],sep=""))
 }

 ############### assign necessari

 assign("bigmark",bigmark,envir=.GlobalEnv)
 assign("decimalmark",decimalmark,envir=.GlobalEnv)

 assign("pop_levels",pop_levels,envir=.GlobalEnv)
 assign("local_system_levels",local_system_levels,envir=.GlobalEnv)
 assign("global_system_levels",global_system_levels,envir=.GlobalEnv)
 assign("adjusters",adjusters,envir=.GlobalEnv)
 assign("list_merge",list_merge,envir=.GlobalEnv)
 assign("funnel_unit",funnel_unit,envir=.GlobalEnv)
 assign("dot_size",dot_size,envir=.GlobalEnv)
 assign("funnel_unit_name",funnel_unit_name,envir=.GlobalEnv)
 assign("target",target,envir=.GlobalEnv)
 assign("target_name",target_name,envir=.GlobalEnv)
 assign("list_patologie",list_patologie,envir=.GlobalEnv)
 assign("list_patologie_names",list_patologie_names,envir=.GlobalEnv)
 assign("list_patologie_algo",list_patologie_algo,envir=.GlobalEnv)
 assign("list_ricoveri",list_ricoveri,envir=.GlobalEnv)
 assign("list_costi",list_costi,envir=.GlobalEnv)
 assign("list_costi_2",list_costi_2,envir=.GlobalEnv)
 assign("list_costi_ext",list_costi_ext,envir=.GlobalEnv)
 assign("list_costi_ext_2",list_costi_ext_2,envir=.GlobalEnv)
 assign("list_costi_names",list_costi_names,envir=.GlobalEnv)
 assign("list_percent",list_percent,envir=.GlobalEnv)
 assign("list_percent_names",list_percent_names,envir=.GlobalEnv)
 assign("list_highscore",list_highscore,envir=.GlobalEnv)
 assign("list_fonti",list_fonti,envir=.GlobalEnv)
 assign("list_fonti_vars",list_fonti_vars,envir=.GlobalEnv)
 assign("list_costi_names_reduced",list_costi_names_reduced,envir=.GlobalEnv)
 assign("indicatori",indicatori,envir=.GlobalEnv)
 assign("indicatori_global",indicatori_global,envir=.GlobalEnv)
 assign("indicatori_desc",indicatori_desc,envir=.GlobalEnv)
 assign("indicatori_desc_short",indicatori_desc_short,envir=.GlobalEnv)

 assign("PkgsNeed",PkgsNeed,envir=.GlobalEnv)
 assign("list_numvars",list_numvars,envir=.GlobalEnv)

}

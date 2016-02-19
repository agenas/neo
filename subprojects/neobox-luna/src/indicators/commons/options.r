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

liste_opzioni <- function(installed=1) {

 if (language=="it") {
  language<-"italian"
 } else if (language=="en") {
  language<-"english"
 }

 # Print all warnings
 options(warn=1)

 # Suppress scientific notation
 options(scipen=999)

 PkgsNeed <- c("VennDiagram","gridExtra","gdata","vcd","scales","gtable","ggplot2","plyr","chron","data.table","utils")

 for (i in 1:length(PkgsNeed)) {
   if (installed == 1) {
    suppressMessages(library(PkgsNeed[i],character.only=TRUE))
   }
 }

 bigmark="."
 decimalmark=","
  
 global_system_levels<-c("liv3_regione")
 pop_levels<-c("liv2_asl")
 local_system_levels<-c("liv1_zona")
 adjusters<-c("genere")

 list_merge=c(pop_levels,local_system_levels,adjusters)
 list_criteri<-c("2.1 Accessibilita'","2.2 Continuita' Assistenziale","2.3 Tempestivita'","2.4 Efficacia","2.5 Appropriatezza")

 indicatori<-list(
  c("i1a","i1b","i1c","i2a","i2b"),
  c("i3a","i3b"),
  c("i4a","i4b","i4c"),
  c("i5a","i5b","i6a","i6b"),
  c("i7a","i7b","i8a","i8b","i9a","i9b","i10a","i10b","i11a","i11b","i11c","i11d","i11e")
 )

 list_numvars<-c()
 for (j in 1:length(list_criteri)) {
  for (k in 1:length(indicatori[[j]])) {
  list_numvars<-c(list_numvars,paste(toupper(indicatori[[j]][k]),"_NUM",sep=""),paste(toupper(indicatori[[j]][k]),"_DEN",sep=""))
  }
 }

 indicatori_explanatory_vars<-c("significato","numeratore","denominatore","formula","stratificazione","standardizzazione","fonti","note")

 if (language=="italian") {

  indicatori_desc<-list(
   c("Percentuale di anziani non autosufficienti in carico in assistenza domiciliare integrata sul totale di anziani non autosufficienti attesi nel semestre",
     "Percentuale di anziani non autosufficienti in carico in residenziale/semiresidenziale sul totale di anziani non autosufficienti attesi nel semestre",
     "Percentuale di anziani non autosufficienti in carico sul totale di anziani non autosufficienti attesi nel semestre",
     "Rapporto (%) tra numero di anziani non autosufficienti deceduti per causa non oncologica durante la presa in carico in assistenza domiciliare integrata e totale anziani deceduti per causa non oncologica nel semestre",
     "Rapporto (%) tra numero di anziani non autosufficienti deceduti per causa non oncologica durante la presa in carico in assistenza domiciliare integrata e totale anziani deceduti per causa non oncologica nel semestre"),
   c("Percentuale di dimessi da ospedale (AFO medica, degenza 7+ gg, regime ordinario) che viene presa in carico in assistenza semiresidenziale o domiciliare integrata entro 7gg dalla dimissione, nel semestre",
     "Percentuale di presi in carico in regime semiresidenziale o domiciliare integrata dimessi nei 7gg precedenti alla data di erogazione dell’assistenza, nel semestre"),
   c("Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza domiciliare integrata, nel semestre",
     "Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza residenziale, nel semestre",
     "Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza semiresidenziale, nel semestre"),
   c("Tasso di ospedalizzazione degli anziani non autosufficienti in assistenza domiciliare integrata, nel semestre",
     "Tasso di ospedalizzazione degli anziani non autosufficienti in assistenza residenziale, nel semestre",
     "Tasso di accesso al Pronto Soccorso con codice triage bianco e verde degli anziani non autosufficienti in assistenza domiciliare integrata, nel semestre",
     "Tasso di accesso al Pronto Soccorso con codice triage bianco e verde degli anziani non autosufficienti in assistenza residenziale, nel semestre"),
   c("Media delle giornate di degenza ospedaliera nell’ultimo mese di vita di anziani non autosufficienti in assistenza domiciliare integrata nel semestre",
     "Percentuale di decessi avvenuti in ospedale tra gli anziani deceduti durante la presa in carico in assistenza domiciliare integrata nel semestre",
     "Percentuale di presi in carico in regime residenziale (R1/R2(d)/R3), entro i 3 mesi dalla valutazione, con compromissione dell’autonomia grave e supporto sociale scarso/assente, nel semestre",
     "Percentuale di anziani non autosufficienti con compromissione dell’autonomia grave e supporto sociale scarso/assente che accede al regime residenziale (R1/R2(d)/R3), entro 3 mesi dalla data di valutazione, nel semestre",
     "Percentuale di anziani non autosufficienti presi in carico in regime residenziale per demenza che ha un deficit cognitivo moderato/grave e disturbi del comportamento presenti",
     "Percentuale di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti che accede al regime assistenziale residenziale per demenza (R2d) rispetto ad altre tipologie di residenzialit `a (R1/R2/R3)",
     "Percentuale di anziani non autosufficienti presi in carico in regime semiresidenziale per demenza che ha un deficit cognitivo moderato/grave e disturbi del comportamento presenti",
     "Percentuale di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti che accede al regime semiresidenziale per demenza senile (SR2)",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte dell’infermiere tra gli anziani non autosufficienti in regime di domiciliare integrata nel semestre, per intensita' del bisogno sanitario",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del MMG tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per complessita' clinica",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del OSS/OTA tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per livello di autonomia",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del Medico palliativista tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per complessita' assistenziale",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del fisioterapista tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per bisogno riabilitativo")
  )

  indicatori_explanatory<-c("Significato","Numeratore","Denominatore","Formula","Stratificazione","Standardizzazione","Fonti","Note")

  indicatori_significato<-list(
   c("Indica il livello di copertura del bisogno di assistenza sociosanitaria della popolazione anziana non autosufficiente in termini di assistenza domiciliare erogata.",
     "Indica il livello di copertura del bisogno di assistenza sociosanitaria della popolazione anziana non autosufficiente in termini di assistenza residenziale/semiresidenziale erogata.",
     "Indica il livello di copertura del bisogno di assistenza sociosanitaria in termini di assistenza domiciliare diretta, residenziale e semiresidenziale, erogata alla popolazione anziana non autosufficiente.",
     "Esprime la percentuale di pazienti malati oncologici presi in carico nella fase di fine vita a domicilio.",
     "Esprime la percentuale di pazienti non oncologici presi in carico nella fase di fine vita a domicilio."),
   c("Rileva quanti anziani non autosufficienti, che provengono dall’ospedale e necessitano di una presa in carico nel territorio, sono presi in carico tempestivamente. Valori elevati indicano una capacita' di gestire l’assistenza in continuita' tra ospedale e territorio.",
     "Rileva quanti anziani non autosufficienti in regime semiresidenziale o domiciliare integrata sono stati dimessi dall’ospedale ed assistiti tempestivamente."),
   c("Esprime la tempestivita' dell’erogazione dell’assistenza domiciliare a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia.",
     "Esprime la tempestivita' dell’erogazione dell’assistenza residenziale a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia.",
     "Esprime la tempestivita' dell’erogazione dell’assistenza semiresidenziale a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia."),
   c("Misura del ricorso al ricovero in ospedale (area medica) da parte degli anziani non autosufficienti in assistenza domiciliare integrata, tenuto conto della et `a e complessit`a del bisogno.",
     "E’ una misura del ricorso al ricovero in ospedale (area medica) da parte degli anziani non autosufficienti in strutture residenziali tenuto conto della et `a e complessit`a del bisogno.",
     "Misura l’utilizzo inappropriato del Pronto Soccorso da parte degli anziani non autosufficienti in assistenza domiciliare integrata. Ci`o pu`o essere conseguente ad una carenza dei servizi assistenziali domiciliari erogati.",
     "Misura l’utilizzo inappropriato del Pronto Soccorso da parte degli anziani non autosufficienti in assistenza residenziale. Ci`o pu`o essere conseguente ad una carenza dei servizi assistenziali residenziali erogati."),
   c("Esprime l’utilizzo dell’ospedale in termini di gg di degenza da parte di anziani non autosufficienti negli ultimi 30 gg di vita, in carico in domiciliare.",
     "Indica la proporzione di morti in ospedale tra gli anziani che sono deceduti nel periodo di presa in carico in assistenza domiciliare integrata.",
     "Esprime l’appropriatezza dell’utilizzo della tipologia di assistenza territoriale R1/R2(d)/R3 fornita. Valori elevati indicano che le strutture R1/R2(d)/R3 ospitano i pazienti elegibili per il regime residenziale.",
     "Esprime la proporzione di anziani non autosufficienti elegibili per l’assistenza residenziale (R1/R2/R3) che riceve questa tipologia di assistenza piuttosto che quella domiciliare. Valori elevati indicano coerenza tra l’esito della valutazione multidimensionale ed il regime di presa in carico.",
     "Indica, tra gli anziani non autosufficienti ospiti nelle strutture residenziali per dementi (R2d), coloro assistiti appropriatamente, in quanto teoricamente elegibili per questo percorso assistenziale (R2d) tenuto conto della tempestivit `a della presa in carico. Valori elevati indicano che l’offerta assistenziale in strutture per dementi (R2d) `e erogata ai soggetti che necessitano di questa tipologia di assistenza.",
     "Indica la percentuale di anziani non autosufficienti affetti da disturbi del comportamento e deficit cognitivi che sono ospiti in strutture residenziali dedicate a soggetti con disturbi del comportamento e deficit cognitivi (R2d), tenuto anche conto della tempestivita' dell’erogazione del servizio. Valori elevati indicano che molti soggetti affetti da disturbi del comportamento e deficit cognitivi ricevono un’offerta assistenziale in strutture che svolgono attivita' a loro piu' indicate (R2d).",
     "Indica la proporzione di anziani non autosufficienti con deficit cognitivo e disturbi del comportamento che sono ospiti nelle strutture semiresidenziali per dementi (SR2) ossia in strutture che erogano assistenza pi `u appropriata alle loro necessita', tenuto conto della tempestivita' della presa in carico. Valori elevati indicano che l’offerta assistenziale in centri diurni per dementi (SR2) e' erogata ai soggetti che necessitano questa tipologia di assistenza.",
     "Indica la percentuale di anziani non autosufficienti teoricamente elegibili per centri diurni specializzati per le demenze senili (SR2), effettivamente ospiti in queste strutture piuttosto che in altra tipologia di assistenza semiresidenziale (SR1), tenuto conto della tempestivita' dell’erogazione del servizio. Valori elevati indicano che molti anziani non autosufficienti, affetti da disturbi del comportamento e deficit cognitivi, ricevono un’offerta assistenziale in centri diurni a loro dedicati (SR2).",
     "Rileva l’impiego di personale infermieristico nei servizi di assistenza domiciliare integrata a seconda del bisogno sanitario, a parit `a di supporto socio-familiare e autonomia dell’anziano non autosufficiente in carico. All’aumentare del bisogno dovrebbe corrispondere un aumento delle giornate con almeno un accesso dell’infermiere.",
     "Rileva l’impiego del MMG nei servizi di assistenza domiciliare integrata a seconda del livello di intensit `a assistenziale (determinato dall’indice di Charlson), a parit `a di deficit cognitivo e supporto sociale dell’anziano non autosufficiente in carico.",
     "Rileva l’impiego di personale OSS/OTA in assistenza domiciliare integrata a seconda della compromissione nell’autonomia, a parit `a di supporto socio familiare e di presenza di disturbi del comportamento dell’anziano non autosufficiente in carico. All’aumentare del bisogno dovrebbe corrispondere un aumento delle giornate in cui `e avvenuto almeno un accesso dell’OSS/OTA",
     "Rileva l’impiego del medico palliativista nei servizi di assistenza domiciliare integrata a seconda del livello di intensit `a assistenziale (determinata dall’indice di Charlson) e a parita' di deficit cognitivo e supporto sociale dell’anziano non autosufficiente in carico.",
     "Rileva l’impiego di fisioterapisti nei servizi di assistenza domiciliare integrata a seconda del bisogno di assistenza riabilitativa a parit `a di deficit cognitivo e mobilit `a dell’anziano non autosufficiente in carico.")
  )

  indicatori_numeratore<-list(
   c("Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza domiciliare integrata nel semestre.",
     "Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza residenziale/semiresidenziale nel semestre.",
     "Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza residenziale/semiresidenziale o domiciliare integrata nel semestre.",
     "Totale numero di anziani in fine vita oncologici deceduti da assistenza domiciliare integrata nel semestre.",
     "Numero di anziani in fine vita non oncologici deceduti in assistenza domiciliare integrata nel semestre."),
   c("Dimessi vivi da ospedale nel semestre, elegibili per la presa in carico da parte del servizi territoriali, presi in carico (semiresidenziale o domiciliare integrata) con prestazione erogata entro 7gg dalla data di dimissione ospedaliera.",
     "Presi in carico (in regime semiresidenziale o domiciliare integrata) nel semestre, dimessi dall’ospedale (per ricovero ordinario AFO medica di 7+ gg) entro i 7gg precedenti alla data di erogazione."),
   c("Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione del servizio.",
     "Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione del servizio.",
     "Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione delservizio."),
   c("Numero di ricoveri ordinari in regime per acuti (area medica) effettuati da anziani non autosufficienti in assistenza domiciliare integrata nel semestre durante la presa in carico.",
     "Numero di ricoveri ordinari in regime per acuti (area medica) effettuati da anziani non autosufficienti in assistenza residenziale nel semestre durante la presa in carico.",
     "Numero di accessi al PS con codice triage in uscita bianco/verde (a cui non segue ricovero) effettuati da anziani non autosufficienti in assistenza domiciliare integrata nel semestre durante la presa in carico.",
     "Numero di accessi al PS con codice triage in uscita bianco/verde (a cui non segue ricovero) effettuati da anziani non autosufficienti in assistenza residenziale nel semestre durante la presa in carico."),
   c("Valore Giornate di degenza ospedaliera effettuate durante gli ultimi 30 giorni di assistenza dagli anziani non autosufficienti dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani che sono deceduti in ospedale tra i dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani non autosufficienti presi in carico in regime residenziale (R1/R2(d)/R3), entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale temporaneo/assente, nel semestre.",
     "Numero di anziani non autosufficienti presi in carico in regime residenziale (R1/R2(d)/R3) nel semestre, entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale temporaneo/assente.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza (R2d) entro 3 mesi dalla data di valutazione, con un deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza entro 3 mesi dalla data di valutazione con deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti con un deficit cognitivo moderato/grave e disturbi del comportamento presenti, presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti (o moderati), presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte dell’infermiere, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del MMG, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte dell’OSS/OTA, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del medico palliativista, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del fisioterapista, nel semestre.")
  )

  indicatori_denominatore<-list(
   c("Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Numero di anziani deceduti per causa oncologica nella zona nel semestre (da ultimo dato mortalita' Istat disponibile).",
     "Numero di anziani deceduti per causa non oncologica nella zona nel semestre (da ultimo dato mortalita' Istat disponibile)."),
   c("Dimessi vivi da ospedale nel periodo di riferimento, elegibili per la presa in carico da parte del servizi semiresidenziali e domiciliari.",
     "Presi in carico con erogazione dell’assistenza nel periodo di riferimento (semiresidenziale o domiciliare integrata)."),
   c("Anziani non autosufficienti presi in carico con erogazione dell’assistenza domiciliare nel semestre.",
     "Anziani non autosufficienti presi in carico con erogazione dell’assistenza residenziale nel semestre.",
     "Anziani non autosufficienti presi in carico con erogazione dell’assistenza semiresidenziale nel semestre."),
   c("Anziani non autosufficienti in assistenza domiciliare integrata nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza residenziale nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza domiciliare integrata nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza residenziale nel semestre (in anni-persona)."),
   c("Popolazione target Numero di anziani dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani dimessi per decesso da assistenza domiciliare integrata nel semestre.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti presi in carico (regime residenziale/domiciliare) nel semestre, entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale scarso/assente.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza (R2d) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti presi in carico nel semestre/anno in qualsiasi tipologia di assistenza residenziale nel semestre entro 3 mesi dalla data di valutazione con deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti, presi in carico in regime semiresidenziale (SR1, SR2) nel semestre entro 3 mesi dalla data di valutazione.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.")
  )

  indicatori_formula<-list(
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore) x100",
     "(Numeratore/Denominatore)x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore) x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100"),
   c("Media",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100")
  )

  indicatori_stratificazione<-list(
   c("","","","",""),
   c("",""),
   c("","",""),
   c("","","",""),
   c("","","","","","","","","Asse sanitario (lieve-moderato, grave)",
     "Charlson index (0-1, 2+)",
     "Compromissione autonomia (lieve-moderata, grave)",
     "Charlson index (0-1, 2+)",
     "Presenza di bisogno di trattamento riabilitativo neurologico o ortopedico (si, no)")
  )

  indicatori_standardizzazione<-list(
   c("Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)"),
   c("Charlson index (0, 1+)",
     "Charlson index(0,1+)"),
   c("Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)",
     "Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)",
     "Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)"),
   c("Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)"),
   c("Classe di eta' (65-84, 85+) e Accesso del palliativista (s`ı, no) (Vedi note)",
     "Classe di eta' (65-84, 85+) e Genere (m, f)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+) (Vedi note)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Supporto sociale (presente, temporaneo/assente) e Compromissione autonomia (lieve/moderata, grave)",
     "Supporto sociale (presente, temporaneo/assente) e Deficit cognitivo (lieve, moderato/grave)",
     "Disturbi del comportamento (lievi, moderati/gravi) e Supporto sociale (presente, temporaneo/assente)",
     "Deficit cognitivo (lieve, moderato/grave) e Supporto sociale (presente, temporaneo/assente)",
     "Deficit cognitivo (lieve, moderato/grave) e Disabilit `a motoria (lieve, moderata/grave)")
  )

  indicatori_fonti<-list(
   c("SIAD ISTAT","FAR ISTAT","SIAD FAR ISTAT","SIAD ISTAT","SIAD ISTAT"),
   c("SIAD FAR SDO","SIAD FAR SDO"),
   c("SIAD","FAR","FAR"),
   c("SIAD SDO","FAR SDO","SIAD PS","FAR PS"),
   c("SIAD SDO","SIAD","FAR","SIAD FAR","FAR","FAR","FAR","FAR","SIAD","SIAD","SIAD","SIAD","SIAD")
  )

  indicatori_note<-list(
   c("","","","",""),
   c("L’elegibilita' e' definita come: ricovero ordinario in Area Funzionale Omogenea (AFO) medica, con degenza di 7+ gg. In caso di ricoveri acuti a cui faccia seguito un ricovero in riabilitazione/lungodegenza entro 0-1 giorni dalla dimissione, si considera come data di dimissione la data in cui il soggetto viene dimesso dal reparto di riabilitazione/lungodegenza.",""),
   c("Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo.",
     "Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo.",
     "Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo."),
   c("","","",""),
   c("A causa della scarsa numerosit`a non `e stato possibile standardizzare il dato, pertanto il valore dell’indicatore `e in realta' grezzo.",
     "A causa della scarsa numerosit`a non `e stato possibile standardizzare il dato, pertanto il valore dell’indicatore `e in realta' grezzo.",
     "","","","","","","","","","")
  )

 } else if (language=="english") {

  indicatori_desc<-list(
   c("Percentuale di anziani non autosufficienti in carico in assistenza domiciliare integrata sul totale di anziani non autosufficienti attesi nel semestre",
     "Percentuale di anziani non autosufficienti in carico in residenziale/semiresidenziale sul totale di anziani non autosufficienti attesi nel semestre",
     "Percentuale di anziani non autosufficienti in carico sul totale di anziani non autosufficienti attesi nel semestre",
     "Rapporto (%) tra numero di anziani non autosufficienti deceduti per causa non oncologica durante la presa in carico in assistenza domiciliare integrata e totale anziani deceduti per causa non oncologica nel semestre",
     "Rapporto (%) tra numero di anziani non autosufficienti deceduti per causa non oncologica durante la presa in carico in assistenza domiciliare integrata e totale anziani deceduti per causa non oncologica nel semestre"),
   c("Percentuale di dimessi da ospedale (AFO medica, degenza 7+ gg, regime ordinario) che viene presa in carico in assistenza semiresidenziale o domiciliare integrata entro 7gg dalla dimissione, nel semestre",
     "Percentuale di presi in carico in regime semiresidenziale o domiciliare integrata dimessi nei 7gg precedenti alla data di erogazione dell’assistenza, nel semestre"),
   c("Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza domiciliare integrata, nel semestre",
     "Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza residenziale, nel semestre",
     "Tempo intercorso tra la data di valutazione multidimensionale e la data di erogazione dell’assistenza semiresidenziale, nel semestre"),
   c("Tasso di ospedalizzazione degli anziani non autosufficienti in assistenza domiciliare integrata, nel semestre",
     "Tasso di ospedalizzazione degli anziani non autosufficienti in assistenza residenziale, nel semestre",
     "Tasso di accesso al Pronto Soccorso con codice triage bianco e verde degli anziani non autosufficienti in assistenza domiciliare integrata, nel semestre",
     "Tasso di accesso al Pronto Soccorso con codice triage bianco e verde degli anziani non autosufficienti in assistenza residenziale, nel semestre"),
   c("Media delle giornate di degenza ospedaliera nell’ultimo mese di vita di anziani non autosufficienti in assistenza domiciliare integrata nel semestre",
     "Percentuale di decessi avvenuti in ospedale tra gli anziani deceduti durante la presa in carico in assistenza domiciliare integrata nel semestre",
     "Percentuale di presi in carico in regime residenziale (R1/R2(d)/R3), entro i 3 mesi dalla valutazione, con compromissione dell’autonomia grave e supporto sociale scarso/assente, nel semestre",
     "Percentuale di anziani non autosufficienti con compromissione dell’autonomia grave e supporto sociale scarso/assente che accede al regime residenziale (R1/R2(d)/R3), entro 3 mesi dalla data di valutazione, nel semestre",
     "Percentuale di anziani non autosufficienti presi in carico in regime residenziale per demenza che ha un deficit cognitivo moderato/grave e disturbi del comportamento presenti",
     "Percentuale di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti che accede al regime assistenziale residenziale per demenza (R2d) rispetto ad altre tipologie di residenzialit `a (R1/R2/R3)",
     "Percentuale di anziani non autosufficienti presi in carico in regime semiresidenziale per demenza che ha un deficit cognitivo moderato/grave e disturbi del comportamento presenti",
     "Percentuale di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti che accede al regime semiresidenziale per demenza senile (SR2)",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte dell’infermiere tra gli anziani non autosufficienti in regime di domiciliare integrata nel semestre, per intensita' del bisogno sanitario",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del MMG tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per complessita' clinica",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del OSS/OTA tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per livello di autonomia",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del Medico palliativista tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per complessita' assistenziale",
     "Percentuale di giornate in cui e' avvenuto almeno un accesso domiciliare da parte del fisioterapista tra gli anziani non autosufficienti in regime di assistenza domiciliare integrata nel semestre, per bisogno riabilitativo")
  )

  indicatori_explanatory<-c("Significato","Numeratore","Denominatore","Formula","Stratificazione","Standardizzazione","Fonti","Note")

  indicatori_significato<-list(
   c("Indica il livello di copertura del bisogno di assistenza sociosanitaria della popolazione anziana non autosufficiente in termini di assistenza domiciliare erogata.",
     "Indica il livello di copertura del bisogno di assistenza sociosanitaria della popolazione anziana non autosufficiente in termini di assistenza residenziale/semiresidenziale erogata.",
     "Indica il livello di copertura del bisogno di assistenza sociosanitaria in termini di assistenza domiciliare diretta, residenziale e semiresidenziale, erogata alla popolazione anziana non autosufficiente.",
     "Esprime la percentuale di pazienti malati oncologici presi in carico nella fase di fine vita a domicilio.",
     "Esprime la percentuale di pazienti non oncologici presi in carico nella fase di fine vita a domicilio."),
   c("Rileva quanti anziani non autosufficienti, che provengono dall’ospedale e necessitano di una presa in carico nel territorio, sono presi in carico tempestivamente. Valori elevati indicano una capacita' di gestire l’assistenza in continuita' tra ospedale e territorio.",
     "Rileva quanti anziani non autosufficienti in regime semiresidenziale o domiciliare integrata sono stati dimessi dall’ospedale ed assistiti tempestivamente."),
   c("Esprime la tempestivita' dell’erogazione dell’assistenza domiciliare a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia.",
     "Esprime la tempestivita' dell’erogazione dell’assistenza residenziale a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia.",
     "Esprime la tempestivita' dell’erogazione dell’assistenza semiresidenziale a seguito di valutazione multidimensionale, tenuto conto della copertura sociale e del livello di autonomia."),
   c("Misura del ricorso al ricovero in ospedale (area medica) da parte degli anziani non autosufficienti in assistenza domiciliare integrata, tenuto conto della et `a e complessit`a del bisogno.",
     "E’ una misura del ricorso al ricovero in ospedale (area medica) da parte degli anziani non autosufficienti in strutture residenziali tenuto conto della et `a e complessit`a del bisogno.",
     "Misura l’utilizzo inappropriato del Pronto Soccorso da parte degli anziani non autosufficienti in assistenza domiciliare integrata. Ci`o pu`o essere conseguente ad una carenza dei servizi assistenziali domiciliari erogati.",
     "Misura l’utilizzo inappropriato del Pronto Soccorso da parte degli anziani non autosufficienti in assistenza residenziale. Ci`o pu`o essere conseguente ad una carenza dei servizi assistenziali residenziali erogati."),
   c("Esprime l’utilizzo dell’ospedale in termini di gg di degenza da parte di anziani non autosufficienti negli ultimi 30 gg di vita, in carico in domiciliare.",
     "Indica la proporzione di morti in ospedale tra gli anziani che sono deceduti nel periodo di presa in carico in assistenza domiciliare integrata.",
     "Esprime l’appropriatezza dell’utilizzo della tipologia di assistenza territoriale R1/R2(d)/R3 fornita. Valori elevati indicano che le strutture R1/R2(d)/R3 ospitano i pazienti elegibili per il regime residenziale.",
     "Esprime la proporzione di anziani non autosufficienti elegibili per l’assistenza residenziale (R1/R2/R3) che riceve questa tipologia di assistenza piuttosto che quella domiciliare. Valori elevati indicano coerenza tra l’esito della valutazione multidimensionale ed il regime di presa in carico.",
     "Indica, tra gli anziani non autosufficienti ospiti nelle strutture residenziali per dementi (R2d), coloro assistiti appropriatamente, in quanto teoricamente elegibili per questo percorso assistenziale (R2d) tenuto conto della tempestivit `a della presa in carico. Valori elevati indicano che l’offerta assistenziale in strutture per dementi (R2d) `e erogata ai soggetti che necessitano di questa tipologia di assistenza.",
     "Indica la percentuale di anziani non autosufficienti affetti da disturbi del comportamento e deficit cognitivi che sono ospiti in strutture residenziali dedicate a soggetti con disturbi del comportamento e deficit cognitivi (R2d), tenuto anche conto della tempestivita' dell’erogazione del servizio. Valori elevati indicano che molti soggetti affetti da disturbi del comportamento e deficit cognitivi ricevono un’offerta assistenziale in strutture che svolgono attivita' a loro piu' indicate (R2d).",
     "Indica la proporzione di anziani non autosufficienti con deficit cognitivo e disturbi del comportamento che sono ospiti nelle strutture semiresidenziali per dementi (SR2) ossia in strutture che erogano assistenza pi `u appropriata alle loro necessita', tenuto conto della tempestivita' della presa in carico. Valori elevati indicano che l’offerta assistenziale in centri diurni per dementi (SR2) e' erogata ai soggetti che necessitano questa tipologia di assistenza.",
     "Indica la percentuale di anziani non autosufficienti teoricamente elegibili per centri diurni specializzati per le demenze senili (SR2), effettivamente ospiti in queste strutture piuttosto che in altra tipologia di assistenza semiresidenziale (SR1), tenuto conto della tempestivita' dell’erogazione del servizio. Valori elevati indicano che molti anziani non autosufficienti, affetti da disturbi del comportamento e deficit cognitivi, ricevono un’offerta assistenziale in centri diurni a loro dedicati (SR2).",
     "Rileva l’impiego di personale infermieristico nei servizi di assistenza domiciliare integrata a seconda del bisogno sanitario, a parit `a di supporto socio-familiare e autonomia dell’anziano non autosufficiente in carico. All’aumentare del bisogno dovrebbe corrispondere un aumento delle giornate con almeno un accesso dell’infermiere.",
     "Rileva l’impiego del MMG nei servizi di assistenza domiciliare integrata a seconda del livello di intensit `a assistenziale (determinato dall’indice di Charlson), a parit `a di deficit cognitivo e supporto sociale dell’anziano non autosufficiente in carico.",
     "Rileva l’impiego di personale OSS/OTA in assistenza domiciliare integrata a seconda della compromissione nell’autonomia, a parit `a di supporto socio familiare e di presenza di disturbi del comportamento dell’anziano non autosufficiente in carico. All’aumentare del bisogno dovrebbe corrispondere un aumento delle giornate in cui `e avvenuto almeno un accesso dell’OSS/OTA",
     "Rileva l’impiego del medico palliativista nei servizi di assistenza domiciliare integrata a seconda del livello di intensit `a assistenziale (determinata dall’indice di Charlson) e a parita' di deficit cognitivo e supporto sociale dell’anziano non autosufficiente in carico.",
     "Rileva l’impiego di fisioterapisti nei servizi di assistenza domiciliare integrata a seconda del bisogno di assistenza riabilitativa a parit `a di deficit cognitivo e mobilit `a dell’anziano non autosufficiente in carico.")
  )

  indicatori_numeratore<-list(
   c("Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza domiciliare integrata nel semestre.",
     "Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza residenziale/semiresidenziale nel semestre.",
     "Numero di anziani non autosufficienti residenti nella zona che risultano in carico in assistenza residenziale/semiresidenziale o domiciliare integrata nel semestre.",
     "Totale numero di anziani in fine vita oncologici deceduti da assistenza domiciliare integrata nel semestre.",
     "Numero di anziani in fine vita non oncologici deceduti in assistenza domiciliare integrata nel semestre."),
   c("Dimessi vivi da ospedale nel semestre, elegibili per la presa in carico da parte del servizi territoriali, presi in carico (semiresidenziale o domiciliare integrata) con prestazione erogata entro 7gg dalla data di dimissione ospedaliera.",
     "Presi in carico (in regime semiresidenziale o domiciliare integrata) nel semestre, dimessi dall’ospedale (per ricovero ordinario AFO medica di 7+ gg) entro i 7gg precedenti alla data di erogazione."),
   c("Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione del servizio.",
     "Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione del servizio.",
     "Giorni trascorsi dalla data di valutazione multidimensionale alla data di erogazione delservizio."),
   c("Numero di ricoveri ordinari in regime per acuti (area medica) effettuati da anziani non autosufficienti in assistenza domiciliare integrata nel semestre durante la presa in carico.",
     "Numero di ricoveri ordinari in regime per acuti (area medica) effettuati da anziani non autosufficienti in assistenza residenziale nel semestre durante la presa in carico.",
     "Numero di accessi al PS con codice triage in uscita bianco/verde (a cui non segue ricovero) effettuati da anziani non autosufficienti in assistenza domiciliare integrata nel semestre durante la presa in carico.",
     "Numero di accessi al PS con codice triage in uscita bianco/verde (a cui non segue ricovero) effettuati da anziani non autosufficienti in assistenza residenziale nel semestre durante la presa in carico."),
   c("Valore Giornate di degenza ospedaliera effettuate durante gli ultimi 30 giorni di assistenza dagli anziani non autosufficienti dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani che sono deceduti in ospedale tra i dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani non autosufficienti presi in carico in regime residenziale (R1/R2(d)/R3), entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale temporaneo/assente, nel semestre.",
     "Numero di anziani non autosufficienti presi in carico in regime residenziale (R1/R2(d)/R3) nel semestre, entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale temporaneo/assente.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza (R2d) entro 3 mesi dalla data di valutazione, con un deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza entro 3 mesi dalla data di valutazione con deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti con un deficit cognitivo moderato/grave e disturbi del comportamento presenti, presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti (o moderati), presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte dell’infermiere, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del MMG, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte dell’OSS/OTA, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del medico palliativista, nel semestre.",
     "Numero di giornate di assistenza agli anziani non autosufficienti presi in carico, in cui `e avvenuto almeno un accesso domiciliare da parte del fisioterapista, nel semestre.")
  )

  indicatori_denominatore<-list(
   c("Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Residenti non autosufficienti ultra64enni nella zona (da popolazione ISTAT e Multiscopo ISTAT).",
     "Numero di anziani deceduti per causa oncologica nella zona nel semestre (da ultimo dato mortalita' Istat disponibile).",
     "Numero di anziani deceduti per causa non oncologica nella zona nel semestre (da ultimo dato mortalita' Istat disponibile)."),
   c("Dimessi vivi da ospedale nel periodo di riferimento, elegibili per la presa in carico da parte del servizi semiresidenziali e domiciliari.",
     "Presi in carico con erogazione dell’assistenza nel periodo di riferimento (semiresidenziale o domiciliare integrata)."),
   c("Anziani non autosufficienti presi in carico con erogazione dell’assistenza domiciliare nel semestre.",
     "Anziani non autosufficienti presi in carico con erogazione dell’assistenza residenziale nel semestre.",
     "Anziani non autosufficienti presi in carico con erogazione dell’assistenza semiresidenziale nel semestre."),
   c("Anziani non autosufficienti in assistenza domiciliare integrata nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza residenziale nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza domiciliare integrata nel semestre (in anni-persona).",
     "Anziani non autosufficienti in assistenza residenziale nel semestre (in anni-persona)."),
   c("Popolazione target Numero di anziani dimessi per decesso da assistenza domiciliare nel semestre.",
     "Numero di anziani dimessi per decesso da assistenza domiciliare integrata nel semestre.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti presi in carico (regime residenziale/domiciliare) nel semestre, entro 3 mesi dalla data di valutazione, con compromissione dell’autonomia grave e supporto sociale scarso/assente.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime residenziale per demenza (R2d) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti presi in carico nel semestre/anno in qualsiasi tipologia di assistenza residenziale nel semestre entro 3 mesi dalla data di valutazione con deficit cognitivo moderato/grave e disturbi del comportamento presenti.",
     "Numero di anziani non autosufficienti presi in carico nel semestre in regime semiresidenziale per demenza (SR2) entro 3 mesi dalla data di valutazione.",
     "Numero di anziani non autosufficienti con deficit cognitivo moderato/grave e disturbi del comportamento presenti, presi in carico in regime semiresidenziale (SR1, SR2) nel semestre entro 3 mesi dalla data di valutazione.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.",
     "Numero di giornate in regime di assistenza domiciliare integrata agli anziani non autosufficienti, nel semestre.")
  )

  indicatori_formula<-list(
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore) x100",
     "(Numeratore/Denominatore)x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore) x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100"),
   c("(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100",
     "(Numeratore/Denominatore)x100"),
   c("Media",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100",
     "Numeratore/Denominatore)x100")
  )

  indicatori_stratificazione<-list(
   c("","","","",""),
   c("",""),
   c("","",""),
   c("","","",""),
   c("","","","","","","","","Asse sanitario (lieve-moderato, grave)",
     "Charlson index (0-1, 2+)",
     "Compromissione autonomia (lieve-moderata, grave)",
     "Charlson index (0-1, 2+)",
     "Presenza di bisogno di trattamento riabilitativo neurologico o ortopedico (si, no)")
  )

  indicatori_standardizzazione<-list(
   c("Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)",
     "Classe di eta' (65-74, 75-84, 85+) e Genere (m, f)"),
   c("Charlson index (0, 1+)",
     "Charlson index(0,1+)"),
   c("Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)",
     "Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)",
     "Compromissione autonomia (lieve/moderata, grave) e Supporto sociale (presente, temporaneo/assente)"),
   c("Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)",
     "Classe di eta' (65-84, 85+) e Charlson index (0, 1+)"),
   c("Classe di eta' (65-84, 85+) e Accesso del palliativista (s`ı, no) (Vedi note)",
     "Classe di eta' (65-84, 85+) e Genere (m, f)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+) (Vedi note)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Charlson index (0, 1+)",
     "Supporto sociale (presente, temporaneo/assente) e Compromissione autonomia (lieve/moderata, grave)",
     "Supporto sociale (presente, temporaneo/assente) e Deficit cognitivo (lieve, moderato/grave)",
     "Disturbi del comportamento (lievi, moderati/gravi) e Supporto sociale (presente, temporaneo/assente)",
     "Deficit cognitivo (lieve, moderato/grave) e Supporto sociale (presente, temporaneo/assente)",
     "Deficit cognitivo (lieve, moderato/grave) e Disabilit `a motoria (lieve, moderata/grave)")
  )

  indicatori_fonti<-list(
   c("SIAD ISTAT","FAR ISTAT","SIAD FAR ISTAT","SIAD ISTAT","SIAD ISTAT"),
   c("SIAD FAR SDO","SIAD FAR SDO"),
   c("SIAD","FAR","FAR"),
   c("SIAD SDO","FAR SDO","SIAD PS","FAR PS"),
   c("SIAD SDO","SIAD","FAR","SIAD FAR","FAR","FAR","FAR","FAR","SIAD","SIAD","SIAD","SIAD","SIAD")
  )

  indicatori_note<-list(
   c("","","","",""),
   c("L’elegibilita' e' definita come: ricovero ordinario in Area Funzionale Omogenea (AFO) medica, con degenza di 7+ gg. In caso di ricoveri acuti a cui faccia seguito un ricovero in riabilitazione/lungodegenza entro 0-1 giorni dalla dimissione, si considera come data di dimissione la data in cui il soggetto viene dimesso dal reparto di riabilitazione/lungodegenza.",""),
   c("Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo.",
     "Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo.",
     "Nei casi in cui la data di erogazione sia antecedente alla data di valutazione la differenza tra le due viene posta a 0 se inferiore a 7 giorni. I casi superiori a 7 giorni non entrano nel calcolo."),
   c("","","",""),
   c("A causa della scarsa numerosit`a non `e stato possibile standardizzare il dato, pertanto il valore dell’indicatore `e in realta' grezzo.",
     "A causa della scarsa numerosit`a non `e stato possibile standardizzare il dato, pertanto il valore dell’indicatore `e in realta' grezzo.",
     "","","","","","","","","","")
  )

 }

 assign("language",language,envir=.GlobalEnv)
 assign("engine_type",engine_type,envir=.GlobalEnv)
 assign("operator",operator,envir=.GlobalEnv)
 assign("year",year,envir=.GlobalEnv)
 assign("funnel_group",funnel_group,envir=.GlobalEnv)
 assign("select_unit",select_unit,envir=.GlobalEnv)
 assign("reference",reference,envir=.GlobalEnv)
 assign("bigmark",bigmark,envir=.GlobalEnv)
 assign("decimalmark",decimalmark,envir=.GlobalEnv)
 assign("PkgsNeed",PkgsNeed,envir=.GlobalEnv)
 assign("list_criteri",list_criteri,envir=.GlobalEnv)
 assign("list_numvars",list_numvars,envir=.GlobalEnv)
 assign("indicatori",indicatori,envir=.GlobalEnv)
 assign("indicatori_desc",indicatori_desc,envir=.GlobalEnv)
 assign("indicatori_explanatory",indicatori_explanatory,envir=.GlobalEnv)
 assign("indicatori_explanatory_vars",indicatori_explanatory_vars,envir=.GlobalEnv)
 assign("indicatori_significato",indicatori_significato,envir=.GlobalEnv)
 assign("indicatori_numeratore",indicatori_numeratore,envir=.GlobalEnv)
 assign("indicatori_denominatore",indicatori_denominatore,envir=.GlobalEnv)
 assign("indicatori_formula",indicatori_formula,envir=.GlobalEnv)
 assign("indicatori_stratificazione",indicatori_stratificazione,envir=.GlobalEnv)
 assign("indicatori_standardizzazione",indicatori_standardizzazione,envir=.GlobalEnv)
 assign("indicatori_fonti",indicatori_fonti,envir=.GlobalEnv)
 assign("indicatori_note",indicatori_note,envir=.GlobalEnv)
  
}

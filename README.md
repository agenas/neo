Overview
========

Questa repository contiene il codice sorgente del software NEObox.

Queste note servono alla comprensione dell'organizzazione del progetto.

Organizzazione generale
=======================

La struttura delle directory è la seguente:

  NEObox project
  |
	+-- subprojects
		  |
	    |-- neobox-app                  - Sorgenti applicazione NEObox
	    |
	    +-- neobox-matrice              - Pacchetto statistico MATRICE
	    |   |
	    |   +-- src
	    |       |
	    |       +--  import             - Specifiche di import
	    |       |
	    |       +--  indicators         - Moduli statistici
	    |       |
	    |       +--  tools              - Tools interni
	    |
      +-- neobox-matrice-sample-data  - Dati di esempio utili ai test

Compilazione del progetto
=========================

L'intero progetto è basato su gradle. Non è necessario avere gradle installato per poter eseguire la compilazione del codice sorgente.

Infatti è presente un wrapper che si incarica di eseguire il download di tutte le componenti necessarie.

Per creare una distribuzione contentente:

* Un bundle per OSX con l'eseguibile di NEObox
* Un file ZIP con l'eseguibile per Windows
* Il pacchetto dei dati di prova (basato sui dati falsi)
* Il pacchetto degli indicatori

è sufficiente eseguire il comando:

    ./gradlew dist

al termine del processo tutti i file saranno disponibili nella cartella:

    build/neobox-<versione>    

Esecuzione di NEObox in modalità sviluppo
=========================================    

Per testare il funzionamento di NEObox seguire i seguenti step:

    Per Linux/OSX

    cd subprojects/neobox-app
    ./griffonw run-app

    Per Windows

    cd subprojects\neobox-app
    griffonw run-app

La prima volta il processo sarà più lento perchè saranno scaricate tutte le dipendenze necessarie; questa operazione viene eseguita solo la prima volta.  

Come parametri di input per un test dell'installazione possono essere utilizzati i seguenti valori:

    File di dati:
    <project-root>/subprojects/neobox-matrice-sample-data/src/data/reduced_set.csv
    
    File di specifica import:
    <project-root>/subprojects/neobox-matrice/src/import/import.specs
    
    Directory indicatori:
    <project-root>/subprojects/neobox-matrice/src/indicators
    
    Directory di lavoro:
    Una cartella qualsiasi con permessi di scrittura
    

Descrizione di massima del sistema
==================================

Il software NEObox consente all'utente l'esecuzione di tre attività:

* Importazione di un file dati
* Esecuzione di un set di indicatori
* Produzione di un report

La fase di produzione del report avviene automaticamente al termine dell'esecuzione degli indicatori.

Il sistema è completamente configurabile tramite i file descritti di seguito; sostanzialmente si aspetta di ricevere in input:

* Un file .csv contenete i dati da importare
* Un file .specs che descrive il file .csv da importare
* Una cartella contenente gli indicatori
* Una cartella di lavoro in cui saranno creati tutti i file generati dagli indicatori

Specifiche di importazione
==========================

Le specifiche di importazione sono rappresentate da file con estensione `.specs`.

Un file `.specs` contiene tutte le regole necessarie alla corretta importazione del file `.csv` ed è quindi ad esso strettamente legato.

Un file .specs descrive:

* Elenco delle variabili necessarie all'importazione
* Elenco dei campi da importare con relativa descrizione
* Regole per la generazione di campi calcolati

N.B.:
Solo i campi indicati nel file di specifica vengono importati dal file .csv

Esempio di variabile
--------------------

	'NOME_VAR' {
        type = "integer" // integer | decimal | string
        label = "Etichetta"
        mandatory = true // true | false
    }

Esempio di campo
----------------

	'NOMECAMPO' {
	    type = "varchar"
	    size = 8
	    label = "Etichetta"
	}

Esempio di campo calcolato
--------------------------

    'AGE_RANGE' {
        type = "varchar"
        size = 10
        value = { record, context ->
            switch (record['AGE']) {
                case { it <= 16 }:
                    "0-16"
                    break
                case { it > 16 && it <= 50 }:
                    "17-50"
                    break
                default:
                    "50+"
            }
        }
    }

Moduli statistici
=================

Gli indicatori sono costituiti da un descrittore e da uno o più file contenenti codice R.

Il descrittore definisce le regole di selezione dei dati che saranno passati al codice R.

Il moduli in linguaggio R eseguono gli algoritmi statistici che portano alla generazione del report finale.






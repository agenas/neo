indicator1 <- function(xml=1, graphs=1, output=".", append=0, verbose=1) {

  # Set the working directory
  setwd(workDir)

  # Lettura dati generati da neobox
  input_data <- read.table(paste(workDir, "/input.csv", sep=""), header=TRUE, sep=",", colClasses="character")

  # Conversione delle colonne in numerico
  input_data <- make_numeric(input_data, c("COUNT"))

  # Preparazione tabella
  table1_1 <- data.frame(matrix(ncol=6, nrow=2))
  names(table1_1) <- c("Genere", "n", "perc", "align_1", "align_2", "align_3")
  table1_1[,"align_1"] <- "right"
  table1_1[,"align_2"] <- "right"
  table1_1[,"align_3"] <- "right"

  # Inizializzazione tabella
  table1_1[,"n"]     <- "0"
  table1_1[,"perc"]  <- "-"

  # Conteggi
  totale <- sum(input_data[,"COUNT"])
  maschi <- input_data[input_data$GENERE=="1", "COUNT"]
  femmine <- input_data[input_data$GENERE=="2", "COUNT"]

  # Inserimento dei valori in tabella
  table1_1[1,"Genere"] <- "Maschi"
  table1_1[2,"Genere"] <- "Femmine"
  table1_1[1,"n"] <- format(maschi, big.mark=bigmark)
  table1_1[2,"n"] <- format(femmine, big.mark=bigmark)
  table1_1[1,"perc"] <- format(round((sum(maschi)/totale)*100, 2), decimal.mark=decimalmark, nsmall=2)
  table1_1[2,"perc"] <- format(round((sum(femmine)/totale)*100, 2), decimal.mark=decimalmark, nsmall=2)

  pdf("table1_1-graph1.pdf")
  slices <- input_data$COUNT
  lbls <- c("M", "F")
  pie(slices, labels = lbls, main = "Maschi e femmine")
  dev.off()

  # Scrittura file report.xml
  writeTable(file = "report.xml",
      data = table1_1,
      append = 0,
      vars = c("Genere", "n", "perc"),
      headlabs = c("", "N", "%"),
      headwidt = c("260pt", "60pt", "60pt"),
      colalign = c("left", "right", "right"),
      headalign = c("left", "center", "center"),
      varcolalign = c("align_1", "align_2", "align_3"),
      footlabs = c("", "N", "%"),
      title = "Tabella di esempio",
      section = "Sezione 1",
      graph = "table1_1-graph1.pdf")
}

indicator2 <- function(xml=1, graphs=1, output=".", append=0, verbose=1) {

  # Set the working directory
  setwd(workDir)

  # Lettura dati generati da neobox
  input_data <- read.table(paste(workDir, "/input.csv", sep=""), header=TRUE, sep=",", colClasses="character")

  # Conversione delle colonne in numerico
  input_data <- make_numeric(input_data, c("I1A_NUM", "I1A_DEN", "NUM", "DEN", "COUNT"))

  # Ottiene tutte le zone disponibili
  zone <- levels(as.factor(input_data$LIV1_ZONA))

  # Preparazione tabella
  table2_1 <- data.frame(matrix(ncol=8, nrow=length(zone) + 2))
  names(table2_1) <- c("zona", "num", "den", "valore", "align_1", "align_2", "align_3", "align_4")
  table2_1[,"align_1"] <- "left"
  table2_1[,"align_2"] <- "right"
  table2_1[,"align_3"] <- "right"
  table2_1[,"align_4"] <- "right"

  # Inizializzazione tabella
  table2_1[,"zona"] <- "0"
  table2_1[,"num"] <- "0"
  table2_1[,"den"] <- "0"
  table2_1[,"valore"] <- "0"

  for(i in 1:length(zone)) {
    zona_name <- paste("Zona", zone[i], sep=" ")

    zona  <- input_data[input_data$LIV1_ZONA == i,]
    zona_num  <- sum(zona$NUM)
    zona_den  <- sum(zona$DEN)

    table2_1[i, "zona"] <- zona_name
    table2_1[i, "num"] <- format(zona_num, big.mark=bigmark)
    table2_1[i, "den"] <- format(zona_den, big.mark=bigmark)
    table2_1[i, "valore"] <- format(round((zona_num/zona_den) * 100, 2), decimal.mark=decimalmark, nsmall=2)
  }

  # Prende tutti i dati (solo un esempio)
  zona  <- input_data
  zona_num  <- sum(zona$NUM)
  zona_den  <- sum(zona$DEN)

  table2_1[length(zone) + 1, "zona"] <- "ASL (demo)"
  table2_1[length(zone) + 1, "num"] <- format(zona_num, big.mark=bigmark)
  table2_1[length(zone) + 1, "den"] <- format(zona_den, big.mark=bigmark)
  table2_1[length(zone) + 1, "valore"] <- format(round((zona_num/zona_den) * 100, 2), decimal.mark=decimalmark, nsmall=2)

  table2_1[length(zone) + 2, "zona"] <- "Regione (demo)"
  table2_1[length(zone) + 2, "num"] <- format(zona_num, big.mark=bigmark)
  table2_1[length(zone) + 2, "den"] <- format(zona_den, big.mark=bigmark)
  table2_1[length(zone) + 2, "valore"] <- format(round((zona_num/zona_den) * 100, 2), decimal.mark=decimalmark, nsmall=2)

  pdf("table2_1-graph1.pdf")
  boxplot(COUNT~LIV1_ZONA, data=input_data,
    main="Esempio di boxplot (dati finti)",
    xlab="Zone",
    ylab="Valori")
  dev.off()

  # Scrittura file report.xml
  writeTable(file = "report.xml",
      data = table2_1,
      append = 0,
      vars = c("zona", "num", "den", "valore"),
      headlabs = c("Zone", "Num", "Den", "Valore %"),
      headwidt = c("260pt", "60pt", "60pt", "60pt"),
      colalign = c("left", "right", "right", "right"),
      headalign = c("left", "center", "center", "center"),
      varcolalign = c("align_1", "align_2", "align_3", "align_4"),
      footlabs = c("", "Num", "Den", "Valore %"),
      title = "Tabella di esempio 2",
      section = "Sezione 2",
      graph = "table2_1-graph1.pdf")
}

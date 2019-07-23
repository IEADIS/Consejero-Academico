library(ggplot2)
library(plotly)
library(dplyr)
library(cluster)
library(webshot)
source("Analisis/R_Scripts/utils.R")

# =======================================================================================================
# ======================================= DATA ADQ ======================================================
# =======================================================================================================

NOTES = "Data/notas_clean.csv"

asig_sistemas <- c("MMIN", "LCAL ", "MDIS ", "TPRO ", "TCOM", "PIMB", "PIMO", "POOB", "MBDA", "TSOR ", "PRON ", 
                   "AREM ", "PDSW ", "ARSW ", "COSW ", "SOSW ", "ARQC", "SOPC ", "FRED ", "SEGI", "PGR1", "PGR2")

allData <- read.csv(NOTES, header = TRUE)
allData <- allData[ allData$Periodo.Academico != "2013-i", ]

# =======================================================================================================
# ========================================= PLOTS =======================================================
# =======================================================================================================


dataSis <- getFinishedInts(asig_sistemas)

dataHis <- table(dataSis$Periodo.Academico)
barplot(dataHis, main="Numero de inscritos (Graduados) por semestre")
hist(dataSis$Nota.Final, main="Frecuencia de notas (Graduados)")
boxplot(Nota.Final~Periodo.Academico, dataSis, las = 2, col = "blue", main = "Registros Materias (Graduados Sistemas)")


		  
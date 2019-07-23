library(plotly)
library(dplyr)
library(kohonen)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

# =======================================================================================================
# ==================================== SOM CALC & PLOT ==================================================
# =======================================================================================================

som_calc <- function(data){
  
}

plotSOM <- function(asig, titleG, pathFile){
  
}

# =======================================================================================================
# ==================================== ISIS DATA - SOM ==================================================
# =======================================================================================================

allData <- read.csv("notas.csv", header = TRUE)

asig <- c("PIMB", "PIMO", "POOB","MBDA", "PDSW ", "ARSW ", "COSW ", "SOSW ","TSOR ", "PRON ", "AREM ",
          "ARQC", "SOPC ", "FRED ", "SEGI")


colorBox <- c("red","red","red","red", "blue","blue","blue","blue", "sienna","sienna","sienna","darkgreen",
              "darkgreen","darkgreen","darkgreen", "yellow")

dataNotes <- getData(allData,asig)
dataNotes_n <- normalize(dataNotes)

m <- nrow(dataNotes)
n <- ceiling(5*m^0.5)
som_grid <- somgrid(xdim = 2, ydim = 2, topo = "hexagonal")
som_model <- som( dataNotes, grid = som_grid,alpha=c(0.05,0.01) )
pdf(paste("BoxPlotGroup",k,"k.pdf",sep = ""),width=6,height=4,paper='special')

summary(som_model)
plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model, type="dist.neighbours", palette.name=grey.colors)
plot(som_model, type="codes", labels = asig)
plot(som_model, type = "property", property = som_model$codes[[1]][,14], main=names(som_model$data)[4], palette.name=coolBlueHotRed)

k <- max(som_model$unit.classif)
var <- 4
for (i in 1:k){
  boxplot.matrix(dataNotes[som_model$unit.classif == i,], names = asig, las = 2, col = colorBox)
}
dev.off()

ks <- c()
promProg <- c()
promSoft <- c()
promOrg <- c()
promInf <- c()
for (i in 1:k){
  ks[i] <- mean(dataNotes[som_model$unit.classif == i,])
  promProg[i] <-  mean(dataNotes[som_model$unit.classif == i,][,1:4])
  promSoft[i] <-  mean(dataNotes[som_model$unit.classif == i,][,5:8])
  promOrg[i] <-  mean(dataNotes[som_model$unit.classif == i,][,9:11])
  promInf[i] <-  mean(dataNotes[som_model$unit.classif == i,][,12:15])
}
cbind(promProg, promSoft, promOrg, promInf, ks)
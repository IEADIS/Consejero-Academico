library(plotly)
library(plotly)
library(dplyr)
library(cluster)
library(ggplot2)
library(webshot)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

# =======================================================================================================
# ==================================== PCA CALC & PLOT ==================================================
# =======================================================================================================

pca_reduct <- function(data){
  #-------------- Compute PCA ------------------
  Sigm <- (1/length(data))*( t(data)%*%data )
  notes_svd <- svd(Sigm)
  
  #-------------- PCA Reduction ----------------
  ks <- c()
  for (i in 1:length(asig)){
    projData <- data%*%notes_svd$u[, 1:i]
    projData_rec <- projData%*%t(notes_svd$u[, 1:i])
    
    ks[i] <- calcBestK(data, projData_rec)
  }
  ks <- (1-ks)*100
  return(ks)
}

plotVarianceAndGetSVD <- function(asig, titleG, pathFile){
  dataMat <- getData(allData,asig) # FILTER BY ASIGNATURES
  dataMat_norm <- normalize(dataMat) # NOMRALIZE
  
  ks <- pca_reduct(dataMat_norm) # CALCULATE PCA
  
  dt <- data.frame(Numero_Caracteristicas = 1:length(asig), Porcentaje = ks)
  p <- plot_ly( dt, x = ~Numero_Caracteristicas, y = ~Porcentaje, type = "scatter") %>% layout(
    title = titleG)
  export(p, file = pathFile)
  return(notes_svd)
  
}

# =======================================================================================================
# ==================================== ISIS DATA - PCA ==================================================
# =======================================================================================================

allData <- read.csv(NOTES, header = TRUE, stringsAsFactors = FALSE)

#Pre-procesamiento
grad <- allData[ allData$Estado.Estudiante == "Termino estudios", ]
gradISIS <- grad[ grad$Programa.Estudiante == "INGENIERIA DE SISTEMAS", ]

gradISIS <- gradISIS[ gradISIS$Nota.Final <= 50, ]
gradISIS <- gradISIS[ gradISIS$Nota.Final >= 30, ]
gradISIS <- gradISIS[ gradISIS$Estado.Asignatura != "Cancelación", ]
gradISIS <- gradISIS[ gradISIS$Estado.Asignatura != "Retiro", ]
gradISIS <- gradISIS[ gradISIS$Area.Asignatura == "SISTEMAS", ]

#Numero total de graduados

gradId <- gradISIS$Identidad.Estudiante
test <- aggregate(gradId, by=list(gradId), FUN=max)
"Total de Graduados:"
length(test$Group.1)


# Plot PCA & SVD

"--------------Linea Software---------------------"

asig <- c("MBDA", "POOB", "PDSW ", "ARSW ", "COSW ", "SOSW ")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Linea Software", 
                                   "plots/html/img/varianza_retenida_software.png")
notes_svd

"------------Linea Organizaciones-----------------"

asig <- c("TSOR ", "PRON ", "AREM ", "SOSW ")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Linea Organizaciones", 
                                   "plots/html/img/varianza_retenida_organizaciones.png")
notes_svd

"-------------Linea Infraestructura---------------"

asig <- c("ARQC", "SOPC ", "FRED ", "SEGI")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Linea Infraestructura", 
                                   "plots/html/img/varianza_retenida_infraestructura.png")
notes_svd

"--------------- Todas - ISIS --------------------"

asig <- c("PIMB", "PIMO", "POOB", "PDSW ", "ARSW ", "COSW ", "TSOR ", "PRON ", 
          "AREM ", "SOSW ", "MBDA", "MMIN", "LCAL ", "MDIS ", "TPRO ", "TCOM",
          "ARQC", "SOPC ", "FRED ", "SEGI", "PGR1", "PGR2")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Todas las Lineas", 
                                   "plots/html/img/varianza_retenida_todas.png")
notes_svd

"----------------Linea Logica---------------------"

asig <- c("MMIN", "LCAL ", "MDIS ", "TPRO ", "TCOM")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Linea Logica", 
                                   "plots/html/img/varianza_retenida_logica.png")
notes_svd

"---------------Programacion---------------------"

asig <- c("PIMB", "PIMO", "POOB", "MBDA")
notes_svd <- plotVarianceAndGetSVD(asig, "Total Varianza Retenida Programacion", 
                                   "plots/html/img/varianza_retenida_prog_Basica.png")
notes_svd


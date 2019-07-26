library(psych)
library(caret)
library(plotly)
source("Analisis/R_Scripts/utils.R")
source("Analisis/R_Scripts/models/Regression/regression.R")

PLOTS_DIR_REG <- paste(PLOTS_DIR,"Models/Regression/",sep = "")

# =======================================================================================================
# ===================================== MODEL BENEFITS ==================================================
# =======================================================================================================

# DATA ADQUISITION FILTER
data.benefit <- function(data, asig){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO CANCELED
  data.asig <- data.asig[ data.asig$Nota.Final < 30 & data.asig$Nota.Final != 0, ] # NO CANCEL & LOOSE
  data.asig <- droplevels(data.asig) # CLEAN UNUSED FACTORS
  
  return(data.asig)
}

# MODEL BENEFIT
model.benefit <- function(data){
  # GET ALL MODELS
  models <- load.models(LM_MODELS)
  data.results <- data.frame(data, Estado.Modelo = rep("Sin Cubrir",nrow(data)),
                             RMSE = rep(NA,nrow(data)), Rsquared = rep(NA,nrow(data)))
  levels(data.results$Estado.Modelo) <- c(levels(data.results$Estado.Modelo), c("Cubierto","Beneficiado"))
  
  # MODELS FILTER - APPEARANCE ON DATA
  for (model.name in names(models)) {
    model.name.f <- gsub("lm-","",model.name)
    appears <- grep(model.name.f,data$Codigo.Asignatura) # MODEL ON ASIGNATURES
    if (length(appears) > 0) {
      # CHANGE TO COVERED
      data.results[appears,]$Estado.Modelo <- "Cubierto"
      # PREDICT
      test.data <- asig.trans(data[appears,])
      results <- lm.test(models[[model.name]],test.data[,-1])
      
      # CONDITION TO RECOMMEND CANCELLATION
      note.final <- rowMeans(data.frame(data.results[appears,]$nota1,data.results[appears,]$nota2,results$pred))
      benefited <- which(note.final <= 30)
      print(appears)
      print(summary(note.final))
      if (length(benefited) > 0) {
        data.results[appears[benefited],]$Estado.Modelo <- "Beneficiado"
      }
      data.results[appears,]$RMSE <- results$RMSE
      data.results[appears,]$Rsquared <- results$Rsquared
    }
  }
  return(data.results)
}

# PLOT BENEFIT
plot.benefit <- function(results,file_path){
  results$App <- rep(1,nrow(results))
  p <- plot_ly(results, labels = ~Estado.Modelo, values = ~App, type = 'pie') %>%
    layout(title = 'Uso de la Herramienta por Estudiante',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p, file_path)
}

# MAIN
isis.benefit <- function(){
  # DATA ADQUIRE
  allData <- read.csv(NOTES, header = TRUE)
  # LINEAR MODEL ISIS
  asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
  data.filtered <- data.benefit(allData, asig.sistemas)
  # GET BENEFITS BY MODELS
  results <- model.benefit(data.filtered)
  
  # PLOTS
  plot.benefit(results,paste(PLOTS_DIR_REG,"uso_herrmaienta.png",sep = ""))
  infoData(data.filtered,paste(PLOTS_DIR_REG,"notas.png",sep = ""))
  return(results)
}
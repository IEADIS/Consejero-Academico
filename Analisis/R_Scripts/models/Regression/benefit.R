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
data.adq <- function(data, asig){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final != 0 & data.asig$Estado.Asignatura != "Retiro", ] # NO ACCOMP
  
  data.asig <- droplevels(data.asig) # CLEAN UNUSED FACTORS
  
  return(data.asig)
}

# MODEL BENEFIT
model.benefit <- function(data, unanalyzed, wrong, right, condition.func){
  # GET ALL MODELS
  models <- load.models(LM_MODELS)
  data.results <- data.frame(data, Estado.Modelo = rep(unanalyzed,nrow(data)),
                             RMSE = rep(NA,nrow(data)), Rsquared = rep(NA,nrow(data)))
  levels(data.results$Estado.Modelo) <- c(levels(data.results$Estado.Modelo), c(wrong,right))
  
  # MODELS FILTER - APPEARANCE ON DATA
  for (model.name in names(models)) {
    model.name.f <- gsub("lm-","",model.name)
    model.asignature <- gsub("_.*","",model.name.f)
    model.years <- gsub(".*_","",model.name.f)
    model.years.start <- strtoi(gsub("-.*","",model.name.f))
    model.years.end <- strtoi(gsub(".*-","",model.name.f))
    
    appears.asig <- grep(model.asignature,data$Codigo.Asignatura) # DATA SAMPLES TO PRED BY ASIG
    appears.year <- grep(toString(model.years.end+1),data$Periodo.Academico) # DATA SAMPLES TO PRED BY NEXT YEAR
    appears.asig.year <- intersect(appears.asig,appears.year) # BY ASIG & NEXT YEAR
    
    print(paste("MODEL NAME :",model.name.f))
    
    if (length(appears.asig.year) > 0) {
      # CHANGE TO COVERED BUT SUGGEST IN A WRONG WAY
      data.results[appears.asig.year,]$Estado.Modelo <- wrong
      
      # PREDICT
      test.data <- asig.trans(data[appears.asig.year,]) # GET DATA TO PREDICT
      results <- lm.test(models[[model.name]],test.data[,-1]) # PREDICT
      
      # CONDITION TO SUGGEST RIGHT
      note.final <- rowMeans(data.frame(data.results[appears.asig.year,]$nota1,data.results[appears.asig.year,]$nota2,results$pred))
      benefited <- condition.func(note.final)
      
      print(appears.asig.year)
      print(summary(note.final))
      
      if (length(benefited) > 0) {
        data.results[appears.asig.year[benefited],]$Estado.Modelo <- right
      }
      data.results[appears.asig.year,]$RMSE <- results$RMSE
      data.results[appears.asig.year,]$Rsquared <- results$Rsquared
    }
  }
  return(data.results)
}

loose.nocancel.benefit <- function(final.note){
  return(which(final.note < 30))
}
pass.nocancel.benefit <- function(final.note){
  return(which(final.note >= 30))
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

plot.sunburst.tool <- function(){
  d <- data.frame(
    labels = c("Student","Cancelo","Perdio","Paso",
               "Sin Analizar L", "Sin Analizar P"),
    parents = c("","Student","Student","Student",
                "Perdio","Paso"),
    values = c(37,15,10,12,
               10,12),
    stringsAsFactors = FALSE
  )
  
  p <- plot_ly(d, labels = ~labels, parents = ~parents, values = ~values,
               text = ~paste(values, '%'),
               type = 'sunburst',branchvalues = 'total')
  print(p)
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
  
  data.filtered <- data.adq(allData, asig.sistemas)
  data.nocancel <- data.filtered[ data.filtered$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO NO CANCELED
  data.loose <- data.nocancel[ data.nocancel$Nota.Final < 30 & data.nocancel$Nota.Final != 0, ] # NO CANCEL & LOOSE
  data.pass <- data.nocancel[ data.nocancel$Nota.Final >= 30 & data.nocancel$Nota.Final <= 5, ] # NO CANCEL & PASS
  
  # DROPLEVELS
  data.loose <- droplevels(data.loose) # CLEAN UNUSED FACTORS
  data.pass <- droplevels(data.pass) # CLEAN UNUSED FACTORS
  
  # GET BENEFITS BY MODELS
  results.loose <- model.benefit(data.filtered,"Sin Analizar",
                           "Sugiere Continuar","Sugiere Cancelar",loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
  
  # results.pass <- model.benefit(data.filtered,"Sin Analizar",
  #                                "Sugiere Cancelar","Sugiere Continuar",pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
  
  # # PLOTS
  # plot.benefit(results,paste(PLOTS_DIR_REG,"uso_herrmaienta.png",sep = ""))
  # infoData(data.filtered,paste(PLOTS_DIR_REG,"notas.png",sep = ""))
  return(results.loose)
}
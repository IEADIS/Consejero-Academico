library(psych)
library(caret)
source("Analisis/R_Scripts/utils.R")
source("Analisis/R_Scripts/models/Classification/classification_utils.R")
source("Analisis/R_Scripts/models/Classification/logistic_regression.R")
source("Analisis/R_Scripts/models/Classification/support_vector.R")
source("Analisis/R_Scripts/models/Classification/neural_network.R")

NOTES = classif_utils.getDataCleanClassDir()

# Only in Linux/Unix Systems - in Windows doesn't work - You should use parallel instead - This library is to parallelize, you can comment it
# Take care of the number of cores
library(doMC)
registerDoMC(cores = 5)

# =======================================================================================================
# ================================= DEPLOY CLASSIFICATION MODELS ========================================
# =======================================================================================================



deploy.by.window <- function(){
  
}

deploy.classification <- function(allData, asignature, years.ini){
  
  set.seed(123)
  # ADQUISITION AND FILTERING
  data.asig <- classif_utils.asig.adq(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  data.trans <- classif_utils.asig.trans(data.asig)
  data.part <- classif_utils.asig.part(data.trans)
  
  if (nrow(data.asig[data.asig$Codigo.Asignatura %in% asig,]) < 10) {
    return(NULL)
  }
  
  # BY EACH TIME WINDOW, WE'LL GET THE BEST MODEL
  list.year <- 1
  for (year in years.ini) { # BY YEAR WINDOW
    asig.model <- deploy.by.window(data.asig,toString(year),toString(year+4))
    models.comp <- rbind(models.comp, asig.model$Data)
    models.data[[list.asig]][[list.year]] <- asig.model$Model
    names(models.data[[list.asig]])[list.year] <- year
    list.year <- list.year+1
  }
}


# =======================================================================================================
# ================================= ISIS CLASSIFICATION MODELS ==========================================
# =======================================================================================================

isis.models <- function(){
  # DATA ADQUIRE
  allData <- read.csv(NOTES, header = TRUE)
  
  # FILTER BY SUBJECT
  asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
  # DEFINE WINDOW OF YEARS
  years.ini <- c(2009,2010,2011,2012)
  
  # ASIG SISTEMAS
  models.comp <- data.frame(Asignature = c(), TimeSince = c())
  models.data <- list()
  
  list.asig <- 1
  for (asignature in asig.sistemas) { # BY ASIGNATURE
    models.data[[list.asig]] <- list()
    list.year <- 1
    
    asig.model <- deploy.classification( allData, asignature, years.ini )
    models.data[[list.asig]] <- asig.model$Model
    models.comp <- rbind(models.comp, asig.model$Data)
    names(models.data)[list.asig] <- asignature
    
    list.asig <- list.asig+1
  }
  # BEST MODEL SELECTION
  
  # best.models <- list()
  # print(unique(models.comp$Asignature))
  # for (asignature in unique(models.comp$Asignature)) {
  #   print(paste("Asignature : ",asignature))
  #   if (length(models.data[[asignature]]) == 1) {
  #     best.model <- models.data[[asignature]][[1]]
  #   } else {
  #     best.model <- model.selection.lm.sum(models.data,models.comp,asignature)}
  #   best.models[[asignature]] <- best.model
  # }
  # 
  # # SAVE MODELS
  # for (model.name in names(best.models)) {
  #   save.model.lm(best.models[[model.name]], gsub("\\s", "", model.name))
  # }
  # return(best.models)
}
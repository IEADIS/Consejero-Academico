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

get.best.model.by.f1_acc <- function( models, data.test ){
  
  bestF1 <- 0
  bestAcc <- 0
  bestModel <- NULL
  noF1 <- FALSE
  
  for ( i in 1:length(models) ){
    cm <- confusionMatrix( predict( models[[i]][[1]], data.test ), data.test$Grade3 )
    if (  noF1 || is.na( cm$byClass["F1"] )  ){
      noF1 <- TRUE
      if ( bestAcc < cm$overall["Accuracy"]*0.9 ){
        bestModel <- models[[i]]
        bestAcc <- cm$overall["Accuracy"]*0.9
        bestF1 <- cm$byClass["F1"]
        bestModel[[2]]["F1"]  <- bestF1
        bestModel[[2]]["Acc"]  <- bestAcc
      }
    } else {
      if ( bestF1 < cm$byClass["F1"] ){
        bestModel <- models[[i]]
        bestF1 <- cm$byClass["F1"]
        bestAcc <- cm$overall["Accuracy"]
        bestModel[[2]]["F1"]  <- bestF1
        bestModel[[2]]["Acc"]  <- bestAcc
      }
    }
    
  }
  return( bestModel )
}

get.best.model.trained <- function( models ){
  bestF1 <- 0
  bestAcc <- 0
  bestModel <- NULL
  noF1 <- FALSE
  
  for ( i in 1:length(models) ){
    
    currF1 <- models[[i]][[2]]["F1"]
    currAcc <- models[[i]][[2]]["Acc"]
      
    if (  noF1 || is.na( currF1 )  ){
      noF1 <- TRUE
      if ( bestAcc < currAcc*0.9 ){
        bestModel <- models[[i]]
        bestAcc <- currAcc*0.9
        bestF1 <- cm$byClass["F1"]
      }
    } else {
      if ( bestF1 < currF1 ){
        bestModel <- models[[i]]
        bestF1 <- currF1
        bestAcc <- currAcc
      }
    }
    
  }
  return( bestModel )
}

model.selection.best <- function( models, data.test ){
  if ( length(models) == 1 ){
    best.model <- models[[1]]
  } else {
    best.model <- get.best.model.by.f1_acc( models, data.test )
  }
  return( best.model )
}

deploy.by.window <- function( allData, asig, time.start, time.end ){
  
  # ADQUISITION AND FILTERING
  data.asig <- classif_utils.asig.adq(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  data.trans <- classif_utils.asig.trans(data.asig)
  data.part <- classif_utils.asig.part(data.trans)
  
  models <- list()
  models[[1]] <- list( train.glm(data.part$train), data.frame( Acc = 0, F1 = 0, Type = "LogisticRegression" ) )
  return( model.selection.best(models, data.part$test) )
  
}

deploy.classification <- function(allData, asignature, years.ini){
  
  models.comp <- list()
  
  if (nrow(allData[allData$Codigo.Asignatura %in% asignature,]) < 10) {
    return(NULL)
  }
  
  # BY EACH TIME WINDOW, WE'LL GET THE BEST MODEL
  list.year <- 1
  for (year in years.ini) { # BY YEAR WINDOW
    asig.model <- deploy.by.window(allData, asignature, toString(year), toString(year+4))
    models.comp[[list.year]] <- asig.model
    names(models.comp)[list.year] <- year
    list.year <- list.year+1
  }
  
  return( get.best.model.trained(models.comp) )
  
}


# =======================================================================================================
# ================================= ISIS CLASSIFICATION MODELS ==========================================
# =======================================================================================================

isis.models <- function(){
  
  set.seed(123)
  
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
  best.models <- list()
  
  list.asig <- 1
  for (asignature in asig.sistemas) { # BY ASIGNATURE
    write("============================ASIGNATURE============================", stdout())
    write(asignature, stdout())
    if ( asignature == "PES1" || asignature == "PES2" || asignature == "EINL" ) {next()}
    asig.model <- deploy.classification( allData, asignature, years.ini )
    if (is.null(asig.model) ) {next()}
    best.models[[list.asig]] <- asig.model
    names(best.models)[list.asig] <- asignature
    list.asig <- list.asig+1
    write("============================END============================", stdout())
    break
  }

  # SAVE MODELS
  for (model.name in names(best.models)) {
    classif_utils.save.model(best.models[[model.name]], "model-", gsub("\\s", "", model.name))
  }
  return(best.models)
}
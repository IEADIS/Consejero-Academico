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

get.best.model.by.f1_acc <- function( models ){
  bestF1 <- 0
  bestAcc <- 0
  bestModel <- NULL
  noF1 <- FALSE
  
  if ( length(models) == 0 ) { return(NULL) }
  
  for ( i in 1:length(models) ){
    
    currF1 <- models[[i]][[2]]["F1"]
    currAcc <- models[[i]][[2]]["Acc"]
      
    if (  noF1 || is.na( currF1 )  ){
      noF1 <- TRUE
      if ( bestAcc < currAcc*0.9 ){
        bestModel <- models[[i]]
        bestAcc <- currAcc*0.9
        bestF1 <- currF1
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

model.selection.best <- function( models ){
  return( get.best.model.by.f1_acc( models ) )
}

evaluate.models <- function( models, data.test ){
  
  for ( i in 1:length(models) ){
    cm <- confusionMatrix( predict( models[[i]][[1]], data.test ), data.test$Grade3 )
    models[[i]][[2]]["F1"] <- cm$byClass["F1"]
    models[[i]][[2]]["Acc"] <- cm$overall["Accuracy"]
  }
  return(models)
}

deploy.by.window <- function( allData, asig, time.start, time.end ){
  
  # ADQUISITION AND FILTERING
  data.asig <- classif_utils.asig.adq(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  data.trans <- classif_utils.asig.trans(data.asig)
  data.part <- classif_utils.asig.part(data.trans)
  
  sm <- summary( data.part$train$Grade3 )
  
  for ( i in sm ){ 
    if ( i == 0 ) {
      return(NULL)
    }
  }
  
  models <- list()
  models[[1]] <- list( train.glm(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Logistic Regression", stringsAsFactors = FALSE ) )
  models[[2]] <- list( train.svm(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Support Vector Machine", stringsAsFactors = FALSE ) )
  models[[3]] <- list( train.nn(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Neural Networks", stringsAsFactors = FALSE ) )
  
  models <- evaluate.models( models, data.part$test )
  return( model.selection.best(models) )
  
}

deploy.classification <- function(allData, asignature, years.ini){
  
  models.comp <- list()
 
  if (nrow(allData[allData$Codigo.Asignatura %in% asignature,]) < 10) {
    return(NULL)
  }
  
  # BY EACH TIME WINDOW, WE'LL GET THE BEST MODEL
  list.year <- 1
  for (year in years.ini) { # BY YEAR WINDOW
    asig.model <- deploy.by.window(allData, asignature, toString(year), toString(year+5))
    if (is.null(asig.model) ) {next()}
    models.comp[[list.year]] <- asig.model
    names(models.comp)[list.year] <- year
    list.year <- list.year+1
  }
  write("CLACULATE BEST MODELS", stdout())
  return( model.selection.best(models.comp) )
  
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
    asig.model <- deploy.classification( allData, asignature, years.ini )
    if (is.null(asig.model) ) {next()}
    best.models[[list.asig]] <- asig.model
    names(best.models)[list.asig] <- asignature
    list.asig <- list.asig+1
    write("============================END============================", stdout())
  }

  # SAVE MODELS
  for (model.name in names(best.models)) {
    classif_utils.save.model(best.models[[model.name]], "model-", gsub("\\s", "", model.name))
  }
  return(best.models)
}
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

filter.data <- function( allData, asig, time.start, time.end, filterByTime = classif_utils.asig.adq, filterTransform = classif_utils.asig.trans, filterPart = classif_utils.asig.part ){
  
  data.asig <- filterByTime(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  data.trans <- filterTransform(data.asig)
  data.part <- filterPart(data.trans)
  
  return( data.part )
}

deploy.by.window <- function( allData, asig, time.start, time.end, filterPart = classif_utils.asig.part ){
  
  # ADQUISITION AND FILTERING
  
  data.part <- filter.data(allData, asig, time.start, time.end, filterPart = filterPart )
  
  sm <- summary( data.part$train$Grade3 )
  
  for ( i in sm ){ 
    if ( i == 0 ) {
      write("================= OUT =====================", stdout())
      write( asig , stdout() )
      return(NULL)
    }
  }
  
  models <- list()
  models[[1]] <- list( train.glm(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Logistic Regression", timeWindow = c(time.start, time.end), stringsAsFactors = FALSE ) )
  models[[2]] <- list( train.svm(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Support Vector Machine", timeWindow = c(time.start, time.end), stringsAsFactors = FALSE ) )
  models[[3]] <- list( train.nn(data.part$train), data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Neural Networks", timeWindow = c(time.start, time.end), stringsAsFactors = FALSE ) )
  
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
# =================================== CALCULATE BEST METRICS ============================================
# =======================================================================================================


getBestDeltaTimeByAsig <- function(allData, init.time, final.time, asig){
  
  models <- list()
  list.model <- 1
  for ( current.time.final in final.time:(init.time-1) ){
    
    models.by.window <- list()
    list.model.window <- 1
    for ( current.time.init in (current.time.final-1):init.time ){
      write( paste( "TIME LAPSE: ", toString(current.time.init), toString(current.time.final) ), stdout() )
      asig.model <- deploy.by.window(allData, asig, current.time.init, current.time.final, filterPart = function( data.trans ){
        data.train <- data.trans[ -grepl( toString(current.time.final), data.trans$Periodo ), ] # TRAINING DATA
        data.test <- data.trans[ grepl( toString(current.time.final), data.trans$Periodo ), ] # TEST DATA
        return(list(train = na.omit(data.train), test = na.omit(data.test)))
      })
      models.by.window[[list.model.window]] <- asig.model
      list.model.window <- list.model.window + 1
    }
    models[[list.model]] <- model.selection.best(models.by.window)
    list.model <- list.model + 1
  }
  return( models )
}

getBestDeltaTime <- function(){
  
  set.seed(123)
  
  # GET DATA AND SET VARIABLES
  allData <- read.csv(NOTES, header = TRUE)
  init.time <- 2009
  final.time <- 2016
  
  # GET RELEVANT ASIGNATURES
  asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  
  models <- list()
  list.model <- 1
  
  for ( asig in asig.sistemas ){
     models[[list.model]] <- getBestDeltaTimeByAsig(allData, init.time, final.time, asig)
     list.model <- list.model + 1
     classif_utils.save.model( models, "models_list-", "all-Models-DeltaTime" )
  }
  return( models )
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
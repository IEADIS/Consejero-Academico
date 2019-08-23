library(psych)
library(caret)
source("Analisis/R_Scripts/utils.R")
source("Analisis/R_Scripts/models/Classification/classification_utils.R")
source("Analisis/R_Scripts/models/Classification/logistic_regression.R")
source("Analisis/R_Scripts/models/Classification/support_vector.R")
source("Analisis/R_Scripts/models/Classification/neural_network.R")

# Only in Linux/Unix Systems - in Windows doesn't work - You should use parallel instead - This library is to parallelize, you can comment it
# Take care of the number of cores
library(doMC)
registerDoMC(cores = 3)

# =======================================================================================================
# ================================= DEPLOY CLASSIFICATION MODELS ========================================
# =======================================================================================================

get.best.model.by.f1_acc <- function( models, getIndex  = FALSE ){
  bestF1 <- 0
  bestAcc <- 0
  bestModel <- NULL
  bestI <- -1
  noF1 <- FALSE
  index <- 1
  
  if ( length(models) == 0 ) { return(NULL) }

  for ( i in 1:length(models) ){
    if ( is.null(models[[i]]) ){next()}
    
    currF1 <- models[[i]][[2]]["F1"]
    currAcc <- models[[i]][[2]]["Acc"]
      
    if (  noF1 || is.na( currF1 )  ){
      noF1 <- TRUE
      if ( bestAcc < currAcc*0.9 ){
        bestModel <- models[[i]]
        bestAcc <- currAcc*0.9
        bestF1 <- currF1
        if (getIndex) bestModel$data$bestIndex <- i
      }
    } else {
      if ( bestF1 < currF1 ){
        bestModel <- models[[i]]
        bestF1 <- currF1
        bestAcc <- currAcc
        if (getIndex) bestModel$data$bestIndex <- i
      }
    }
    
  }
  return( bestModel )
}

model.selection.best <- function( models, getIndex  = FALSE ){
  return( get.best.model.by.f1_acc( models, getIndex  = getIndex ) )
}

evaluate.models <- function( models, data.test ){
  
  for ( i in 1:length(models) ){
    if ( nrow(data.test) == 0 ) {next()}
    test.model <<- models[[i]]$model
    test.data.test <<- data.test
    cm <- confusionMatrix( predict( models[[i]]$model, data.test ), data.test$Grade3 )
    models[[i]]$data["F1"] <- cm$byClass["F1"]
    models[[i]]$data["Acc"] <- cm$overall["Accuracy"]
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
  
  if ( nrow( classif_utils.asig.adq(allData, asig, time.end, time.end ) ) == 0 ) return(NULL);
  
  sm <- summary( data.part$train$Grade3 )
  
  for ( i in sm ){ 
    if ( i == 0 ) {
      return(NULL)
    }
  }
  
  models <- list()
  models[[1]] <- list( model = train.glm(data.part$train), data = data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Logistic Regression", timeWindowStart = time.start, timeWindowEnd = time.end, bestIndex = -1, stringsAsFactors = FALSE ) )
  models[[2]] <- list( model = train.svm(data.part$train), data = data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Support Vector Machine", timeWindowStart = time.start, timeWindowEnd = time.end, bestIndex = -1, stringsAsFactors = FALSE ) )
  models[[3]] <- list( model = train.nn(data.part$train), data = data.frame( Asignature = asig, Acc = 0, F1 = 0, Type = "Neural Networks", timeWindowStart = time.start, timeWindowEnd = time.end, bestIndex = -1, stringsAsFactors = FALSE ) )
  
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
  return( model.selection.best(models.comp) )
  
}


# =======================================================================================================
# =================================== CALCULATE BEST METRICS ============================================
# =======================================================================================================


getBestDeltaTimeByAsig <- function(allData, init.time, final.time, asig){
  
  models <- list(); allmodels <- list();
  list.model <- 1; current.time.final <- final.time;
  while ( current.time.final > init.time ){
    
    models.by.window <- list()
    list.model.window <- 1; current.time.init <- current.time.final-1
    
    while ( current.time.init >= init.time ){
      
      write( paste( "TIME LAPSE: ", toString(current.time.init), toString(current.time.final), "ASIG: ", asig ), stdout() )
      asig.model <- deploy.by.window(allData, asig, current.time.init, current.time.final, filterPart = function( data.trans ){
        test.data.trans <<- data.trans
        data.train <- data.trans[ !grepl( toString(current.time.final), data.trans$Periodo ), ] # TRAINING DATA
        data.test <- data.trans[ grepl( toString(current.time.final), data.trans$Periodo ) 
                                 & data.trans$Programa.Estudiante %in% "INGENIERIA DE SISTEMAS", ] # TEST DATA
        return(list(train = na.omit(data.train), test = na.omit(data.test)))
      })
      current.time.init <- current.time.init - 1
      if ( is.null(asig.model) ){next()}
      models.by.window[[list.model.window]] <- asig.model
      list.model.window <- list.model.window + 1
    }
    window.model <- model.selection.best(models.by.window, getIndex  = TRUE)
    current.time.final <- current.time.final - 1
    
    if ( is.null(window.model) ) {next()}
    models[[list.model]] <- window.model
    names(models)[list.model] <- current.time.final + 2
    
    models.by.window[[ models[[list.model]]$data$bestIndex ]]$data$bestIndex <- models[[list.model]]$data$bestIndex;
    
    write( (current.time.final+1), stdout() )
    
    classif_utils.plotMetricsOfModelsTablePDF(models.by.window, paste(asig, "/", (current.time.final+1), sep = "" ))
    
    allmodels <- c(allmodels, models.by.window)
    list.model <- list.model + 1
  }
  
  if ( length(allmodels) != 0 ) classif_utils.plotMetricsOfModelsTablePDF(allmodels, paste(asig, sep = "" ), name = "allModelsLambdas.html");
  if ( length(models) != 0 ) classif_utils.plotMetricsOfModelsTablePDF(models, paste(asig, sep = "" ), name = "LambdaSelection.html")
  
  return( models )
}

classif.bestModel.train <- function(allData, asignatures, omitted.years = c()){
  
  set.seed(123)
  
  # GET DATA AND SET VARIABLES
  # allData <- read.csv(NOTES, header = TRUE)
  # init.time <- 2009
  # final.time <- 2016
  # 
  # # GET RELEVANT ASIGNATURES
  # asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  # asig.sistemas <- asig.sistemas[ 24:length(asig.sistemas) ]
  years <- unique(sub("-.*","",droplevels(allData[allData$Codigo.Asignatura %in% asignatures,]$Periodo.Academico))) # ALL YEARS
  years.num <- strtoi(years) # INT VALUES OF YEARS
  years.num <- years.num[!(years.num %in% strtoi(omitted.years))]
  init.time <- min(years.num)
  final.time <- max(years.num)
  
  models <- list()
  list.model <- 1
  write("# INIT TRAIN", stdout())
  for ( asig in asignatures ){
     write(asig, stdout())
     model.asig <- getBestDeltaTimeByAsig(allData, init.time, final.time, asig)
     
     classif_utils.createClassDir( paste( getClassificationModelsDir(), getClassificationModelsDeltaDir(), "/", asig, sep = "" ) )
     
     if ( is.null(model.asig) ){next()}
     models[[list.model]] <- model.asig
     names(models)[list.model] <- asig
     for ( model in model.asig ){
       asig.f <- gsub("\\s", "", asig)
       classif_utils.save.model( model, 
            paste( "cf-", asig.f, "_", sep = "" ), 
            toString(model$data$timeWindowEnd + 1))
     }
     list.model <- list.model + 1
  }
  return( models )
}

# =======================================================================================================
# ================================= ISIS CLASSIFICATION MODELS ==========================================
# =======================================================================================================

# isis.models <- function(){
#   
#   set.seed(123)
#   
#   # DATA ADQUIRE
#   allData <- read.csv(NOTES, header = TRUE)
#   
#   # FILTER BY SUBJECT
#   asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
#   asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
#   asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
#   asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
#   asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
#   
#   # DEFINE WINDOW OF YEARS
#   years.ini <- c(2009,2010,2011,2012)
#   
#   # ASIG SISTEMAS
#   best.models <- list()
#   
#   list.asig <- 1
#   for (asignature in asig.sistemas) { # BY ASIGNATURE
#     write("============================ASIGNATURE============================", stdout())
#     write(asignature, stdout())
#     asig.model <- deploy.classification( allData, asignature, years.ini )
#     if (is.null(asig.model) ) {next()}
#     best.models[[list.asig]] <- asig.model
#     names(best.models)[list.asig] <- asignature
#     list.asig <- list.asig+1
#     write("============================END============================", stdout())
#   }
# 
#   # SAVE MODELS
#   for (model.name in names(best.models)) {
#     classif_utils.save.model(best.models[[model.name]], "model-", gsub("\\s", "", model.name))
#   }
#   return(best.models)
# }

# =======================================================================================================
# ============================== ISIS CLASSIFICATION MODELS EVALUATE ====================================
# =======================================================================================================

classif.cancel.benefit <- function(final.note){
  return(which(final.note == "failed"))
}
classif.loose.nocancel.benefit <- function(final.note){
  return(which(final.note == "failed"))
}
classif.pass.nocancel.benefit <- function(final.note){
  return(which(final.note == "approved" ))
}

# MODEL BENEFIT
classif.model.benefit <- function(data, unanalyzed, wrong, right, condition.func){
  # GET ALL MODELS
  #models <- classif_utils.loaded.models.list( classif_utils.load.models( paste( CLASS_MODELS, CLASS_MODELS_DELTA, sep = "" ) ) )
  models <- load.models(CL_MODELS)
  data.results <- data.frame(data, Estado.Modelo = rep(unanalyzed,nrow(data)) )
  levels(data.results$Estado.Modelo) <- c(levels(data.results$Estado.Modelo), c(wrong,right))
  
  # MODELS FILTER - APPEARANCE ON DATA
  for (model.name in names(models)) {
    model.name.f <- gsub("cf-","",model.name) # MODEL WITHOUT LEARNING APPROACH
    model.asignature <- gsub("_.*","",model.name.f) # MODEL ASIGNATURE
    model.year.end <- gsub(".*_","",model.name.f) # MODEL YEAR TO PREDICT
    
    appears.asig <- grep(model.asignature,data$Codigo.Asignatura) # DATA SAMPLES TO PRED BY ASIG
    appears.year <- grep(toString(model.year.end),data$Periodo.Academico) # DATA SAMPLES TO PRED BY NEXT YEAR
    appears.asig.year <- intersect(appears.asig,appears.year) # BY ASIG & NEXT YEAR
    
    if (length(appears.asig.year) > 0) {
      # CHANGE TO COVERED BUT SUGGEST IN A WRONG WAY
      data.results[appears.asig.year,]$Estado.Modelo <- wrong
      
      # PREDICT
      test.data <- classif_utils.asig.trans(data[appears.asig.year,]) # GET DATA TO PREDICT
      results <- predict(models[[model.name]][[1]],test.data) # PREDICT
      
      # CONDITION TO SUGGEST RIGHT
      benefited <- condition.func(results)
      
      if (length(benefited) > 0) {
        data.results[appears.asig.year[benefited],]$Estado.Modelo <- right
      }
    }
  }
  return(data.results)
}


classif.bestModel.benefit.general <- function(allData, asignatures){
   
  data.filtered <- classif_utils.data.adq(allData, asignatures, removeCancel = FALSE)
  
  data.cancel <- data.filtered[ data.filtered$Estado.Asignatura %in% "CancelaciaIn", ]
  data.nocancel <- data.filtered[ data.filtered$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO NO CANCELED
  
  data.loose <- data.nocancel[ data.nocancel$Nota.Final < 30 & data.nocancel$Nota.Final != 0, ] # NO CANCEL & LOOSE
  data.pass <- data.nocancel[ data.nocancel$Nota.Final >= 30 & data.nocancel$Nota.Final <= 50, ] # NO CANCEL & PASS
  
  # DROPLEVELS
  data.cancel <- droplevels(data.cancel) # CLEAN UNUSED FACTORS
  data.loose <- droplevels(data.loose) # CLEAN UNUSED FACTORS
  data.pass <- droplevels(data.pass) # CLEAN UNUSED FACTORS
  
  # GET BENEFITS BY MODELS
  results.cancel <- classif.model.benefit(data.cancel,"Sin Analizar",
                                "Sugiere Continuar","Sugiere Cancelar",classif.cancel.benefit) # PASS & NO CANCEL RESULTS
  
  results.loose <- classif.model.benefit(data.loose,"Sin Analizar",
                                 "Sugiere Continuar","Sugiere Cancelar",classif.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
  
  results.pass <- classif.model.benefit(data.pass,"Sin Analizar",
                                "Sugiere Cancelar","Sugiere Continuar",classif.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
  
  # PLOTS
  files <- c(paste(PLOTS_DIR_CLA,"total_benefit.html",sep = ""),
             paste(PLOTS_DIR_CLA,"total_benefit_unanalyzed.html",sep = ""))
  classif_utils.plot.sunburst.tool(results.cancel,results.loose,results.pass,files)
}

classif.bestModel.benefit.asig <- function(allData, asignatures){
  
  # # DATA ADQUIRE
  # allData <- read.csv(NOTES, header = TRUE)
  # 
  # # LINEAR MODEL ISIS
  # asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  # asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  # asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  # asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  # asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
  for (asig in asignatures) {
    data.filtered <- classif_utils.data.adq(allData, asig)
    data.cancel <- data.filtered[ data.filtered$Estado.Asignatura %in% "CancelaciaIn", ]
    data.nocancel <- data.filtered[ data.filtered$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO NO CANCELED
    data.loose <- data.nocancel[ data.nocancel$Nota.Final < 30 & data.nocancel$Nota.Final != 0, ] # NO CANCEL & LOOSE
    data.pass <- data.nocancel[ data.nocancel$Nota.Final >= 30 & data.nocancel$Nota.Final <= 50, ] # NO CANCEL & PASS
    
    # DROPLEVELS
    data.cancel <- droplevels(data.cancel) # CLEAN UNUSED FACTORS
    data.loose <- droplevels(data.loose) # CLEAN UNUSED FACTORS
    data.pass <- droplevels(data.pass) # CLEAN UNUSED FACTORS
    
    # GET BENEFITS BY MODELS
    results.cancel <- classif.model.benefit(data.cancel,"Sin Analizar",
                                            "Sugiere Continuar","Sugiere Cancelar",classif.cancel.benefit) # PASS & NO CANCEL RESULTS
    
    results.loose <- classif.model.benefit(data.loose,"Sin Analizar",
                                           "Sugiere Continuar","Sugiere Cancelar",classif.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
    
    results.pass <- classif.model.benefit(data.pass,"Sin Analizar",
                                          "Sugiere Cancelar","Sugiere Continuar",classif.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
    
    # PLOTS
    asig.f <- gsub("\\s", "",asig)
    dir <- paste(PLOTS_DIR_CLA,asig.f,"/",sep = "")
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    files <- c(paste(dir,asig.f,"_benefit.html",sep = ""),
               paste(dir,asig.f,"_benefit_unanalyzed.html",sep = ""))
    classif_utils.plot.sunburst.tool(results.cancel,results.loose,results.pass,files)
  }
}

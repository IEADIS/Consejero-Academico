library(psych)
library(caret)
library(plotly)
source("Analisis/R_Scripts/utils.R")
source("Analisis/R_Scripts/models/Classification/classification.R")
source("Analisis/R_Scripts/models/approach_selection.R")
source("Analisis/R_Scripts/models/Regression/benefit.R")

# =======================================================================================================
# ======================================== PREDICTIONS ==================================================
# =======================================================================================================

# MODEL BENEFIT
abs.model.benefit <- function(data, unanalyzed, wrong, right, condition.func){
  # GET ALL MODELS
  models <- load.models(ABS_MODELS)
  data.results <- data.frame(data, Estado.Modelo = rep(unanalyzed,nrow(data)))
  levels(data.results$Estado.Modelo) <- c(unanalyzed,wrong,right)
  
  # MODELS FILTER - APPEARANCE ON DATA
  for (model.name in names(models)) {
    model.name.f <- gsub("abs-","",model.name) # MODEL WITHOUT LEARNING APPROACH
    model.asignature <- gsub("_.*","",model.name.f) # MODEL ASIGNATURE
    model.year.pred <- gsub(".*_","",model.name.f) # MODEL YEAR TO PREDICT
    
    appears.asig <- grep(model.asignature,data$Codigo.Asignatura) # DATA SAMPLES TO PRED BY ASIG
    appears.year <- grep(toString(model.year.pred),data$Periodo.Academico) # DATA SAMPLES TO PRED BY NEXT YEAR
    appears.asig.year <- intersect(appears.asig,appears.year) # BY ASIG & NEXT YEAR
    
    if (length(appears.asig.year) > 0) {
      # CHANGE TO COVERED BUT SUGGEST IN A WRONG WAY
      data.results[appears.asig.year,]$Estado.Modelo <- wrong
      
      # PREDICT
      results <- NULL
      if (models[[model.name]]$Approach %in% 'Regression') {
        test.data <- asig.trans(data[appears.asig.year,]) # GET DATA TO PREDICT
        results <- lm.test(models[[model.name]],test.data[,-1])$pred # PREDICT
        note.final <- rowMeans(data.frame(data.results[appears.asig.year,]$nota1,data.results[appears.asig.year,]$nota2,results))
      } else if (models[[model.name]]$Approach %in% 'Classification') {
        test.data <- classif_utils.asig.trans(data[appears.asig.year,]) # GET DATA TO PREDICT
        results <- predict(models[[model.name]],test.data) # PREDICT
        note.final <- results
      }
      
      # CONDITION TO SUGGEST RIGHT
      
      benefited <- condition.func(note.final,models[[model.name]]$Approach)
      
      if (length(benefited) > 0) {
        data.results[appears.asig.year[benefited],]$Estado.Modelo <- right
      }
    }
  }
  return(data.results)
}

abs.loose.nocancel.benefit <- function(final.note,approach){
  if (approach %in% 'Regression') {
    return(regress.loose.nocancel.benefit(final.note))
  } else if (approach %in% 'Classification') {
    return(classif.loose.nocancel.benefit(final.note))
  }
  return(NULL)
}
abs.pass.nocancel.benefit <- function(final.note,approach){
  if (approach %in% 'Regression') {
    return(regress.pass.nocancel.benefit(final.note))
  } else if (approach %in% 'Classification') {
    return(classif.pass.nocancel.benefit(final.note))
  }
  return(NULL)
}
abs.cancel.benefit <- function(final.note,approach){
  if (approach %in% 'Regression') {
    return(regress.cancel.benefit(final.note))
  } else if (approach %in% 'Classification') {
    return(classif.cancel.benefit(final.note))
  }
  return(NULL)
}

# =======================================================================================================
# ======================================== ABS BENEFIT ==================================================
# =======================================================================================================

abs.bestModel.benefit.general <- function(allData, asignatures){
  
  data.filtered <- regress.data.adq(allData, asignatures)
  data.cancel <- data.filtered[ data.filtered$Estado.Asignatura %in% "CancelaciaIn", ]
  data.nocancel <- data.filtered[ data.filtered$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO NO CANCELED
  data.loose <- data.nocancel[ data.nocancel$Nota.Final < 30 & data.nocancel$Nota.Final != 0, ] # NO CANCEL & LOOSE
  data.pass <- data.nocancel[ data.nocancel$Nota.Final >= 30 & data.nocancel$Nota.Final <= 50, ] # NO CANCEL & PASS
  
  # DROPLEVELS
  data.cancel <- droplevels(data.cancel) # CLEAN UNUSED FACTORS
  data.loose <- droplevels(data.loose) # CLEAN UNUSED FACTORS
  data.pass <- droplevels(data.pass) # CLEAN UNUSED FACTORS
  
  # GET BENEFITS BY MODELS
  results.cancel <- abs.model.benefit(data.cancel,"Sin Analizar",
                                           "Sugiere Continuar","Sugiere Cancelar",abs.cancel.benefit)
  
  results.loose <- abs.model.benefit(data.loose,"Sin Analizar",
                                         "Sugiere Continuar","Sugiere Cancelar",abs.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
  
  results.pass <- abs.model.benefit(data.pass,"Sin Analizar",
                                        "Sugiere Cancelar","Sugiere Continuar",abs.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
  
  # PLOTS
  files <- c(paste(PLOTS_DIR_ABS,"total_benefit.html",sep = ""),
             paste(PLOTS_DIR_ABS,"total_benefit_unanalyzed.html",sep = ""),
             paste(PLOTS_DIR_ABS,"conf_results.html",sep = ""))
  regress.plot.sunburst.tool(results.cancel,results.loose,results.pass,files,asignatures,"Final")
}

abs.bestModel.benefit.asig <- function(allData, asignatures){
  
  for (asig in asignatures) {
    data.filtered <- regress.data.adq(allData, asig)
    data.cancel <- data.filtered[ data.filtered$Estado.Asignatura %in% "CancelaciaIn", ]
    data.nocancel <- data.filtered[ data.filtered$Estado.Asignatura != "CancelaciaIn", ] # STUDENTS WHO NO CANCELED
    data.loose <- data.nocancel[ data.nocancel$Nota.Final < 30 & data.nocancel$Nota.Final != 0, ] # NO CANCEL & LOOSE
    data.pass <- data.nocancel[ data.nocancel$Nota.Final >= 30 & data.nocancel$Nota.Final <= 50, ] # NO CANCEL & PASS
    
    # DROPLEVELS
    data.cancel <- droplevels(data.cancel) # CLEAN UNUSED FACTORS
    data.loose <- droplevels(data.loose) # CLEAN UNUSED FACTORS
    data.pass <- droplevels(data.pass) # CLEAN UNUSED FACTORS
    
    # GET BENEFITS BY MODELS
    results.cancel <- abs.model.benefit(data.cancel,"Sin Analizar",
                                            "Sugiere Continuar","Sugiere Cancelar",abs.cancel.benefit)
    
    results.loose <- abs.model.benefit(data.loose,"Sin Analizar",
                                           "Sugiere Continuar","Sugiere Cancelar",abs.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
    
    results.pass <- abs.model.benefit(data.pass,"Sin Analizar",
                                          "Sugiere Cancelar","Sugiere Continuar",abs.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
    
    # PLOTS
    asig.f <- gsub("\\s", "",asig)
    dir <- paste(PLOTS_DIR_ABS,asig.f,"/",sep = "")
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    files <- c(paste(dir,asig.f,"_benefit.html",sep = ""),
               paste(dir,asig.f,"_benefit_unanalyzed.html",sep = ""),
               paste(dir,asig.f,"_conf_results.html",sep = ""))
    regress.plot.sunburst.tool(results.cancel,results.loose,results.pass,files,asig,"Final")
  }
}
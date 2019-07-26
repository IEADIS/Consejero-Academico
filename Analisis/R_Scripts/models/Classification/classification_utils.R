# Utils for Classification Models
library(caret)
source("Analisis/R_Scripts/utils.R")

# =======================================================================================================
# ========================================= DATA UTILS ==================================================
# =======================================================================================================

# Get data directory
classif_utils.getDataCleanClassDir <- function(){
  return( getDataCleanDir() )
}

# ADQUISITION OF DATA BY ASIGNATURE AND TIME WINDOW [TIME.START < TIME.END]
classif_utils.asig.adq <- function(data, asig, time.start, time.end){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final != 0 & data.asig$Estado.Asignatura != "Retiro", ] # NO ACCOMP
  
  total.ind <- c()
  for (i in c(time.start:time.end)) {
    ind <- grep(i,data.asig$Periodo.Academico)
    total.ind <- c(total.ind,ind)
  }
  data.asig <- data.asig[total.ind,] # TIME WINDOW
  return(data.asig)
}

# TRANSFORMATION OF THE DATA TO A FORMAT ACCEPTED BY THE MODELS TO TRAIN
classif_utils.asig.trans <- function(data){
  data.trans <- data.frame( Asig = data$Codigo.Asignatura, Grade1 = data$nota1, Grade2 = data$nota2, 
                            Grade3 = factor(+(data$Nota.Final >= 30)) ) # VARIABLES SELECTION
  levels(data.trans$Grade3) <- c("0" = "failed", "1" = "approved")
  return(data.trans)
}

# PARTITIONING OF OUR DATA INTO TRAIN AND TEST
classif_utils.asig.part <- function(data.trans){
  m <- nrow(data.trans) # LENGTH OF DATA
  data.trans <- data.trans[sample(m), ] # RANDOM SAMPLES
  
  data.train <- data.trans[1:round(m*0.7),] # TRAINING DATA - 70%
  data.test <- data.trans[round(m*0.7)+1:round(m*0.3),] # TEST DATA - 30%
  return(list(train = na.omit(data.train), test = na.omit(data.test)))
}


# =======================================================================================================
# ========================================= SAVE MODEL ==================================================
# =======================================================================================================

classif_utils.save.model <- function(model, model.type, model.name){
  MODELS <- getClassificationModelsDir()
  saveRDS(model, file = paste(MODELS, model.type, model.name, ".rds", sep = ""))
}

# =======================================================================================================
# ======================================= EVALUATE MODEL ================================================
# =======================================================================================================

# RETURNS THE MOST RELEVANT METRICS IN CLASSIFICATION
classif_utils.getMetricsBinomial <- function( pred, class ){
  return( confusionMatrix(pred, class)$byClass )
}




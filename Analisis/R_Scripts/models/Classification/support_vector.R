# Support Vector Machine Model

# =======================================================================================================
# ========================================== TRAIN MODEL ================================================
# =======================================================================================================

train.svm <- function(data){

  # TRAIN MODEL
  write( "===============DATA TRAIN===================", stdout() )
  write( str(data), stdout() )
  write( data$Grade3, stdout() )
  write( "===============DATA TRAIN===================", stdout() )
  ctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 5, savePredictions = TRUE, allowParallel = TRUE)
  grid <- expand.grid(C = c(0.1,0.25,0.5,0.75,1))
  mod_fit <- train( Grade3 ~ Grade1 + Grade2,  data=data, method="svmLinear", family="binomial", 
                    trControl = ctrl, tuneGrid = grid, verbose = TRUE)
  
  return(mod_fit)
  
}

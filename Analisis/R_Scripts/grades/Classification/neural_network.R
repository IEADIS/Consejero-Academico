# Neural Network Model

# =======================================================================================================
# ========================================== TRAIN MODEL ================================================
# =======================================================================================================

train.nn <- function(data){
  
  # TRAIN MODEL
  ctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 5, savePredictions = TRUE, allowParallel = TRUE)
  grid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-4), .size = c(3, 5, 10))
  mod_fit <- train( Grade3 ~ Grade1 + Grade2,  data=data, method="nnet",
                    preProcess = c('center', 'scale'), trControl = ctrl, tuneGrid = grid)
  
  return(mod_fit)
  
}

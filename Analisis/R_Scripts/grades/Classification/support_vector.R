# Support Vector Machine Model

# =======================================================================================================
# ========================================== TRAIN MODEL ================================================
# =======================================================================================================

train.svm <- function(data){

  # TRAIN MODEL
  
  train.data <<- data
  
  grid <- expand.grid(C = c(0.1,0.25,0.5,0.75,1))
  
  mod_fit <- tryCatch( {
    ctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 5, savePredictions = TRUE, allowParallel = TRUE)
    mod_fit <- train( Grade3 ~ Grade1 + Grade2,  data=data, method="svmLinear", family="binomial", 
                      trControl = ctrl, tuneGrid = grid, verbose = TRUE, preProcess = c("scale", "center"))
    return(mod_fit)
  }, error = {
    mod_fit <- train( Grade3 ~ Grade1 + Grade2,  data=data, method="svmLinear", family="binomial", 
                      tuneGrid = grid, verbose = TRUE)
    return(mod_fit)
  } )
  
  return(mod_fit)
  
}

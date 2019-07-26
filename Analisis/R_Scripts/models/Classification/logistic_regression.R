# Logistic regression Model


# =======================================================================================================
# ========================================== TRAIN MODEL ================================================
# =======================================================================================================

train.glm <- function(data){
  
  # TRAIN MODEL
  ctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 10, savePredictions = TRUE, allowParallel = TRUE)
  mod_fit <- train( Grade3 ~ Grade1 + Grade2,  data=data, method="glm", family="binomial", 
                    trControl = ctrl, tuneLength = 5)
  
  return(mod_fit)
  
}

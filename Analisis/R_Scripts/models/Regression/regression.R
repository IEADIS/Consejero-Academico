library(psych)
library(caret)
library(cowplot)
library(scatterplot3d)
source("Analisis/R_Scripts/utils.R")
#devtools::install_github("laresbernardo/lares")

NOTES = "Data/notas_clean.csv"
LM_MODELS = "Data/Modelos/Regresion/Lineal/"
PLOTS_DIR = "Analisis/Plots/"


# =======================================================================================================
# ====================================== GRADES ADQUIS ==================================================
# =======================================================================================================

# ADQUISITION OF DATA BY ASIGNATURE AND TIME WINDOW [TIME.START < TIME.END]
asig.adq <- function(data, asig, time.start, time.end){
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

asig.trans <- function(data){
  data.trans <- data.frame( Asig = data$Codigo.Asignatura, Grade1 = data$nota1, Grade2 = data$nota2, 
                      Grade3 = data$nota3 ) # VARIABLES SELECTION
  return(data.trans)
}

asig.part <- function(data.trans){
  m <- nrow(data.trans) # LENGTH OF DATA
  data.trans <- data.trans[sample(m), ] # RANDOM SAMPLES
  
  data.train <- data.trans[1:round(m*0.7),] # TRAINING DATA - 70%
  data.test <- data.trans[round(m*0.7)+1:round(m*0.3),] # TEST DATA - 30%
  return(list(train = na.omit(data.train), test = na.omit(data.test)))
}


# =======================================================================================================
# ====================================== MODEL TRAIN/TEST ===============================================
# =======================================================================================================

lm.train <- function(data.train){
  regressControl  <- trainControl(method="repeatedcv",
                                  number = 10,
                                  repeats = 5)

  lm.model <- train(Grade3 ~ .,
                   data = data.train,
                   method  = "lm",
                   trControl = regressControl)
  return(lm.model)
}

lm.test <- function(lm.model,data.test){
  pred.test <- predict( lm.model, data.test )
  err <- sqrt( mean((pred.test - data.test$Grade3)^2) )
  lin.cor <- cor(pred.test,data.test$Grade3)
  return(list(pred = pred.test, RMSE = err, Rsquared = lin.cor))
}

# =======================================================================================================
# ================================ MODEL SELECTION / COMPARISON ========================================
# =======================================================================================================

model.selection.lm.sum <- function(models, test.results, asignature){ # BY SUM CV + TEST METRIC
  # CARET TRAIN COMPARISON
  train.results <- resamples(models[[asignature]])
  
  # LM METRICS
  rmse.cv <- summary(train.results)$statistics[["RMSE"]][,c("Mean")]
  r2.cv <- summary(train.results)$statistics[["Rsquared"]][,c("Mean")]
  rmse.test <- test.results[test.results$Asignature %in% asignature,]$RMSE
  r2.test <- test.results[test.results$Asignature %in% asignature,]$Rsquared
  
  best.rmse <- names(which.min((rmse.cv + rmse.test)/2))
  best.r2 <- names(which.max((r2.cv + r2.test)/2))
  
  # INDEX OF BEST MODEL
  best.model.ind <- names(which.max(table(c(best.rmse,best.r2))))
  return(models[[asignature]][[best.model.ind]])
}

model.selection.lm.app <- function(models, test.results, asignature){ # BY APPEARANCES
  # CARET TRAIN COMPARISION
  train.results <- resamples(models[[asignature]])
  
  # LM METRICS
  app.best.model <- c(names(which.min(summary(train.results)$statistics[["RMSE"]][,c("Mean")])), # RMSE
                      names(which.max(summary(train.results)$statistics[["Rsquared"]][,c("Mean")]))) # R2
  
  app.best.model <- c(app.best.model,
                      test.results[which.min(test.results[test.results$Asignature %in% asignature,]$RMSE),]$TimeSince, #RMSE
                      test.results[which.max(test.results[test.results$Asignature %in% asignature,]$Rsquared),]$TimeSince) #R2
  
  # INDEX OF BEST MODEL
  best.model.ind <- names(which.max(table(app.best.model)))
  print(paste("Index: ",best.model.ind))
  print(paste("Models Dim: ",length(models[[asignature]])))
  return(models[[asignature]][[best.model.ind]])
}

# =======================================================================================================
# ========================================= MODEL PLOTS =================================================
# =======================================================================================================

plots.lm <- function(best.model,models,asig){
  
  asig <- gsub("\\s", "",asig)
  dir <- paste(PLOTS_DIR,"MODELS/Regression/",asig,"/",sep = "")
  dir.create(dir)
  
  
  if (length(models) > 1) {
    # MODELS PREDICTIONS COMP
    plot.comp.lm(models,best.model,asig,dir)
    
    # MODELS BOX COMPARE
    pdf(paste(dir,"Models_Metrics.pdf",sep = ""))
    p <- bwplot(resamples(models), main = paste("Box Plot Comparing",asig,"Linear Models",sep = " "))
    print(p)
    dev.off()
  }
  
  
  # FINAL MODEL
  plot.single.lm(best.model,asig,dir)

}

plot.comp.lm <- function(models,best.model,asig,dir){
  plots.grid <- list()
  for (model.name in names(models)) {
    tmp.model <- models[[model.name]]
    year <- paste(model.name,strtoi(model.name)+5,sep = " ")
    p <- lares::mplot_lineal(tag = tmp.model$trainingData$.outcome, 
                             score = predict(tmp.model), 
                             subtitle = paste("Grade3 Regression Model",year), 
                             model_name = paste(asig,"reg",year,sep = " "))
    plots.grid[[model.name]] <- p
  }
  p <- plot_grid(plotlist = plots.grid, labels = "AUTO")
  save_plot(paste(dir,"Models_by_Years.pdf",sep = ""),p,nrow = 2,ncol = 2,device = cairo_pdf)
}

plot.single.lm <- function(model,asig,dir){
  pdf(paste(dir,"Final_Model_3D.pdf",sep = ""))
  
  # 3D PLOT
  Grade1 <- seq(min(model$trainingData$Grade1),50, by=0.5) # SECUENCIAL DATA GRADE1
  Grade2 <- seq(min(model$trainingData$Grade2),50, by=0.5) # SECUENCIAL DATA GRADE2
  
  pred_grid <- expand.grid(Grade1 = Grade1, Grade2 = Grade2)
  pred_grid$Grade3 <-predict(model, new = pred_grid)
  
  dat <- data.frame(Grade1 = model$trainingData$Grade1,
                    Grade2 = model$trainingData$Grade2, 
                    real.val = model$trainingData$.outcome)
  
  # 3D 15 Degrees
  plane.model.15 <- scatterplot3d(pred_grid$Grade1, pred_grid$Grade2,
                                  pred_grid$Grade3, angle = 15,
                                  color = "dodgerblue", pch = 1, main = paste(asig,"Regression Model"),
                                  ylab = "Grade2", xlab = "Grade1", zlab = "Grade3" )
  plane.model.15$points3d(dat$Grade1, dat$Grade2, dat$real.val, pch=16)
  
  # 3D 30 Degrees
  plane.model.30 <- scatterplot3d(pred_grid$Grade1, pred_grid$Grade2,
                                  pred_grid$Grade3, angle = 30,
                                  color = "dodgerblue", pch = 1, main = paste(asig,"Regression Model"),
                                  ylab = "Grade2", xlab = "Grade1", zlab = "Grade3" )
  plane.model.30$points3d(dat$Grade1, dat$Grade2, dat$real.val, pch=16)
  
  # 3D 45 Degrees
  plane.model.45 <- scatterplot3d(pred_grid$Grade1, pred_grid$Grade2,
                                  pred_grid$Grade3, angle = 45,
                                  color = "dodgerblue", pch = 1, main = paste(asig,"Regression Model"),
                                  ylab = "Grade2", xlab = "Grade1", zlab = "Grade3" )
  plane.model.45$points3d(dat$Grade1, dat$Grade2, dat$real.val, pch=16)
  
  # 3D 60 Degrees
  plane.model.60 <- scatterplot3d(pred_grid$Grade1, pred_grid$Grade2,
                                  pred_grid$Grade3, angle = 60,
                                  color = "dodgerblue", pch = 1, main = paste(asig,"Regression Model"),
                                  ylab = "Grade2", xlab = "Grade1", zlab = "Grade3" )
  plane.model.60$points3d(dat$Grade1, dat$Grade2, dat$real.val, pch=16)
  
  plane.model.15
  plane.model.30
  plane.model.45
  plane.model.60
  
  dev.off()
  
  # REAL VS PREDICTED VALUES
  lares.f <- lares::mplot_lineal(tag = model$trainingData$.outcome, 
                                 score = predict(model), 
                                 subtitle = "Grade3 Regression Model", 
                                 model_name = paste(asig,"reg",sep = " "))
  save_plot(paste(dir,"Final_Model.pdf",sep = ""),lares.f,device = cairo_pdf)
}


# =======================================================================================================
# ====================================== MODELS SAVE/LOAD ===============================================
# =======================================================================================================

save.model.lm <- function(model,model.name){
  save.model(model, file.name.path = paste(LM_MODELS,"lm-",model.name,".rds",sep = ""))
}


# =======================================================================================================
# ======================================== MODEL DEPLOY =================================================
# =======================================================================================================

deploy.lm <- function(asig,time.start,time.end){
  set.seed(123)
  # ADQUISITION
  allData <- read.csv(NOTES, header = TRUE)
  data.asig <- asig.adq(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  if (nrow(data.asig[data.asig$Codigo.Asignatura %in% asig,]) < 10) {
    return(NULL)
  }
  data.trans <- asig.trans(data.asig)
  data.part <- asig.part(data.trans)
  
  # TRAINING
  model <- lm.train(data.part$train[,-1])
  
  # TESTING
  test.result <- lm.test(model,data.part$test[,-1])
  
  # SAVING DATA
  model.data <- data.frame(Asignature = unique(data.part$test$Asig),
                           TimeSince = time.start,
                           RMSE = test.result$RMSE, Rsquared = test.result$Rsquared,
                           stringsAsFactors = FALSE)
  return(list(Data = model.data, Model = model))
}

# =======================================================================================================
# ========================================= ISIS MODELS =================================================
# =======================================================================================================

isis.models <- function(){
  # DATA ADQUIRE
  allData <- read.csv(NOTES, header = TRUE)
  # LINEAR MODEL ISIS
  asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  years.ini <- c(2009,2010,2011,2012)
  # ASIG SISTEMAS
  models.comp <- data.frame(Asignature = c(), TimeSince = c(), RMSE = c(), Rsquared = c())
  models.data <- list()
  list.asig <- 1
  for (asignature in asig.sistemas) { # BY ASIGNATURE
    models.data[[list.asig]] <- list()
    list.year <- 1
    for (year in years.ini) { # BY YEAR WINDOW
      asig.model <- deploy.lm(asignature,toString(year),toString(year+4))
      if (is.null(asig.model)) {next()}
      models.comp <- rbind(models.comp, asig.model$Data)
      models.data[[list.asig]][[list.year]] <- asig.model$Model
      names(models.data[[list.asig]])[list.year] <- year
      list.year <- list.year+1
    }
    names(models.data)[list.asig] <- asignature
    list.asig <- list.asig+1
  }
  # BEST MODEL SELECTION
  
  best.models <- list()
  print(unique(models.comp$Asignature))
  for (asignature in unique(models.comp$Asignature)) {
    print(paste("Asignature : ",asignature))
    if (length(models.data[[asignature]]) == 1) {
      best.model <- models.data[[asignature]][[1]]
    } else {
    best.model <- model.selection.lm.sum(models.data,models.comp,asignature)}
    best.models[[asignature]] <- best.model
    
    # PLOTS ASIGNATURE
    plots.lm(best.model,models.data[[asignature]],asignature)
  }
  
  # SAVE MODELS
  for (model.name in names(best.models)) {
    save.model.lm(best.models[[model.name]], gsub("\\s", "", model.name))
  }
  return(list(best.models <- best.models, all.models <- models.data))
}

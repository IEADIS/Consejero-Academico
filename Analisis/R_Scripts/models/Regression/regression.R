library(psych)
library(caret)
library(cowplot)
library(scatterplot3d)
source("Analisis/R_Scripts/utils.R")
#devtools::install_github("laresbernardo/lares")


# =======================================================================================================
# ====================================== GRADES ADQUIS ==================================================
# =======================================================================================================

# ADQUISITION OF DATA BY ASIGNATURE AND TIME WINDOW [TIME.START < TIME.END]
asig.adq <- function(data, asig, time.start, time.end){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final > 0 & data.asig$Nota.Final <= 50 & !(data.asig$Estado.Asignatura %in% c("Retiro","CancelaciaIn")), ] # NO ACCOMP
  
  total.ind <- c()
  for (i in c(time.start:time.end)) {
    ind <- grep(i,data.asig$Periodo.Academico)
    total.ind <- c(total.ind,ind)
  }
  data.asig <- data.asig[total.ind,] # TIME WINDOW
  return(data.asig)
}

asig.adq.test <- function(data){
  data.asig <- data[ data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS'), ] # ISIS STUDENTS
  data.asig <- droplevels(data.asig)
  return(na.omit(data.asig))
}

asig.trans <- function(data){
  data.trans <- data.frame( Asig = data$Codigo.Asignatura, Grade1 = data$nota1, Grade2 = data$nota2, 
                      Grade3 = data$nota3 ) # VARIABLES SELECTION
  return(data.trans)
}

asig.part <- function(data,last.year){ # WALK-BACKWARD APPROACH
  
  data.test <- data[sub("-.*","",droplevels(data$Periodo.Academico)) %in% last.year,] 
  data.train <- data[!(sub("-.*","",droplevels(data$Periodo.Academico)) %in% last.year),]

  return(list(train = na.omit(data.train[sample(nrow(data.train)),]), test = na.omit(data.test[sample(nrow(data.test)),])))
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
  test.pred <<- pred.test; test.data <<- data.test
  err <- sqrt( mean((pred.test - data.test$Grade3)^2) )
  lin.cor <- cor(pred.test,data.test$Grade3)
  return(list(pred = pred.test, RMSE = err, Rsquared = lin.cor))
}

# =======================================================================================================
# ================================ MODEL SELECTION / COMPARISON ========================================
# =======================================================================================================

models.mean.lm <- function(models, test.results, asignature){
  if (length(models) == 1 | "finalModel" %in% names(models)) {
    # CV METRICS
    if ("results" %in% names(models)) {
      rmse.cv <- models$results$RMSE
      r2.cv <- models$results$Rsquared
    } else {
      rmse.cv <- models[[1]]$results$RMSE
      r2.cv <- models[[1]]$results$Rsquared
    }
  } else {
    # CARET TRAIN RESULTS
    train.results <- resamples(models)
    
    # CV METRICS
    rmse.cv <- summary(train.results)$statistics[["RMSE"]][,c("Mean")]
    r2.cv <- summary(train.results)$statistics[["Rsquared"]][,c("Mean")]
  }
  
  # TEST METRICS
  rmse.test <- test.results$RMSE
  r2.test <- test.results$Rsquared
  
  # METRICS MEAN
  rmse.mean <- mean((rmse.cv + rmse.test)/2)
  r2.mean <- mean((r2.cv + r2.test)/2)
  
  return(data.frame(RMSE = rmse.mean, RSquared = r2.mean))
}

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

plots.lm <- function(lambda.results,models,asig,year.pred,selected.lambda){
  
  asig <- gsub("\\s", "",asig)
  dir <- paste(PLOTS_DIR_REG,asig,"/",year.pred,"/",sep = "")
  print(dir)
  dir.create(dir)
  
  # LAMBDA TABLE RESULTS
  plot.lambdas(lambda.results,dir,selected.lambda)
  
  if (length(models) > 1) {
    # MODELS PREDICTIONS COMP
    plot.comp.lm(models,asig,dir,year.pred)
    
    # MODELS BOX COMPARE
    pdf(paste(dir,"Models_Metrics.pdf",sep = ""))
    p <- bwplot(resamples(models), main = paste("Box Plot Comparing",asig,"Linear Models",sep = " "))
    print(p)
    dev.off()
  } else {
    plot.single.lm(models[[1]],asig,dir)
  }
  
  
  

}

plot.comp.lm <- function(models,asig,dir,year.pred){
  plots.grid <- list()
  for (model.name in names(models)) {
    tmp.model <- models[[model.name]]
    p <- lares::mplot_lineal(tag = tmp.model$trainingData$.outcome, 
                             score = predict(tmp.model), 
                             subtitle = paste("Grade3 Regression",year.pred,": Years trained:",models[[model.name]]$yearsTrained), 
                             model_name = paste(asig,"reg",model.name,sep = " "))
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

plot.lambdas <- function(lambdas.results,dir,selected.lambda){
  row.fill <- c()
  for (row in 1:nrow(lambdas.results)) {
    if (row %in% selected.lambda) {
      row.fill <- c(row.fill,'#25FEFD')
    } else {
      row.fill <- c(row.fill,'white')
    }
  }
  
  p <- plot_ly(
    type = 'table',
    header = list(
      values = c('<b>Asignature<b>',
                 '<b>YearPred<b>',
                 '<b>Lambda</b>',
                 '<b>RMSE</b>',
                 '<b>RSquared</b>',
                 '<b>RMSE Norm</b>',
                 '<b>RSquared Norm</b>'),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('left','center'),
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = rbind(lambdas.results$Asig,
                     lambdas.results$YearPred,
                     lambdas.results$Lambda,
                     round(lambdas.results$RMSE,4),
                     round(lambdas.results$RSquared,4),
                     round(lambdas.results$RMSE.norm,4),
                     round(lambdas.results$RSquared.norm,4)),
      line = list(color = '#506784'),
      fill = list(color = list(row.fill)),
      align = c('left', 'center'),
      font = list(color = c('#506784'), size = 12)
    ))
  print(paste(dir,"Lambda_Selection.html",sep = ""))
  
  htmlwidgets::saveWidget(p, file.path(normalizePath(dirname(paste(dir,"Lambda_Selection.html",sep = ""))),
                                       basename(paste(dir,"Lambda_Selection.html",sep = ""))))
}

plot.occ.lambdas <- function(lambdas.results,dir){
  p <- plot_ly(data = data.frame(table(unlist(lambdas.results$Lambda))),
               x = ~Var1, y = ~Freq, type = 'bar', name = 'Lambda Occurences') %>%
    layout(title = "Lambdas Occurrences", yaxis = list(title = 'Count'), xaxis = list(title = 'Lambdas'))
  export(p, paste(dir,"Lambda_Occurrences.pdf",sep = ""))
}


# =======================================================================================================
# ====================================== MODELS SAVE/LOAD ===============================================
# =======================================================================================================

save.model.lm <- function(model,model.name){
  print(paste(LM_MODELS,"lm-",model.name,".rds",sep = ""))
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
  if (nrow(data.asig[data.asig$Codigo.Asignatura %in% asig,]) < 20) {
    return(NULL)
  }
  data.part <- asig.part(data.asig,time.end)
  data.trans.train <- asig.trans(data.part$train)
  
  # TEST WITH JUST ISIS STUDENTS
  data.part$test <- asig.adq.test(data.part$test)
  data.trans.test <- asig.trans(data.part$test)
  
  # TRAINING
  model <- lm.train(data.trans.train[,-1])
  
  # TESTING
  test.result <- lm.test(model,data.trans.test[,-1])
  
  # SAVING DATA
  model.data <- data.frame(Asignature = unique(data.trans.test$Asig),
                           TimeWindow = toString((strtoi(time.end) - strtoi(time.start))+1),
                           TimePred = toString(strtoi(time.end)+1),
                           RMSE = test.result$RMSE, Rsquared = test.result$Rsquared,
                           stringsAsFactors = FALSE)
  return(list(Data = model.data, Model = model))
}

# =======================================================================================================
# ========================================= ISIS MODELS =================================================
# =======================================================================================================

regress.bestModel.train <- function(allData, asignatures, omitted.years = c()){
  # DATA ADQUIRE
  # allData <- read.csv(NOTES, header = TRUE)
  # LINEAR MODEL ISIS
  # asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  # asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  # asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  # asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  # asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
  asig.sistemas <- asignatures
  
  # ASIG SISTEMAS
  models.comp <- data.frame(Asignature = c(), TimeWindow = c(), TimePred = c(), RMSE = c(), Rsquared = c())
  models.data <- list()
  list.asig <- 1
  for (asignature in asig.sistemas) { # BY ASIGNATURE
    models.data[[list.asig]] <- list()
    # GET ALL YEARS
    years <- unique(sub("-.*","",droplevels(allData[allData$Codigo.Asignatura %in% asignature,]$Periodo.Academico))) # ALL YEARS
    years.num <- strtoi(years) # INT VALUES OF YEARS
    years.num <- c(years.num,max(years.num)+1) # ADD THE NEW YEAR TO PRED
    list.year.pred <- 1
    
    for (year.pred in years.num) { # BY YEAR TO PREDICT
      if ( (year.pred - min(years.num)) < 2 | (year.pred %in% strtoi(omitted.years)) ) { next() }# IF THE MIN DIFF IS LESS THAN 2, CANT BE TRAIN
      year.time.lambdas <- 2:(year.pred - min(years.num))
      models.data[[list.asig]][[list.year.pred]] <- list()
      list.lambda <- 1
      
      for (year.diff in year.time.lambdas) { # BY TIME WINDOW / YEAR LAMBDA
        asig.model <- deploy.lm(asignature,toString(year.pred-year.diff),toString(year.pred-1))
        if (is.null(asig.model) | length(asig.model$Model) == 0) {next()} # CANT CREATE THE MODEL
        if (any(is.na(asig.model$Model$results))) {next()} # THE MODEL CONTAINS NAs RESAMPLES, NO SENSE MODEL
        models.comp <- rbind(models.comp, asig.model$Data)
        models.data[[list.asig]][[list.year.pred]][[list.lambda]] <- asig.model$Model
        names(models.data[[list.asig]][[list.year.pred]])[list.lambda] <- year.diff
        list.lambda <- list.lambda+1
      }
      names(models.data[[list.asig]])[list.year.pred] <- year.pred
      list.year.pred <- list.year.pred+1
    }
    models.data[[list.asig]] <- Filter(length,models.data[[list.asig]])
    names(models.data)[list.asig] <- asignature
    list.asig <- list.asig+1
  }
  
  m.results <<- models.comp
  m.data <<- models.data
  
  # BEST MODEL SELECTION
  
  best.models <- list()
  lambdas.ind.all <- c()
  all.results <- data.frame(Asig = c(), YearPred = c(), Lambda = c(), RMSE = c(), RSquared = c(),
                             RMSE.norm = c(), RSquared.norm = c())
  
  print(unique(models.comp$Asignature))
  for (asignature in unique(models.comp$Asignature)) {
    best.models[[asignature]] <- list()
    lambdas.ind.asig <- c()
    asig.results <- data.frame(Asig = c(), YearPred = c(), Lambda = c(), RMSE = c(), RSquared = c(),
                               RMSE.norm = c(), RSquared.norm = c())
    print(paste("Asignature : ",asignature))
    # PRE-PLOTING PARAMS
    asig <- gsub("\\s", "",asignature)
    dir <- paste(PLOTS_DIR_REG,asig,"/",sep = "")
    dir.create(dir)
    
    for (year.topred in names(models.data[[asignature]])) {
      lambda.results <- data.frame(Asig = c(),YearPred = c(), Lambda = c(), RMSE = c(), RSquared = c())
      
      for (year.lambda in names(models.data[[asignature]][[year.topred]])) {
        models.metrics <- models.mean.lm(models.data[[asignature]][[year.topred]][[year.lambda]],
                                         models.comp[models.comp$Asignature %in% asignature & models.comp$TimePred %in% year.topred & models.comp$TimeWindow %in% year.lambda,],
                                         asignature)
        models.metrics$Lambda <- year.lambda
        models.metrics$YearPred <- strtoi(year.topred)
        models.metrics$Asig <- asignature
        lambda.results <- rbind(lambda.results, models.metrics)
      }
      print(lambda.results)
      lambda.results$RMSE.norm <- scales::rescale(-lambda.results$RMSE, to=c(0,1))
      lambda.results$RSquared.norm <- scales::rescale(lambda.results$RSquared, to=c(0,1))
      
      best.lambda.ind <- which.max(rowMeans(lambda.results[,c(6,7)]))
      best.model.lambda <- lambda.results$Lambda[best.lambda.ind]
      print(paste("Best Lambda for",year.topred,":",best.model.lambda))
      
      lambdas.ind.asig <- c(lambdas.ind.asig, best.lambda.ind+nrow(asig.results))
      asig.results <- rbind(asig.results, lambda.results)
      
      best.models[[asignature]][[year.topred]] <- models.data[[asignature]][[year.topred]][[best.model.lambda]]
      best.models[[asignature]][[year.topred]]$yearsTrained <- paste(strtoi(year.topred)-1-strtoi(best.model.lambda),
                                                                     "-",strtoi(year.topred)-1,sep = "")
      # PLOTS ASIGNATURE YEAR
      plots.lm(lambda.results,models.data[[asignature]][[year.topred]],asignature,year.topred,best.lambda.ind)
    }
    lambdas.ind.all <- c(lambdas.ind.all, lambdas.ind.asig+nrow(all.results))
    all.results <- rbind(all.results, asig.results)
    # PLOTS LAMBDAS ASIGNATURE
    plot.lambdas(asig.results,dir,lambdas.ind.asig)
  }
  # PLOTS LAMBDAS ALL
  plot.lambdas(all.results[lambdas.ind.all,],PLOTS_DIR_REG,c())
  plot.occ.lambdas(all.results[lambdas.ind.all,],PLOTS_DIR_REG)
  
  # SAVE MODELS
  for (asig.name in names(best.models)) {
    for (pred.asig in names(best.models[[asig.name]])) {
      save.model.lm(best.models[[asig.name]][[pred.asig]],
                    paste(gsub("\\s", "", asig.name),pred.asig,sep = "_"))
    }
  }
  return(best.models)
}

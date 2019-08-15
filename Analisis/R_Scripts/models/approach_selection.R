library(psych)
library(caret)
library(cowplot)
source("Analisis/R_Scripts/utils.R")

model.approach.selection <- function(test.data, regress.model, classif.model, asig, year){
  reg.f1 <- reg.approach.cof(test.data, regress.model)
  cla.f1 <- cla.approach.cof(test.data, classif.model)
  
  reg.mean <- mean(reg.f1$byClass['F1'], reg.f1$overall['Accuracy'])
  cla.mean <- mean(cla.f1$byClass['F1'], cla.f1$overall['Accuracy'])
  
  app.results <- data.frame(Asig = asig,
                            YearPred = year,
                            L.Approach = 'Classification',
                            F1 = cla.f1$byClass["F1"],
                            Acc = cla.f1$overall['Accuracy'],
                            Spec = 'No Neccesary',
                            Mean = 'None', stringsAsFactors = FALSE)
  
  if (!is.na(reg.f1$byClass['Specificity']) & !is.na(cla.f1$byClass['Specificity'])) {
    reg.mean <- mean(reg.f1$byClass['Specificity'], reg.mean)
    cla.mean <- mean(cla.f1$byClass['Specificity'], cla.mean)
    
    app.results$Spec <- cla.f1$byClass['Specificity']
    if (reg.mean >= cla.mean) {
      app.results$Spec <- reg.f1$byClass['Specificity']
    }
  }
  if (reg.mean >= cla.mean) {
    app.results$L.Approach <- 'Regression'
    app.results$Mean <- reg.mean
    app.results$F1 <- reg.f1$byClass["F1"]
    app.results$Acc <- reg.f1$overall['Accuracy']
    regress.model$Approach <- 'Regression'
    return(list(Model = regress.model,Results = app.results))
  }
  
  app.results$Mean <- cla.mean
  classif.model$Approach <- 'Classification'
  return(list(Model = classif.model,Results = app.results))
}

reg.approach.cof <- function(test.data, regress.model){
  data.trans.reg <- asig.trans(test.data)
  
  reg.pred <- predict(regress.model, data.trans.reg[,-1])
  reg.Grade3 <- factor(+(data.trans.reg$Grade3 < 30))
  reg.Grade3.pred <- factor(+(reg.pred < 30))
  levels(reg.Grade3) <- c("1" = "failed", "0" = "approved")
  levels(reg.Grade3.pred) <- c("1" = "failed", "0" = "approved")
  
  return(confusionMatrix(reg.Grade3.pred, reg.Grade3))
}

cla.approach.cof <- function(test.data, classif.model){
  data.trans.cla <- classif_utils.asig.trans(test.data)
  
  return(confusionMatrix(predict(classif.model, data.trans.cla), data.trans.cla$Grade3))
}

abs.models.selection <- function(data, regress.models, classif.models, asignatures, years.pred){
  best.models <- list()
  list.asig <- 1
  all.app.results <- data.frame(Asig = c(), YearPred = c(), L.Approach = c(), F1 = c(), Acc = c(), Spec = c(), Mean = c())
  for (asig in asignatures) {
    dir <- paste(PLOTS_DIR_ABS,gsub("\\s", "", asig),"/",sep = "")
    dir.create(dir)
    app.results <- data.frame(Asig = c(), YearPred = c(), L.Approach = c(), F1 = c(), Acc = c(), Spec = c(), Mean = c())
    
    abs.models.asig.reg <- NULL
    abs.models.asig.cla <- NULL
    if ( length(grep(asig, names(regress.models), fixed = TRUE)) > 0 ) { # REGRESSION MODEL - GET
      abs.models.asig.reg <- regress.models[[names(regress.models)[grep(asig, names(regress.models), fixed = TRUE)]]]
    }
    if ( length(grep(asig, names(classif.models), fixed = TRUE)) > 0 ) { # CLASSIFICATION MODEL - GET
      abs.models.asig.cla <- classif.models[[names(classif.models)[grep(asig, names(classif.models), fixed = TRUE)]]]
    }
    best.models[[list.asig]] <- list()
    list.year.pred <- 1
    for (year in years.pred) {
      abs.model.year.reg <- NULL
      abs.model.year.cla <- NULL
      if ( length(grep(year, names(abs.models.asig.reg))) > 0 ) { # REGRESSION MODEL BY YEAR - GET
        abs.model.year.reg <- abs.models.asig.reg[[names(abs.models.asig.reg)[grep(year, names(abs.models.asig.reg))]]]
      }
      if ( length(grep(year, names(abs.models.asig.cla))) > 0 ) { # REGRESSION MODEL BY YEAR - GET
        abs.model.year.cla <- abs.models.asig.cla[[names(abs.models.asig.cla)[grep(year, names(abs.models.asig.cla))]]]
      }
      abs.model <- NULL
      if (!is.null(abs.model.year.reg) & !is.null(abs.model.year.cla)) {
        data.filtered <- asig.adq(data,asig,year-1,year-1)
        results.abs.model <- model.approach.selection(data.filtered,abs.model.year.reg, abs.model.year.cla$model,asig,year)
        abs.model <- results.abs.model$Model
        app.results <- rbind(app.results,results.abs.model$Results)
        print(paste('1: ASIG:',asig,'YEAR:',year,'Approach:',abs.model$Approach))
      }else if (!is.null(abs.model.year.reg)) {
        abs.model <- abs.model.year.reg
        abs.model$Approach <- 'Regression'
        reg.conf <- reg.approach.cof(asig.adq(data,asig,year-1,year-1),abs.model)
        reg.spec <- 'None'
        reg.mean <- mean(reg.conf$byClass["F1"],reg.conf$overall['Accuracy'])
        if (!is.na(reg.conf$byClass['Specificity'])) {
          reg.spec <- reg.conf$byClass['Specificity']
          reg.mean <- mean(reg.conf$byClass['Specificity'],reg.mean)}
        app.results <- rbind(app.results,data.frame(Asig = asig, YearPred = year, L.Approach = 'Regression',
                                                    F1 = reg.conf$byClass["F1"], Acc = reg.conf$overall['Accuracy'],
                                                    Spec = reg.spec, Mean = reg.mean,
                                                    stringsAsFactors = FALSE))
        print(paste('2: ASIG:',asig,'YEAR:',year,'Approach:',abs.model$Approach))
      }else if (!is.null(abs.model.year.cla)) {
        abs.model <- abs.model.year.cla$model
        abs.model$Approach <- 'Classification'
        cla.conf <- cla.approach.cof(asig.adq(data,asig,year-1,year-1),abs.model)
        cla.spec <- 'None'
        cla.mean <- mean(cla.conf$byClass["F1"],cla.conf$overall['Accuracy'])
        if (!is.na(cla.conf$byClass['Specificity'])) {
          cla.spec <- cla.conf$byClass['Specificity']
          cla.mean <- mean(cla.conf$byClass['Specificity'],cla.mean)}
        app.results <- rbind(app.results,data.frame(Asig = asig, YearPred = year, L.Approach = 'Regression',
                                                    F1 = cla.conf$byClass["F1"], Acc = cla.conf$overall['Accuracy'],
                                                    Spec = cla.spec, Mean = cla.mean,
                                                    stringsAsFactors = FALSE))
        print(paste('3: ASIG:',asig,'YEAR:',year,'Approach:',abs.model$Approach))
      }
      if (is.null(abs.model)) {next()}
      best.models[[list.asig]][[list.year.pred]] <- abs.model
      names(best.models[[list.asig]])[list.year.pred] <- year
      list.year.pred <- list.year.pred + 1
    }
    if (length(app.results) == 0) {next()}
    best.models[[list.asig]] <- Filter(length,best.models[[list.asig]])
    names(best.models)[list.asig] <- asig
    list.asig <- list.asig + 1
    all.app.results <- rbind(all.app.results, app.results)
    # PLOT ASIGNATURE ABS SELECTION
    plot.abs.selection(app.results,dir)
  }
  # PLOT ALL ABS SELECTION
  plot.abs.selection(all.app.results,PLOTS_DIR_ABS)
  
  # SAVE MODELS
  for (asig.name in names(best.models)) {
    for (pred.asig in names(best.models[[asig.name]])) {
      save.model.abs(best.models[[asig.name]][[pred.asig]],
                    paste(gsub("\\s", "", asig.name),pred.asig,sep = "_"))
    }
  }
  
  return(best.models)
}

save.model.abs <- function(model,model.name){
  save.model(model, file.name.path = paste(ABS_MODELS,"abs-",model.name,".rds",sep = ""))
}


# =======================================================================================================
# ============================================ PLOTS ====================================================
# =======================================================================================================

plot.abs.selection <- function(abs.results,dir){
  p <- plot_ly(
    type = 'table',
    header = list(
      values = c('<b>Asignature<b>',
                 '<b>YearPred<b>',
                 '<b>Approach</b>',
                 '<b>F1</b>',
                 '<b>Accuracy</b>',
                 '<b>Specificity</b>',
                 '<b>Mean</b>'),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('left','center'),
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = rbind(abs.results$Asig,
                     abs.results$YearPred,
                     abs.results$L.Approach,
                     abs.results$F1,
                     abs.results$Acc,
                     abs.results$Spec,
                     abs.results$Mean),
      line = list(color = '#506784'),
      fill = list(color = 'white'),
      align = c('left', 'center'),
      font = list(color = c('#506784'), size = 12)
    ))
  print(paste(dir,"ABS_Selection.pdf",sep = ""))
  export(p, paste(dir,"ABS_Selection.pdf",sep = ""))
  
  abs.results$App <- rep(1,nrow(abs.results))
  p.props <- plot_ly(abs.results, labels = ~L.Approach, values = ~App, type = 'pie') %>%
    layout(title = 'Proporcion de Enfoques',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p.props, paste(dir,"Approach_Props.pdf",sep = ""))
}
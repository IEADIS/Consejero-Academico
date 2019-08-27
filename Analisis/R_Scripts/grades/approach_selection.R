# =====================================================================================================================
# ========================================== ABS MODEL SELECTION ======================================================
# =====================================================================================================================

analy.falses <- function(data){
  grade.real <- data$Nota.Final
  grade.pred <- rowMeans(as.data.frame(data$nota1,data$nota2,data$nota3.Pred))
  data.falses.ind <- which((grade.real < 30 & grade.pred >= 30) | (grade.real >= 30 & grade.pred < 30))
  diff.falses <- abs(grade.pred[data.falses.ind]-30)
  
  data.falses <- data[data.falses.ind,]
  data.falses$nota.pred.var <- diff.falses
  data.falses <- droplevels(data.falses)
  
  # p <- plot_ly(y = data.falses$nota.pred.var, 
  #              color = data.falses$Codigo.Asignatura, type = "box",
  #              colors = colorRampPalette(c("red","blue",'green'))(nrow(unique(data.falses$Codigo.Asignatura))))
  p <- plot_ly(y = data.falses$nota.pred.var, type = "box")
  
  htmlwidgets::saveWidget(p, file.path(normalizePath(dirname(paste(PLOTS_DIR_ABS,"Var_Falses.html",sep = ""))),
                                       basename(paste(PLOTS_DIR_ABS,"Var_Falses.html",sep = ""))))
  return(data.falses)
}

model.approach.selection <- function(test.data, regress.model, classif.model, asig, year){
  reg.analyz <- reg.approach.cof(test.data, regress.model)
  reg.f1 <- reg.analyz$cm
  test.data$nota3.Pred <- reg.analyz$preds
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
    return(list(Model = regress.model,Results = app.results, Data = test.data))
  }
  
  app.results$Mean <- cla.mean
  classif.model$Approach <- 'Classification'
  return(list(Model = classif.model,Results = app.results, Data = test.data))
}

reg.approach.cof <- function(test.data, regress.model){
  data.trans.reg <- asig.trans(test.data)
  
  reg.pred <- predict(regress.model, data.trans.reg[,-1])
  
  reg.final <- factor(+(test.data$Nota.Final < 30))
  reg.final.pred <- factor(+(rowMeans(as.data.frame(reg.pred,data.trans.reg$Grade1,data.trans.reg$Grade2)) < 30))
  levels(reg.final) <- c("1" = "failed", "0" = "approved")
  levels(reg.final.pred) <- c("1" = "failed", "0" = "approved")
  
  return(list(cm = confusionMatrix(reg.final.pred, reg.final), preds = reg.pred))
}

cla.approach.cof <- function(test.data, classif.model){
  data.trans.cla <- classif_utils.asig.trans(test.data)
  
  return(confusionMatrix(predict(classif.model, data.trans.cla), data.trans.cla$Grade3))
}

abs.best.approach <- function(data,abs.model.year.reg,abs.model.year.cla,asig,year.test,year.pred){
  abs.model <- NULL
  best.results <- NULL
  data.filtered <- asig.adq(data,asig,year.test,year.test)
  data.filtered <- asig.adq.test(data.filtered) # JUST ISIS STUDENTS
  if (!is.null(abs.model.year.reg) & !is.null(abs.model.year.cla) & (nrow(data.filtered) != 0)) {
    results.abs.model <- model.approach.selection(data.filtered,abs.model.year.reg, abs.model.year.cla$model,asig,year.pred)
    abs.model <- results.abs.model$Model
    best.results <- results.abs.model$Results
    data.filtered <- results.abs.model$Data
  }else if (!is.null(abs.model.year.reg) & (nrow(data.filtered) != 0)) {
    abs.model <- abs.model.year.reg
    abs.model$Approach <- 'Regression'
    reg.analyz <- reg.approach.cof(data.filtered,abs.model)
    data.filtered$nota3.Pred <- reg.analyz$preds
    reg.conf <- reg.analyz$cm
    reg.spec <- 'None'
    reg.mean <- mean(reg.conf$byClass["F1"],reg.conf$overall['Accuracy'])
    if (!is.na(reg.conf$byClass['Specificity'])) {
      reg.spec <- reg.conf$byClass['Specificity']
      reg.mean <- mean(reg.conf$byClass['Specificity'],reg.mean)}
    best.results <- data.frame(Asig = asig, YearPred = year.pred, L.Approach = 'Regression',
                               F1 = reg.conf$byClass["F1"], Acc = reg.conf$overall['Accuracy'],
                               Spec = reg.spec, Mean = reg.mean,
                               stringsAsFactors = FALSE)
  }else if (!is.null(abs.model.year.cla) & (nrow(data.filtered) != 0)) {
    abs.model <- abs.model.year.cla$model
    abs.model$Approach <- 'Classification'
    cla.conf <- cla.approach.cof(data.filtered,abs.model)
    cla.spec <- 'None'
    cla.mean <- mean(cla.conf$byClass["F1"],cla.conf$overall['Accuracy'])
    if (!is.na(cla.conf$byClass['Specificity'])) {
      cla.spec <- cla.conf$byClass['Specificity']
      cla.mean <- mean(cla.conf$byClass['Specificity'],cla.mean)}
    best.results <- data.frame(Asig = asig, YearPred = year.pred, L.Approach = 'Regression',
                               F1 = cla.conf$byClass["F1"], Acc = cla.conf$overall['Accuracy'],
                               Spec = cla.spec, Mean = cla.mean,
                               stringsAsFactors = FALSE)
  }
  return(list(bestmodel = abs.model, results = best.results, data = data.filtered))
}

abs.models.selection <- function(data, regress.models, classif.models, asignatures, years.pred){
  best.models <- list()
  list.asig <- 1
  all.app.results <- data.frame(Asig = c(), YearPred = c(), L.Approach = c(), F1 = c(), Acc = c(), Spec = c(), Mean = c())
  val.results <- all.app.results
  val.data <- data.frame()
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
      
      best.approach <- abs.best.approach(data,abs.model.year.reg,abs.model.year.cla,asig,year-1,year) # TEST
      abs.model <- best.approach$bestmodel
      app.results <- rbind(app.results,best.approach$results)
      
      if (is.null(abs.model)) {next()}
      
      best.approach.val <- abs.best.approach(data,abs.model.year.reg,abs.model.year.cla,asig,year,year) # VALIDATION
      val.results <- rbind(val.results,best.approach.val$results)
      
      if ('nota3.Pred' %in% names(best.approach$data)) { val.data <- rbind(val.data,best.approach$data,best.approach.val$data) }
      
      abs.model$Confidence <- best.approach.val$results$Mean #CONFIDENCE METRIC ADDED TO MODEL
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
  # PLOT VALIDATION
  dir.create(paste(PLOTS_DIR_ABS,'Validation/',sep = ''))
  tot.val.results <- rbind(val.results,all.app.results)
  plot.abs.selection(tot.val.results[order(tot.val.results$Asig, tot.val.results$YearPred),],paste(PLOTS_DIR_ABS,'Validation/',sep = ''))
  plot.abs.validation(val.results[,c(1:3)],all.app.results[,c(1:3)],paste(PLOTS_DIR_ABS,'Validation/',sep = ''))
  
  # VARIANCE ON FP/FN
  analy.falses(val.data)
  
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

plot.abs.validation <- function(results.val, results.abs, dir){
  validation <- data.frame()
  true.best <- dplyr::intersect(results.abs,results.val)[,c(1:2)]
  false.best <- dplyr::setdiff(results.val,results.abs)[,c(1:2)]
  unpredicted <- dplyr::setdiff(dplyr::setdiff(results.abs,results.val)[,c(1:2)],false.best)
  
  true.best$state <- factor(rep('TRUE BEST',nrow(true.best)))
  false.best$state <- factor(rep('FALSE BEST',nrow(false.best)))
  unpredicted$state <- factor(rep('UNPREDICTED',nrow(unpredicted)))
  
  validation <- rbind(unpredicted,true.best,false.best)
  validation$App <- rep(1,nrow(validation))
  
  p.props <- plot_ly(validation, labels = ~state, values = ~App, type = 'pie') %>%
    layout(title = 'VALIDACI?N DE MEJOR MODELO',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p.props, paste(dir,"Approach_Vals.pdf",sep = ""))
}

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
  htmlwidgets::saveWidget(p, file.path(normalizePath(dirname(paste(dir,"ABS_Selection.html",sep = ""))),
                                       basename(paste(dir,"ABS_Selection.html",sep = ""))))
  
  abs.results$App <- rep(1,nrow(abs.results))
  p.props <- plot_ly(abs.results, labels = ~L.Approach, values = ~App, type = 'pie') %>%
    layout(title = 'Proporcion de Enfoques',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p.props, paste(dir,"Approach_Props.pdf",sep = ""))
}
library(psych)
library(caret)
library(plotly)
source("Analisis/R_Scripts/utils.R")
source("Analisis/R_Scripts/models/Regression/regression.R")

# =======================================================================================================
# ====================================== GRADES ADQUIS ==================================================
# =======================================================================================================

# DATA ADQUISITION FILTER
regress.data.adq <- function(data, asig){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final <= 50 & data.asig$Estado.Asignatura != "Retiro", ] # NO ACCOMP
  
  data.asig <- data[ data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS'), ] # ISIS STUDENTS
  
  data.asig <- droplevels(data.asig) # CLEAN UNUSED FACTORS
  
  return(data.asig)
}

# =======================================================================================================
# ======================================== PREDICTIONS ==================================================
# =======================================================================================================

# MODEL BENEFIT
regress.model.benefit <- function(data, unanalyzed, wrong, right, condition.func){
  # GET ALL MODELS
  models <- load.models(LM_MODELS)
  data.results <- data.frame(data, Estado.Modelo = rep(unanalyzed,nrow(data)),
                             RMSE = rep(NA,nrow(data)), Rsquared = rep(NA,nrow(data)))
  levels(data.results$Estado.Modelo) <- c(unanalyzed,wrong,right)
  
  # MODELS FILTER - APPEARANCE ON DATA
  for (model.name in names(models)) {
    model.name.f <- gsub("lm-","",model.name) # MODEL WITHOUT LEARNING APPROACH
    model.asignature <- gsub("_.*","",model.name.f) # MODEL ASIGNATURE
    model.year.pred <- gsub(".*_","",model.name.f) # MODEL YEAR TO PREDICT
    
    # model.years <- gsub(".*_","",model.name.f) # MODEL YEAR TO PREDICT
    # model.years.start <- strtoi(gsub("-.*","",model.name.f))
    # model.years.end <- strtoi(gsub(".*-","",model.name.f))
    
    appears.asig <- grep(model.asignature,data$Codigo.Asignatura) # DATA SAMPLES TO PRED BY ASIG
    appears.year <- grep(toString(model.year.pred),data$Periodo.Academico) # DATA SAMPLES TO PRED BY NEXT YEAR
    appears.asig.year <- intersect(appears.asig,appears.year) # BY ASIG & NEXT YEAR
    
    if (length(appears.asig.year) > 0) {
      # CHANGE TO COVERED BUT SUGGEST IN A WRONG WAY
      data.results[appears.asig.year,]$Estado.Modelo <- wrong
      
      # PREDICT
      test.data <- asig.trans(data[appears.asig.year,]) # GET DATA TO PREDICT
      results <- lm.test(models[[model.name]],test.data[,-1]) # PREDICT
      
      # CONDITION TO SUGGEST RIGHT
      note.final <- rowMeans(data.frame(data.results[appears.asig.year,]$nota1,data.results[appears.asig.year,]$nota2,results$pred))
      benefited <- condition.func(note.final)
      
      if (length(benefited) > 0) {
        data.results[appears.asig.year[benefited],]$Estado.Modelo <- right
      }
      data.results[appears.asig.year,]$RMSE <- results$RMSE
      data.results[appears.asig.year,]$Rsquared <- results$Rsquared
    }
  }
  return(data.results)
}

regress.loose.nocancel.benefit <- function(final.note){
  return(which(final.note < 30))
}
regress.pass.nocancel.benefit <- function(final.note){
  return(which(final.note >= 30))
}
regress.cancel.benefit <- function(final.note){
  return(which(final.note < 30))
}

# =======================================================================================================
# ====================================== BENEFIT PLOTS ==================================================
# =======================================================================================================

# PLOT BENEFIT
regress.plot.benefit <- function(results,file_path){
  results$App <- rep(1,nrow(results))
  p <- plot_ly(results, labels = ~Estado.Modelo, values = ~App, type = 'pie') %>%
    layout(title = 'Uso de la Herramienta por Estudiante',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p, file_path)
}

regress.plot.sunburst.tool <- function(cancel.data,loose.data,pass.data,files_path,asignatures,method){
  name.asig <- asignatures
  if (length(asignatures) > 1) {
    name.asig <- "General"
  }
  all.values <- data.frame(total = sum(nrow(cancel.data),nrow(pass.data),nrow(loose.data)),
                           canceled = nrow(cancel.data),
                           loose = nrow(loose.data),
                           pass = nrow(pass.data),
                           unanalyzed.cancel = strtoi(summary(cancel.data$Estado.Modelo)["Sin Analizar"]),
                           wrong.cancel = strtoi(summary(cancel.data$Estado.Modelo)["Sugiere Continuar"]),
                           right.cancel = strtoi(summary(cancel.data$Estado.Modelo)["Sugiere Cancelar"]),
                           unanalyzed.loose = strtoi(summary(loose.data$Estado.Modelo)["Sin Analizar"]),
                           wrong.loose = strtoi(summary(loose.data$Estado.Modelo)["Sugiere Continuar"]),
                           right.loose = strtoi(summary(loose.data$Estado.Modelo)["Sugiere Cancelar"]),
                           unanalyzed.pass = strtoi(summary(pass.data$Estado.Modelo)["Sin Analizar"]),
                           wrong.pass = strtoi(summary(pass.data$Estado.Modelo)["Sugiere Cancelar"]),
                           right.pass = strtoi(summary(pass.data$Estado.Modelo)["Sugiere Continuar"]))
  
  d <<- data.frame(
    labels = c("Inscripciones","Cancelo","Perdio","Paso",
               " Sug. Continuar"," Sug. Cancelar",
               "Sug. Continuar","Sug. Cancelar",
               "Sug. Cancelar ","Sug. Continuar "),
    parents = c("","Inscripciones","Inscripciones","Inscripciones",
                "Cancelo","Cancelo",
                "Perdio","Perdio",
                "Paso","Paso"),
    values = c(all.values$total- all.values$unanalyzed.cancel - all.values$unanalyzed.loose - all.values$unanalyzed.pass,
               all.values$cancel - all.values$unanalyzed.cancel,
               all.values$loose - all.values$unanalyzed.loose,all.values$pass - all.values$unanalyzed.pass,
               all.values$wrong.cancel,all.values$right.cancel,
               all.values$wrong.loose,all.values$right.loose,
               all.values$wrong.pass,all.values$right.pass),
    stringsAsFactors = FALSE
  )
  
  percentages <<- c(round((d$values[1:4]/d$values[1])*100,2),
                   round((d$values[5:6]/d$values[2])*100,2),
                   round((d$values[7:8]/d$values[3])*100,2),
                   round((d$values[9:10]/d$values[4])*100,2))
  
  percentages[percentages %in% NaN] <<- 0
  
  conf <- data.frame(Estado = c("Paso","Perdio"),TP = c(d$values[10],d$values[8]), TN = c(d$values[8],d$values[10]),
                     FP = c(d$values[7],d$values[9]), FN = c(d$values[9],d$values[7]), stringsAsFactors = FALSE)
  conf$PREC <- conf$TP/(conf$TP+conf$FP)
  conf$RECALL <- conf$TP/(conf$TP+conf$FN)
  conf$F1.score <- 2*(conf$PREC*conf$RECALL)/(conf$PREC+conf$RECALL)
  
  p.conf <- plot_ly(
    type = 'table',
    header = list(
      values = c('<b>Asignature<b>',
                 '<b>Method<b>',
                 '<b>Estado<b>',
                 '<b>TP</b>',
                 '<b>FP</b>',
                 '<b>FN</b>',
                 '<b>TN</b>',
                 '<b>PREC</b>',
                 '<b>RECALL</b>',
                 '<b>F1</b>'),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('left','center'),
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = rbind(rep(name.asig,2),
                     rep(method,2),
                     conf$Estado,
                     conf$TP,
                     conf$FP,
                     conf$FN,
                     conf$TN,
                     round(conf$PREC,3),
                     round(conf$RECALL,3),
                     round(conf$F1,3)),
      line = list(color = '#506784'),
      fill = list(color = '#25FEFD'),
      align = c('left', 'center'),
      font = list(color = c('#506784'), size = 12)
    ))
  
  htmlwidgets::saveWidget(p.conf, file.path(normalizePath(dirname(files_path[3])),basename(files_path[3])))
  
  d.unanalyzed <- data.frame(
    labels = c("Inscripciones","Cancelo","Perdio","Paso",
               " Sin Analizar"," Sug. Continuar"," Sug. Cancelar",
               "Sin Analizar","Sug. Continuar","Sug. Cancelar",
               "Sin Analizar ","Sug. Cancelar ","Sug. Continuar "),
    parents = c("","Inscripciones","Inscripciones","Inscripciones",
                "Cancelo","Cancelo","Cancelo",
                "Perdio","Perdio","Perdio",
                "Paso","Paso","Paso"),
    values = c(all.values$total,all.values$cancel,all.values$loose,all.values$pass,
               all.values$unanalyzed.cancel,all.values$wrong.cancel,all.values$right.cancel,
               all.values$unanalyzed.loose,all.values$wrong.loose,all.values$right.loose,
               all.values$unanalyzed.pass,all.values$wrong.pass,all.values$right.pass),
    stringsAsFactors = FALSE
  )
  
  percentages.unanalyzed <- c(round((d.unanalyzed$values[1:4]/d.unanalyzed$values[1])*100,2),
                              round((d.unanalyzed$values[5:7]/d.unanalyzed$values[2])*100,2),
                              round((d.unanalyzed$values[8:10]/d.unanalyzed$values[3])*100,2),
                              round((d.unanalyzed$values[11:13]/d.unanalyzed$values[4])*100,2))
  
  percentages.unanalyzed[percentages.unanalyzed %in% NaN] <- 0
  
  p <- plot_ly(d, labels = ~labels, parents = ~parents, values = ~values,
               text = ~paste(percentages, '%'),
               type = 'sunburst',branchvalues = 'total') %>%
    layout(title = ~paste('Beneficios de la Herramienta',method,name.asig))
  
  p.unanalyzed <- plot_ly(d.unanalyzed, labels = ~labels, parents = ~parents, values = ~values,
               text = ~paste(percentages.unanalyzed, '%'),
               type = 'sunburst',branchvalues = 'total') %>%
    layout(title = ~paste('Beneficios de la Herramienta',method,name.asig))
  
  htmlwidgets::saveWidget(as_widget(p), file.path(normalizePath(dirname(files_path[1])),basename(files_path[1])))
  htmlwidgets::saveWidget(as_widget(p.unanalyzed), file.path(normalizePath(dirname(files_path[2])),basename(files_path[2])))
}

# =======================================================================================================
# ======================================= ISIS BENEFIT ==================================================
# =======================================================================================================

regress.bestModel.benefit.general <- function(allData, asignatures){
  # DATA ADQUIRE
  # allData <- read.csv(NOTES, header = TRUE)
  # LINEAR MODEL ISIS
  # asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  # asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  # asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  # asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  # asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
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
  results.cancel <- regress.model.benefit(data.cancel,"Sin Analizar",
                                  "Sugiere Continuar","Sugiere Cancelar",regress.cancel.benefit)
  
  results.loose <- regress.model.benefit(data.loose,"Sin Analizar",
                                 "Sugiere Continuar","Sugiere Cancelar",regress.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
  
  results.pass <- regress.model.benefit(data.pass,"Sin Analizar",
                                "Sugiere Cancelar","Sugiere Continuar",regress.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
  
  # PLOTS
  files <- c(paste(PLOTS_DIR_REG,"total_benefit.html",sep = ""),
             paste(PLOTS_DIR_REG,"total_benefit_unanalyzed.html",sep = ""),
               paste(PLOTS_DIR_REG,"conf_results.html",sep = ""))
  regress.plot.sunburst.tool(results.cancel,results.loose,results.pass,files,asignatures,"Regresion")
}

regress.bestModel.benefit.asig <- function(allData, asignatures){
  # # DATA ADQUIRE
  # allData <- read.csv(NOTES, header = TRUE)
  # # LINEAR MODEL ISIS
  # asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  # asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  # asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  # asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  # asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  
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
    results.cancel <- regress.model.benefit(data.cancel,"Sin Analizar",
                                    "Sugiere Continuar","Sugiere Cancelar",regress.cancel.benefit)
    
    results.loose <- regress.model.benefit(data.loose,"Sin Analizar",
                                   "Sugiere Continuar","Sugiere Cancelar",regress.loose.nocancel.benefit) # LOOSE & NO CANCEL RESULTS
    
    results.pass <- regress.model.benefit(data.pass,"Sin Analizar",
                                  "Sugiere Cancelar","Sugiere Continuar",regress.pass.nocancel.benefit) # PASS & NO CANCEL RESULTS
    
    # PLOTS
    asig.f <- gsub("\\s", "",asig)
    dir <- paste(PLOTS_DIR_REG,asig.f,"/",sep = "")
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    files <- c(paste(dir,asig.f,"_benefit.html",sep = ""),
               paste(dir,asig.f,"_benefit_unanalyzed.html",sep = ""),
               paste(dir,asig.f,"_conf_results.html",sep = ""))
    regress.plot.sunburst.tool(results.cancel,results.loose,results.pass,files,asig,"Regresion")
  }
}
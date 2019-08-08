# Utils for Classification Models
library(caret)
library(plotly)
library(plyr)
library(cowplot)
library(scatterplot3d)
library(gridExtra)
source("Analisis/R_Scripts/utils.R")

PLOTS_SOURCE_CLASS = "Models/Classification/"
PLOTS_SOURCE_DIR_STATS = "Statistics/"

# =======================================================================================================
# ========================================= DATA UTILS ==================================================
# =======================================================================================================

# Get data directory
classif_utils.getDataCleanClassDir <- function(){
  return( getDataCleanDir() )
}

# ADQUISITION OF DATA BY ASIGNATURE AND TIME WINDOW [TIME.START < TIME.END]
classif_utils.asig.adq <- function(data, asig, time.start, time.end){
  
  data.asig <- classif_utils.data.adq(data, asig)
  
  total.ind <- c()
  for (i in c(time.start:time.end)) {
    ind <- grep(i,data.asig$Periodo.Academico)
    total.ind <- c(total.ind,ind)
  }
  data.asig <- data.asig[total.ind,] # TIME WINDOW
  return(data.asig)
}

# DATA ADQUISITION FILTER
classif_utils.data.adq <- function(data, asig, removeCancel = TRUE){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final <= 50 & data.asig$Estado.Asignatura != "Retiro", ] # NO ACCOMP
  
  if (removeCancel) data.asig <- data.asig[ data.asig$Nota.Final >= 0 & data.asig$Estado.Asignatura != "CancelaciaIn", ] # NO ACCOMP
  
  data.asig <- droplevels(data.asig) # CLEAN UNUSED FACTORS
  
  return(data.asig)
}

# TRANSFORMATION OF THE DATA TO A FORMAT ACCEPTED BY THE MODELS TO TRAIN
classif_utils.asig.trans <- function(data){
  data.trans <- data.frame( Asig = data$Codigo.Asignatura, Grade1 = data$nota1, Grade2 = data$nota2, 
                            Grade3 = factor(+(data$Nota.Final >= 30)), Periodo = data$Periodo.Academico ) # VARIABLES SELECTION
  levels(data.trans$Grade3) <- c("0" = "failed", "1" = "approved")
  return(data.trans)
}

# PARTITIONING OF OUR DATA INTO TRAIN AND TEST
classif_utils.asig.part <- function(data.trans){
  m <- nrow(data.trans) # LENGTH OF DATA
  data.trans <- data.trans[sample(m), ] # RANDOM SAMPLES
  data.train <- data.trans[1:round(m*0.7),] # TRAINING DATA - 70%
  data.test <- data.trans[round(m*0.7)+1:round(m*0.3),] # TEST DATA - 30%
  return(list(train = na.omit(data.train[sample(nrow(data.train)),]), test = na.omit(data.testsample(nrow(data.test)),)))
}


# =======================================================================================================
# ========================================= SAVE/LOAD MODEL ==================================================
# =======================================================================================================

classif_utils.createClassDir <- function(path){
  dir.create( path, showWarnings = FALSE )
}

classif_utils.save.model <- function(model, model.type, model.name){
  MODELS <- getClassificationModelsDir()
  saveRDS(model, file = paste(MODELS, model.type, model.name, ".rds", sep = ""))
}

classif_utils.load.models <- function(files.path){
  models.files <- list.files(path = files.path)
  models <- list()
  for (file.model in models.files) {
    models[[file.model]] <- load.models(paste(files.path,"/",file.model, "/",sep = ""))
  }
  return(models)
}

classif_utils.loaded.models.list <- function(models){
  models.list <- list()
  for (name in names(models)) {
    for ( model.name in names(models[[name]]) ){
      models.list[[model.name]] <- models[[name]][[model.name]]
    }
  }
  return(models.list)
}

classif_utils.load.model <- function(file.model){
  return(readRDS(file.model))
}

# =======================================================================================================
# ======================================= EVALUATE MODEL ================================================
# =======================================================================================================

# RETURNS THE MOST RELEVANT METRICS IN CLASSIFICATION
classif_utils.getMetricsBinomial <- function( pred, class ){
  return( confusionMatrix(pred, class)$byClass )
}

# =======================================================================================================
# ============================================ PLOTS ====================================================
# =======================================================================================================

classif_utils.plotMetricsOfModelsTableByModel <- function(models, ext = ".png"){
  
  dir <-PLOTS_DIR_REG
  
  for ( name in names(models) ){
    
    if ( length(models[[name]]) == 0 ){ next() }
    
    classif_utils.plotNumberOfModelsByType(models[[name]], name = "TrainedAlgorithms", dir_stats = paste( gsub("\\s", "",name), "/", sep = "" ))
    
    classif_utils.plotMetricsOfModelsTable(models = models[[name]], name = "TrainedAlgorithms")
    
    classif_utils.plotMetricsOfModelsTablePDF(models = models[[name]], name = name)
    
  }
  
}

classif_utils.plotMetricsOfModelsTablePDF <- function(models, asig, name = "LambdaSelection.pdf"){
  
  dir <-PLOTS_DIR_REG
  test <<- models
  data.table.metrics <- as.data.frame( plyr::laply(models, function(data){ 
    return( c(data$data,  
              "Lambda" = (data$data$timeWindowEnd - data$data$timeWindowStart + 1) )
    ) 
  } ), strinsAsFactors = F )
  df <- lapply(data.table.metrics, unlist)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  row.fill <- c()
  for (row in 1:length(models)) {
    if (is.null(models[[row]]) ) {next()}
    if ( models[[row]]$data$bestIndex != -1 ) {
      row.fill <- c(row.fill,'#25FEFD')
    } else {
      row.fill <- c(row.fill,'white')
    }
  }
  
  p <- plot_ly(
    type = 'table',
    header = list(
      values = c('<b>Asignature</b>',
                 '<b>Time Window</b>',
                 '<b>Predict Year</b>',
                 '<b>Acc</b>',
                 '<b>F1</b>',
                 '<b>Lambda</b>'),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('left','center'),
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = rbind(df$Asignature,
                     paste( df$timeWindowStart, "-",df$timeWindowEnd, sep = ""),
                     df$timeWindowEnd+1,
                     round(df$Acc, 2),
                     round(df$F1, 2),
                     df$Lambda),
      line = list(color = '#506784'),
      fill = list(color = list(row.fill)),
      align = c('left', 'center'),
      font = list(color = c('#506784'), size = 12)
    ))
  orca(p, paste(dir, gsub("\\s", "",asig), "/" , name,sep = ""))
}

classif_utils.plotMetricsOfModelsTable <- function(models, name = "MetricsTable", ext = ".png" ){
  
  dir <- paste( PLOTS_DIR_REG, PLOTS_SOURCE_DIR_STATS, name, sep = "" )
  
  data.table.metrics <- as.data.frame( plyr::laply(models, function(data){ 
    return( c(data$data,  
              "Lambda" = (data$data$timeWindowEnd - data$data$timeWindowStart) )
    ) 
  } ), strinsAsFactors = F )
  png( paste(dir, ext, sep = ""), height = 30*nrow(data.table.metrics), width = 200*ncol(data.table.metrics))
  df <- lapply(data.table.metrics, unlist)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  grid.table(df)
  dev.off()
  
}

classif_utils.plotNumberOfModelsByType <- function( models, name = "TrainedAlgorithmsWindowPart", ext = ".png", dir_stats = PLOTS_SOURCE_DIR_STATS ){
  types <- summary( factor(laply(models, function(data){ return( data[[2]]$Type ) } ) ) )
  test <<- models
  data <- data.frame( "Categorie" = names(types), "vals" = types )
  
  dir <- paste( PLOTS_DIR_REG, dir_stats, name, sep = "" )
  
  p <- plot_ly(data, labels = ~Categorie, values = ~vals, type = 'pie') %>%
    layout(title = 'Porcentaje de Algoritmos entrenados',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  orca(p, file = paste(dir, ext, sep = ""))
  htmlwidgets::saveWidget(as_widget(p), 
          file.path(normalizePath(dirname( paste(dir, ".html", sep = "") )),
                    basename( paste(dir, ".html", sep = "") )))
  return(p)
}

# PLOT BENEFIT
classif_utils.plot.benefit <- function(results,file_path){
  results$App <- rep(1,nrow(results))
  p <- plot_ly(results, labels = ~Estado.Modelo, values = ~App, type = 'pie') %>%
    layout(title = 'Uso de la Herramienta por Estudiante',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  export(p, file_path)
}

classif_utils.plot.sunburst.tool <- function(cancel.data,loose.data,pass.data,files_path){
  
  covered.asigs <- union(union(levels(cancel.data$Codigo.Asignatura),levels(loose.data$Codigo.Asignatura)),levels(pass.data$Codigo.Asignatura))
 
  
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
  d <- data.frame(
    labels = c("Inscripciones","Cancelo","Perdio","Paso",
               " Sug. Continuar"," Sug. Cancelar",
               "Sug. Continuar","Sug. Cancelar",
               "Sug. Cancelar ","Sug. Continuar "),
    parents = c("","Inscripciones","Inscripciones","Inscripciones",
                "Cancelo", "Cancelo",
                "Perdio","Perdio",
                "Paso","Paso"),
    values = c(all.values$total - all.values$unanalyzed.loose - all.values$unanalyzed.pass - all.values$unanalyzed.cancel,
               all.values$cancel - all.values$unanalyzed.cancel,
               all.values$loose - all.values$unanalyzed.loose,all.values$pass - all.values$unanalyzed.pass,
               all.values$wrong.cancel, all.values$right.cancel, 
               all.values$wrong.loose,all.values$right.loose,
               all.values$wrong.pass,all.values$right.pass),
    stringsAsFactors = FALSE
  )
  
  percentages <- c(round((d$values[1:4]/d$values[1])*100, 2),
                   round((d$values[5:6]/d$values[2])*100, 2),
                   round((d$values[7:8]/d$values[3])*100, 2),
                   round((d$values[9:10]/d$values[4])*100, 2))
  
  percentages[ percentages %in% NaN ] <- 0
  
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
               all.values$unanalyzed.cancel, all.values$wrong.cancel, all.values$right.cancel,
               all.values$unanalyzed.loose,all.values$wrong.loose,all.values$right.loose,
               all.values$unanalyzed.pass,all.values$wrong.pass,all.values$right.pass),
    stringsAsFactors = FALSE
  )

  percentages.unanalyzed <- c(round((d.unanalyzed$values[1:4]/d.unanalyzed$values[1])*100, 2),
                              round((d.unanalyzed$values[5:7]/d.unanalyzed$values[2])*100, 2),
                              round((d.unanalyzed$values[8:10]/d.unanalyzed$values[3])*100, 2),
                              round((d.unanalyzed$values[11:13]/d.unanalyzed$values[4])*100, 2))
  
  p <- plot_ly(d, labels = ~labels, parents = ~parents, values = ~values,
               text = ~paste(percentages, '%'),
               type = 'sunburst',branchvalues = 'total') %>%
    layout(title = 'Beneficios de la Herramienta')
  
  p.unanalyzed <- plot_ly(d.unanalyzed, labels = ~labels, parents = ~parents, values = ~values,
                          text = ~paste(percentages.unanalyzed, '%'),
                          type = 'sunburst',branchvalues = 'total') %>%
    layout(title = 'Beneficios de la Herramienta')
  
  htmlwidgets::saveWidget(as_widget(p), file.path(normalizePath(dirname(files_path[1])),basename(files_path[1])))
  htmlwidgets::saveWidget(as_widget(p.unanalyzed), file.path(normalizePath(dirname(files_path[2])),basename(files_path[2])))
}


library(rgl)
library(magick)
library(plotly)

NOTES = "Data/notas_clean.csv"
LM_MODELS = "Data/Models/Regression/"
CL_MODELS = "Data/Models/Classification/"
ABS_MODELS = "Data/Models/Absolute/"

PLOTS_DIR = "Analisis/Plots/"
PLOTS_DIR_REG <- paste(PLOTS_DIR,"Models/Regression/",sep = "")
PLOTS_DIR_CLA <- paste(PLOTS_DIR,"Models/Classification/",sep = "")
PLOTS_DIR_ABS <- paste(PLOTS_DIR,"Models/Absolute/",sep = "")

CLASS_MODELS = "Data/Models/Classification/"
CLASS_MODELS_DELTA = "/Models_Best_Delta"


# =======================================================================================================
# =================================== 3D VISUALIZATION ==================================================
# =======================================================================================================

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}

rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) 
    xlim <- xlim/1.1; zlim <- zlim /1.1
  rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}

grafica3D <- function(x, z, y){
  rgl_init()
  rgl.spheres(x, y, z, r = 0.5, color = "#D95F02") 
  rgl_add_axes(x, y, z, xlab = 'Tercio1', ylab = 'Tercio3', zlab = 'Tercio2', show.bbox = FALSE)
  aspect3d(1,1,1)
  # Compute the linear regression (y = ax + bz + d)
  fit <- lm(y ~ x + z)
  # predict values on regular xz grid
  grid.lines = 26
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  z.pred <- seq(min(z), max(z), length.out = grid.lines)
  xz <- expand.grid( x = x.pred, z = z.pred)
  y.pred <- matrix(predict(fit, newdata = xz), 
                   nrow = grid.lines, ncol = grid.lines)
  # Add regression surface
  rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
              alpha = 0.5, lit = FALSE)  
  # Add grid lines
  rgl.surface(x.pred, z.pred, y.pred, color = "black",
              alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
  
  wwgl <- writeWebGL(dir = getwd(), 
                     filename = file.path(getwd(), "3dplotMbda.html"))
}

# =======================================================================================================
# ====================================== PREPRO DATA ====================================================
# =======================================================================================================

normalize <- function(x){
  x_n <- x - colMeans(x)
  std <- apply(x_n, 2, sd)
  return(x_n/std)
}
normalizacion <- function(x){
  x_n <- (x - mean(x));
  std <- sqrt(sum((x-mean(x))^2/(length(x))));
  return (x_n/std)
}
calcBestK <- function(x, x_app){
  ASPE <- (1/length(x))*sum( (x-x_app)^2 )
  var <- (1/length(x))*sum(x^2)
  return(ASPE/var)
}

# =======================================================================================================
# ===================================== DATA FILTERS ====================================================
# =======================================================================================================


CancelacionesVsSemestres <- function(data, cancel){
  newData <- data.frame( y = data$Estado.Asignatura, label = data$Periodo.Academico , stringsAsFactors = FALSE)
  newData <- aggregate( y ~ label, newData, cancel)
  return(newData)
}

CancelacionesVsMaterias <- function(data, cancel){
  newData <- data.frame( y = data$Estado.Asignatura, label = data$Codigo.Asignatura , stringsAsFactors = FALSE)
  newData <- aggregate( y ~ label, newData, cancel)
  return(newData)
}

NotasMediasVsMaterias <- function(data){
  newData <- data.frame( y = data$Nota.Final, label = data$Codigo.Asignatura , stringsAsFactors = FALSE)
  newData <- aggregate( y ~ label, newData, mean )
  return(newData)
}

NotasMediasVsSemestres <- function(data){
  newData <- data.frame( y = data$Nota.Final, label = data$Periodo.Academico )
  newData <- aggregate( y ~ label, newData, mean )
  return(newData)
}

correlacion <- function(data) {
  dev.new()
  pairs.panels(data)
}

terciosVsTerciofinal <- function(x, y, z){
  Tercio1y2 <- (x+y)/2
  newData <- data.frame( Tercio1y2, TercioFinal = z )
  return(newData)
}

cancel <- function(data){
  return(sum(data == 'Cancelaci?n' | data == 'Retiro'))
} 

#Get all notes of many students in a matrix, given certain asignatures
getData <- function(allData, asig){
  test <- allData[ allData$Codigo.Asignatura %in% asig, ]
  test <- test[ test$Nota.Final >= 30 & test$Nota.Final <= 50 & test$Estado.Asignatura != "Retiro", ]
  testData <- test[ test$Codigo.Asignatura == asig[1], ]
  int <- testData$Identidad.Estudiante
  i <- 2
  while ( i <= length(asig) ){
    testData <- test[ test$Codigo.Asignatura == asig[i], ]
    int <- intersect(int, testData$Identidad.Estudiante)
    i <- i+1
  }
  int <- unique(int)
  test <- test[order(test$Codigo.Asignatura),]
  dataMat <- matrix(test[ test$Identidad.Estudiante == int[1], ]$Nota.Final, ncol = length(asig))
  colnames(dataMat) <- test[ test$Identidad.Estudiante == int[1], ]$Codigo.Asignatura
  i <- 2
  while ( i <= length(int)){
    dataMat <- rbind(dataMat, test[ test$Identidad.Estudiante == int[i], ]$Nota.Final)
    i <- i+1
  }
  return(dataMat)
}

#---- Get all data of students that has ended the Asignatures --------------------
getFinishedInts <- function(asig){
  test <- allData[ allData$Codigo.Asignatura %in% asig, ]
  test <- test[ test$Nota.Final >= 30 & test$Nota.Final <= 50 & test$Estado.Asignatura != "Retiro", ]
  testData <- test[ test$Codigo.Asignatura == asig[1], ]
  int <- testData$Identidad.Estudiante
  i <- 2
  while ( i <= length(asig) ){
    testData <- test[ test$Codigo.Asignatura == asig[i], ]
    int <- intersect(int, testData$Identidad.Estudiante)
    i <- i+1
  }
  int <- unique(int)
  return(test[ test$Identidad.Estudiante %in% int, ])
}

# =======================================================================================================
# ====================================== DATA PLOTS =====================================================
# =======================================================================================================

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

infoData <- function(data, filePlot){
  
  dt <- NotasMediasVsSemestres(data)
  
  p <- plot_ly( dt, x = ~label, y = ~y, type = "scatter", mode = "lines") %>% layout(
    title = "Notas Medias vs Semestres",
    scene = list(
      xaxis = list(title = "Semestre Academico"),
      yaxis = list(title = "Notas medias")
    ))
  
  export(p, file = filePlot)
  summary(dt)
  
}

infoDataCancel <- function(data, filePlot){
  
  dt <- CancelacionesVsSemestres(data, cancel)
  
  p <- plot_ly( dt, x = ~label, y = ~y, type = "scatter", mode = "lines") %>% layout(
    title = "Cancelaciones vs Semestres",
    scene = list(
      xaxis = list(title = "Semestre Academico"),
      yaxis = list(title = "Notas medias")
    ))
  
  export(p, file = filePlot)
  summary(dt)
  
}

infoDataMat <- function(data){
  dt <- CancelacionesVsSemestres(data, cancel)
  p <- plot_ly( dt, x = ~label, y = ~y, type = "scatter", mode = "lines") %>% layout(
    title = "Cancelaciones vs Semestres",
    scene = list(
      xaxis = list(title = "Semestre Academico"),
      yaxis = list(title = "Numero de cancelaciones")
    ))
  export(p, file = paste(PLOTS_SOURCE,"html/img/cancelVsSems.png",sep = "/"))
  dt <- NotasMediasVsSemestres(data)
  
  p <- plot_ly( dt, x = ~label, y = ~y, type = "scatter", mode = "lines") %>% layout(
    title = "Notas Medias vs Semestres",
    scene = list(
      xaxis = list(title = "Semestre Academico"),
      yaxis = list(title = "Numero de cancelaciones")
    ))
  export(p, file = paste(PLOTS_SOURCE,"html/img/notasVsSems.png",sep = "/"))
  
  dt <- data.frame( Tercio1 = data$nota1, Tercio2 = data$nota2, Tercio3 = data$nota3 )
  correlacion(dt)
  
  dt <- terciosVsTerciofinal(data$nota1, data$nota2, data$nota3)
  p <- plot_ly(data = dt, x = ~Tercio1y2, y = ~TercioFinal)
  htmlwidgets::saveWidget(p,paste(getwd(),PLOTS_SOURCE,"html/General.html",sep = "/"))
  
  #  grafica3D(data$nota1, data$nota2, data$nota3)
}

plotProgramsByYear <- function(data,year){
  dataByYear <- data[data$Periodo.Academico == c(paste(year,"-1",sep = ""),paste(year,"-2",sep = ""),paste(year,"-I",sep = "")),]
  dataByYear$Periodo.Academico <- droplevels(dataByYear$Periodo.Academico)
  dataByYear$Programa.Estudiante <- droplevels(dataByYear$Programa.Estudiante)
  counts <- table(dataByYear$Programa.Estudiante,dataByYear$Periodo.Academico)
  barplot(counts, main = paste("N?mero de Estudiantes por Programa",year), xlab="Periodo Academico", col = gray.colors(9),beside=TRUE)
  legend("topright", legend = rownames(counts), fill = gray.colors(9), ncol = 1, cex = 0.45)
}

# =======================================================================================================
# ==================================== UTILS CLASSIFICATION =============================================
# =======================================================================================================

getClassificationModelsDir <- function(){
  return("Data/Models/Classification/")
}

getClassificationModelsDeltaDir <- function(){
  return("/Models_Best_Delta")
}

getDataCleanDir <- function(){
  return("Data/notas_clean.csv")
}

# =======================================================================================================
# ======================================= MODELS SAVE/LOAD ==============================================
# =======================================================================================================

save.model <- function(model,file.name.path){
  saveRDS(model, file = file.name.path)
}

load.models <- function(files.path){
  models.files <- list.files(path = files.path)
  models <- list()
  for (file.model in models.files) {
    model.name <- gsub(".rds","",file.model)
    models[[model.name]] <- load.model(paste(files.path,file.model,sep = ""))
  }
  return(models)
}

load.model <- function(file.model){
  return(readRDS(file.model))
}
library(magrittr)  # to use piping %>%
library(ggplot2)   # for ploting
library(MASS)      # to calculate the pseudo-inverse of a matrix
library(caret)     # to center our data by subtracting its mean
library(reshape2)  # for data manipulation
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

allData <- read.csv(NOTES, header = TRUE, stringsAsFactors = FALSE)


#--------------- Anomaly detection --------------------

sistemas <- allData[ allData$Area.Asignatura == 'SISTEMAS', ]
matematicas <- allData[ allData$Area.Asignatura == 'MATEMATICAS', ]
humanidades <- allData[ allData$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronica <- allData[ allData$Area.Asignatura == 'ELECTRONICA', ]
electrica <- allData[ allData$Area.Asignatura == 'ELECTRICA', ]
industrial <- allData[ allData$Area.Asignatura == 'INDUSTRIAL', ]

cancelaciones <- CancelacionesVsMaterias(sistemas, cancel)
notaFinal <- NotasMediasVsMaterias(sistemas)
dt <- data.frame( mat = cancelaciones$label, can = cancelaciones$y, mat1 = notaFinal$label, final = notaFinal$y, stringsAsFactors = FALSE )


#----------------Anomaly Graph-------------------

anomalyDetection <- function(data, filePlot){
  
  cancelaciones <- CancelacionesVsMaterias(data, cancel)
  data <- data[ data$Nota.Final != 0, ]
  notaFinal <- NotasMediasVsMaterias(data)
  dt <- data.frame( mat = cancelaciones$label, can = cancelaciones$y, final = notaFinal$y )
  
  p <- plot_ly( dt, x = ~can, y = ~final, color = ~mat) %>% layout(
    title = "Cancelaciones vs Semestres",
    scene = list(
      xaxis = list(title = "Semestre Academico"),
      yaxis = list(title = "Notas medias")
    ))
  
  
  return(p)
  
}

# Create preProcess object
X <- dt[,-c(1,3)]
preObj <- preProcess(X,method="center")
# Center the data- subtract the column means from the data points
X2 <- predict(preObj,X)
X2= as.matrix(X2)
sigma2=diag(var(X2))
sigma2=diag(sigma2)
A=(2*pi)^(-ncol(X2)/2)*det(sigma2)^(-0.5)
B = exp(-0.5 *rowSums((X2%*%ginv(sigma2))*X2))
p=A*B
p= p%>%as.data.frame()
names(p)= c('probability')
X= cbind(X,p)
ggplot(X, aes(x=`can`, y=`final`, z=`probability`))+ 
geom_point()+ stat_density2d(color='red')
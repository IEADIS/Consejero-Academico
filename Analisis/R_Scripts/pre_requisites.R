library(rgl)
library(psych)
library(snapshot)
library(magick)
library(ggplot2)
library(plotly)
source("Analisis/R_Scripts/utils.R")



#Getting Data for PIMB, LCAL, MBDA

NOTES = "Data/notas_clean.csv"

allData <- read.csv(NOTES, header = TRUE)
dataPIMB <- allData[ allData$Codigo.Asignatura == 'MBDA', ]
dataLCAL <- allData[ allData$Codigo.Asignatura == 'POOB', ]
dataMBDA <- allData[ allData$Codigo.Asignatura == 'PDSW ', ]

dataPIMBPre <- dataPIMB[ dataPIMB$Nota.Final >= 30, ]
dataLCALPre <- dataLCAL[ dataLCAL$Nota.Final >= 30, ]
dataMBDAPre <- dataMBDA

#Group Up with the same Student Identifier number
DataReq <- merge(x = dataPIMBPre, y = dataLCALPre, by="Identidad.Estudiante")
DataReq <- merge(x = DataReq, y = dataMBDAPre, by="Identidad.Estudiante")

keeps <- c("Identidad.Estudiante", "Codigo.Asignatura.x", "Nota.Final.x", "Codigo.Asignatura.y", "Nota.Final.y", "Codigo.Asignatura", "Nota.Final", "Programa.Estudiante")
DataForReq <- DataReq[keeps]

colnames(DataForReq)[2:3] <- c("Codigo.Asignatura.MBDA", "Nota.Final.MBDA")
colnames(DataForReq)[4:5] <- c("Codigo.Asignatura.POOB", "Nota.Final.POOB")
colnames(DataForReq)[6:7] <- c("Codigo.Asignatura.PDSW", "Nota.Final.PDSW")

Notes <- data.frame(MBDA = DataForReq$Nota.Final.MBDA, POOB = DataForReq$Nota.Final.POOB, PDSW = DataForReq$Nota.Final.PDSW)
summary(Notes)

x <- Notes$MBDA
z <- Notes$POOB
y <- Notes$PDSW

rgl_init()
rgl.spheres(x, y, z, r = 1, color = "#D95F02") 
rgl_add_axes(x, y, z, xlab = 'MBDA', ylab = 'PDSW', zlab = 'POOB', show.bbox = FALSE)
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

summary(fit)






#--------------------------Exploracion-----------------------------
allData <- read.csv(NOTES, header = TRUE)
allData <- allData[allData$Periodo.Academico != "2013-i",]
allData$Periodo.Academico <- droplevels(allData$Periodo.Academico)

#Number of Students
barplot(table(allData$Periodo.Academico), main = "Numero de Estudiantes", names.arg = levels(allData$Periodo.Academico))
  
#Number of Students by Program
plot_ly(allData, x = ~Periodo.Academico, y = ~Programa.Estudiante, type = 'bar', name = 'SF Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')


plotProgramsByYear(allData,"2009")
plotProgramsByYear(allData,"2010")
plotProgramsByYear(allData,"2011")
plotProgramsByYear(allData,"2012")
plotProgramsByYear(allData,"2013")
plotProgramsByYear(allData,"2014")
plotProgramsByYear(allData,"2015")
plotProgramsByYear(allData,"2016")
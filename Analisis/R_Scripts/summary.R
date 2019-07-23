library(psych)
library(webshot)
library(plotly)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

allData <- read.csv(NOTES, header = TRUE)


inter <- c("2009-I", "2010-I", "2011-I", "2012-I", "2013-I", "2014-I", "2015-I", "2016-I" )

semestre <- c("2009-1", "2009-2", "2010-1", "2010-2", "2011-1", "2011-2", "2012-1", "2012-2", "2013-1",
              "2013-2", "2014-1", "2014-2", "2015-1", "2015-2", "2016-1", "2016-2")

semestre1 <- c("2009-1", "2010-1", "2011-1", "2012-1", "2013-1", "2014-1", "2015-1", "2016-1")

semestre2 <- c("2009-2", "2010-2", "2011-2", "2012-2", "2013-2", "2014-2", "2015-2", "2016-2")


allDataNoCan <- allData[ allData$Nota.Final != 0, ]
allDataInter <- allData[ allData$Periodo.Academico %in% inter, ]
allDataSeme <- allData[ allData$Periodo.Academico %in% semestre, ]
allDataSeme1 <- allData[ allData$Periodo.Academico %in% semestre1, ]
allDataSeme2 <- allData[ allData$Periodo.Academico %in% semestre2, ]

#--------Graficos Normales sin cancelaciones--------

sistemas <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'SISTEMAS', ]
matematicas <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'MATEMATICAS', ]
humanidades <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronica <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'ELECTRONICA', ]
electrica <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'ELECTRICA', ]
industrial <- allDataNoCan[ allDataNoCan$Area.Asignatura == 'INDUSTRIAL', ]

infoData( sistemas , "plots/html/img/notasVsSemsSistemas.png")
infoData( matematicas , "plots/html/img/notasVsSemsMatematicas.png")
infoData( humanidades , "plots/html/img/notasVsSemsHumanidades.png")
infoData( electronica , "plots/html/img/notasVsSemsElectronica.png")
infoData( electrica , "plots/html/img/notasVsSemsElectrica.png")
infoData( industrial , "plots/html/img/notasVsSemsIndustrial.png")

#--------Graficos de cancelaciones sin intersemestral--------

sisCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'SISTEMAS', ]
matCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'MATEMATICAS', ]
humCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronicaCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'ELECTRONICA', ]
electricaCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'ELECTRICA', ]
indCanSem <- allDataSeme[ allDataSeme$Area.Asignatura == 'INDUSTRIAL', ]

infoDataCancel( sisCanSem , "plots/html/img/cancelVsSemsSistemas.png")
infoDataCancel( matCanSem , "plots/html/img/cancelVsSemsMatematicas.png")
infoDataCancel( humCanSem , "plots/html/img/cancelVsSemsHumanidades.png")
infoDataCancel( electronicaCanSem , "plots/html/img/cancelVsSemsElectronica.png")
infoDataCancel( electricaCanSem , "plots/html/img/cancelVsSemsElectrica.png")
infoDataCancel( indCanSem , "plots/html/img/cancelVsSemsIndustrial.png")

#--------Graficos de cancelaciones primer semestre del a?o--------

sisCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'SISTEMAS', ]
matCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'MATEMATICAS', ]
humCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronicaCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'ELECTRONICA', ]
electricaCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'ELECTRICA', ]
indCanSem <- allDataSeme1[ allDataSeme1$Area.Asignatura == 'INDUSTRIAL', ]

infoDataCancel( sisCanSem , "plots/html/img/PrimSemcancelVsSemsSistemas.png")
infoDataCancel( matCanSem , "plots/html/img/PrimSemcancelVsSemsMatematicas.png")
infoDataCancel( humCanSem , "plots/html/img/PrimSemcancelVsSemsHumanidades.png")
infoDataCancel( electronicaCanSem , "plots/html/img/PrimSemcancelVsSemsElectronica.png")
infoDataCancel( electricaCanSem , "plots/html/img/PrimSemcancelVsSemsElectrica.png")
infoDataCancel( indCanSem , "plots/html/img/PrimSemcancelVsSemsIndustrial.png")

#--------Graficos de cancelaciones segundo semestre del a?o--------

sisCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'SISTEMAS', ]
matCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'MATEMATICAS', ]
humCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronicaCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'ELECTRONICA', ]
electricaCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'ELECTRICA', ]
indCanSem <- allDataSeme2[ allDataSeme2$Area.Asignatura == 'INDUSTRIAL', ]

infoDataCancel( sisCanSem , "plots/html/img/SegSemcancelVsSemsSistemas.png")
infoDataCancel( matCanSem , "plots/html/img/SegSemcancelVsSemsMatematicas.png")
infoDataCancel( humCanSem , "plots/html/img/SegSemcancelVsSemsHumanidades.png")
infoDataCancel( electronicaCanSem , "plots/html/img/SegSemcancelVsSemsElectronica.png")
infoDataCancel( electricaCanSem , "plots/html/img/SegSemcancelVsSemsElectrica.png")
infoDataCancel( indCanSem , "plots/html/img/SegSemcancelVsSemsIndustrial.png")

#--------------- Anomaly detection --------------------

sistemas <- allData[ allData$Area.Asignatura == 'SISTEMAS', ]
matematicas <- allData[ allData$Area.Asignatura == 'MATEMATICAS', ]
humanidades <- allData[ allData$Area.Asignatura == 'HUMANIDADES E IDIOMAS', ]
electronica <- allData[ allData$Area.Asignatura == 'ELECTRONICA', ]
electrica <- allData[ allData$Area.Asignatura == 'ELECTRICA', ]
industrial <- allData[ allData$Area.Asignatura == 'INDUSTRIAL', ]

anomalyDetection( sistemas , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsSistemas.html")
anomalyDetection( matematicas , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsMatematicas.html")
anomalyDetection( humanidades , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsHumanidades.html")
anomalyDetection( electronica , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsElectronica.html")
anomalyDetection( electrica , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsElectrica.html")
anomalyDetection( industrial , "C:/Users/Sebastian/Google Drive/Artificial Intelligence/APAU/APAU/Proyecto/plots/html/Anom_notasVsSemsIndustrial.html")



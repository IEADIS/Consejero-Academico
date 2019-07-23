library(psych)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

allData <- read.csv(NOTES, header = TRUE)

dataPimb <- allData[ allData$Codigo.Asignatura == 'PIMB', ]
dataLcal <- allData[ allData$Codigo.Asignatura == 'LCAL ', ]
dataPimo <- allData[ allData$Codigo.Asignatura == 'PIMO', ]
dataMbda <- allData[ allData$Codigo.Asignatura == 'MBDA', ]
dataArsw <- allData[ allData$Codigo.Asignatura == 'ARSW ', ]
dataPoob <- allData[ allData$Codigo.Asignatura == 'POOB', ]
dataTpro <- allData[ allData$Codigo.Asignatura == 'TPRO ', ]
dataCald <- allData[ allData$Codigo.Asignatura == 'CALD ', ]
dataEsti <- allData[ allData$Codigo.Asignatura == 'ESTI ', ]

#Separate the data
pimb <- data.frame( Tercio1Pimb = dataPimb$nota1, Tercio2Pimb = dataPimb$nota2, 
                     Tercio3Pimb = dataPimb$nota3 )

lcal <- data.frame( Tercio1Lcal = dataLcal$nota1, Tercio2Lcal = dataLcal$nota2, 
                    Tercio3Lcal = dataLcal$nota3 )

pimo <- data.frame( Tercio1Pimo = dataPimo$nota1, Tercio2Pimo = dataPimo$nota2, 
                    Tercio3Pimo = dataPimo$nota3 )

mbda <- data.frame( Tercio1Mbda = dataMbda$nota1, Tercio2Mbda = dataMbda$nota2, 
                    Tercio3Mbda = dataMbda$nota3 )

arsw <- data.frame( Tercio1Arsw = dataArsw$nota1, Tercio2Arsw = dataArsw$nota2, 
                    Tercio3Arsw = dataArsw$nota3 )

poob <- data.frame( Tercio1Poob = dataPoob$nota1, Tercio2Poob = dataPoob$nota2, 
                    Tercio3Poob = dataPoob$nota3 )

tpro <- data.frame( Tercio1Tpro = dataTpro$nota1, Tercio2Tpro = dataTpro$nota2, 
                    Tercio3Tpro = dataTpro$nota3 )

cald <- data.frame( Tercio1Cald = dataCald$nota1, Tercio2Cald = dataCald$nota2, 
                    Tercio3Cald = dataCald$nota3 )

esti <- data.frame( Tercio1Esti = dataEsti$nota1, Tercio2Esti = dataEsti$nota2, 
                    Tercio3Esti = dataEsti$nota3 )

dataMbda <- dataMbda[ dataMbda$Estado.Asignatura != 'Cancelación' & dataMbda$Estado.Asignatura != 'Retiro'
                      & dataMbda$Nota.Final != 0,]

#Data analysis
# dev.new()
# pairs.panels(pimb)
# pairs.panels(pimo)
# pairs.panels(lcal)
# pairs.panels(mbda)
# pairs.panels(arsw)
# pairs.panels(poob)
# pairs.panels(tpro)
# pairs.panels(cald)
# pairs.panels(esti)

#Separate the data between a training set (70%) and a test set (30%)

m <- length(pimb$Tercio1Pimb)
pimb <- pimb[sample(nrow( pimb )), ]
trainPimb <- data.frame( Tercio1Pimb = pimb$Tercio1Pimb[1:round(m*0.7)], Tercio2Pimb = pimb$Tercio2Pimb[1:round(m*0.7)], 
                         Tercio3Pimb = pimb$Tercio3Pimb[1:round(m*0.7)] )
testPimb <- data.frame( Tercio1Pimb = pimb$Tercio1Pimb[round(m*0.7)+1:round(m*0.3)], 
                        Tercio2Pimb = pimb$Tercio2Pimb[round(m*0.7)+1:round(m*0.3)], 
                        Tercio3Pimb = pimb$Tercio3Pimb[round(m*0.7)+1:round(m*0.3)] )

m <- length(pimo$Tercio1Pimo)
pimo <- pimo[sample(nrow( pimo )), ]
trainPimo <- data.frame( Tercio1Pimo = pimo$Tercio1Pimo[1:round(m*0.7)], Tercio2Pimo = pimo$Tercio2Pimo[1:round(m*0.7)], 
                         Tercio3Pimo = pimo$Tercio3Pimo[1:round(m*0.7)] )
testPimo <- data.frame( Tercio1Pimo = pimo$Tercio1Pimo[round(m*0.7)+1:round(m*0.3)], 
                        Tercio2Pimo = pimo$Tercio2Pimo[round(m*0.7)+1:round(m*0.3)], 
                        Tercio3Pimo = pimo$Tercio3Pimo[round(m*0.7)+1:round(m*0.3)] )

m <- length(mbda$Tercio1Mbda)
mbda <- mbda[sample(nrow( mbda )), ]
trainMbda <- data.frame( Tercio1Mbda = mbda$Tercio1Mbda[1:round(m*0.7)], Tercio2Mbda = mbda$Tercio2Mbda[1:round(m*0.7)], 
                         Tercio3Mbda = mbda$Tercio3Mbda[1:round(m*0.7)] )
testMbda <- data.frame( Tercio1Mbda = mbda$Tercio1Mbda[round(m*0.7)+1:round(m*0.3)], 
                        Tercio2Mbda = mbda$Tercio2Mbda[round(m*0.7)+1:round(m*0.3)], 
                        Tercio3Mbda = mbda$Tercio3Mbda[round(m*0.7)+1:round(m*0.3)] )

m <- length(poob$Tercio1Poob)
poob <- poob[sample(nrow( poob )), ]
trainPoob <- data.frame( Tercio1Poob = poob$Tercio1Poob[1:round(m*0.7)], Tercio2Poob = poob$Tercio2Poob[1:round(m*0.7)], 
                         Tercio3Poob = poob$Tercio3Poob[1:round(m*0.7)] )
testPoob <- data.frame( Tercio1Poob = poob$Tercio1Poob[round(m*0.7)+1:round(m*0.3)], 
                        Tercio2Poob = poob$Tercio2Poob[round(m*0.7)+1:round(m*0.3)], 
                        Tercio3Poob = poob$Tercio3Poob[round(m*0.7)+1:round(m*0.3)] )

#Training

modelPimb <- lm( Tercio3Pimb ~ . , trainPimb )
modelPimo <- lm( Tercio3Pimo ~ . , trainPimo )
modelMbda <- lm( Tercio3Mbda ~ . , trainMbda )
modelPoob <- lm( Tercio3Poob ~ . , trainPoob )

#Evaluate the model

summary(modelPimb)
summary(modelPimo)
summary(modelMbda)
summary(modelPoob)

#Testing

write("---- Test Pimb ----", stdout())

predTestPimb <- predict( modelPimb, testPimb )
predTrainPimb <- predict( modelPimb, trainPimb )

sum((predTestPimb - testPimb$Tercio3Pimb)^2) / length(predTestPimb)
sum((predTrainPimb - trainPimb$Tercio3Pimb)^2) / length(predTrainPimb)

write("---- Test Pimo ----", stdout())

predTestPimo <- predict( modelPimo, testPimo )
predTrainPimo <- predict( modelPimo, trainPimo )

sum((predTestPimo - testPimo$Tercio3Pimo)^2) / length(predTestPimo)
sum((predTrainPimo - trainPimo$Tercio3Pimo)^2) / length(predTrainPimo)

write("---- Test Mbda ----", stdout())
predTestMbda <- predict( modelMbda, testMbda )
predTrainMbda <- predict( modelMbda, trainMbda )

sum((predTestMbda - testMbda$Tercio3Mbda)^2) / length(predTestMbda)
sum((predTrainMbda - trainMbda$Tercio3Mbda)^2) / length(predTrainMbda)

write("---- Test Poob ----", stdout())
predTestPoob <- predict( modelPoob, testPoob )
predTrainPoob <- predict( modelPoob, trainPoob )

sum((predTestPoob - testPoob$Tercio3Poob)^2) / length(predTestPoob)
sum((predTrainPoob - trainPoob$Tercio3Poob)^2) / length(predTrainPoob)

x <- dataMbda$nota1
z <- dataMbda$nota2
y <- dataMbda$nota3

dataNoCan <- data.frame( tercio1 = x, tercio2 = z, tercio3 = y )

data <- allData[ allData$Codigo.Asignatura == 'MBDA',  ]
dataN <- data.frame( tercio1 = data$nota1, tercio2 = data$nota2, tercio3 = data$nota3 )

fitAll <- lm( tercio3 ~ ., dataN )
fit <- lm( tercio3 ~ ., dataNoCan )

dataCancel <- data[ (data$Estado.Asignatura == 'Cancelación' & data$Nota.Final < 30), ]
dataCancelN <- data.frame( tercio1 = dataCancel$nota1, tercio2 = dataCancel$nota2, 
                           tercio3 = dataCancel$nota3 )
predCan <- predict( fit, dataCancelN  )
sum(predCan >= 30) / length(predCan)

dataLose <- data[ (data$Estado.Asignatura != 'Cancelación' & data$Nota.Final < 30 & data$Nota.Final != 0 ), ]
dataLoseN <- data.frame( tercio1 = dataLose$nota1, tercio2 = dataLose$nota2, 
                           tercio3 = dataLose$nota3 )
predLose <- predict( fit, dataLoseN  )


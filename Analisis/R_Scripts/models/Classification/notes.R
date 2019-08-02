allData <- read.csv(NOTES, header = TRUE)
asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)

data.asig <- classif_utils.asig.adq(data = allData, asig = asig.sistemas, time.start = "2009", time.end = "2016")
data.trans <- classif_utils.asig.trans(data.asig)

data.train <- data.trans[ -grepl( toString(2016), data.trans$Periodo ), ]
data.test <- data.trans[ grepl( toString(2016), data.trans$Periodo ), ]


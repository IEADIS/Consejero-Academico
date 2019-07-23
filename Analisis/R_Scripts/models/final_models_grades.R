library(psych)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"


# =======================================================================================================
# ====================================== GRADES ADQUIS ==================================================
# =======================================================================================================

# ADQUISITION OF DATA BY ASIGNATURE AND TIME WINDOW [TIME.START < TIME.END]
asig.adq <- function(data, asig, time.start, time.end){
  data.asig <- data[ data$Codigo.Asignatura %in% asig, ] # ASIGNATURE
  data.asig <- data.asig[ data.asig$Periodo.Academico != "2013-i", ] # WEIRD DATA
  data.asig <- data.asig[ data.asig$Nota.Final != 0, ] # NO ACCOMP
  
  total.ind <- c()
  for (i in c(time.start:time.end)) {
    ind <- grep(i,data.asig$Periodo.Academico)
    total.ind <- c(total.ind,ind)
  }
  data.asig <- data.asig[total.ind,] # TIME WINDOW
  return(data.asig)
}

asig.trans <- function(data){
  data.trans <- data.frame( Asig = data$Codigo.Asignatura, Grade1 = data$nota1, Grade2 = data$nota2, 
                      Grade3 = data$nota3 ) # VARIABLES SELECTION
  return(data.trans)
}

asig.part <- function(data.trans){
  m <- nrow(data.trans) # LENGTH OF DATA
  data.trans <- data.trans[sample(m), ] # RANDOM SAMPLES
  
  data.train <- data.trans[1:round(m*0.7),] # TRAINING DATA - 70%
  data.test <- data.trans[round(m*0.7)+1:round(m*0.3),] # TEST DATA - 30%
  return(list(train = data.train, test = data.test))
}


# =======================================================================================================
# ====================================== MODEL TRAIN/TEST ===============================================
# =======================================================================================================

lm.train <- function(data.train){
  lm.model <- lm( Grade3 ~ . , data.train )
  return(lm.model)
}

lm.test <- function(lm.model,data.test){
  pred.test <- predict( lm.model, data.test )
  err <- sqrt(sum((pred.test - data.test$Grade3)^2) / (length(pred.test)-1))
  lin.cor <- cor(pred.test,data.test$Grade3)
  return(list(pred = pred.test, std.err = err, r.squared = lin.cor))
}


# =======================================================================================================
# ========================================= MODEL PLOTS =================================================
# =======================================================================================================

plots.lm <- function(asig,model,predictions,actual,time.lapse){
  pdf(paste("Analisis/Plots/Models/","linearmodel-",asig,time.lapse,".pdf",sep = ""),width=6,height=4,paper='special')
  plot(model, main = paste(asig,"Training Results"))
  plot(actual,
       predictions, main = paste(asig,"Test Results"),
       ylab = 'Predictions', xlab = 'Actual Values - Grade3')
  dev.off()
}


# =======================================================================================================
# ======================================== MODEL DEPLOY =================================================
# =======================================================================================================

deploy.lm <- function(asig,time.start,time.end){
  set.seed(123)
  # ADQUISITION
  allData <- read.csv(NOTES, header = TRUE)
  data.asig <- asig.adq(data = allData, asig = asig, time.start = time.start, time.end = time.end)
  if (nrow(data.asig[data.asig$Codigo.Asignatura %in% asig,]) < 10) {
    return(NULL)
  }
  data.trans <- asig.trans(data.asig)
  data.part <- asig.part(data.trans)
  print('=================== DATA ===================')
  print(head(data.asig))
  
  # TRAINING
  model <- lm.train(data.part$train[,-1])
  print('=================== MODEL ===================')
  print(summary(model))
  
  # TESTING
  test.result <- lm.test(model,data.part$test[,-1])
  print('=================== STANDARD ERROR ===================')
  print(test.result$std.err)
  
  print('=================== CORRELATION COEF - RSQUARED ===================')
  print(test.result$r.squared)
  
  # FINAL PLOTS
  plots.lm(unique(data.part$test$Asig), model, test.result$pred,
           data.part$test[,-1]$Grade3,paste(time.start,time.end,sep = '-'))
  
  # SAVING DATA
  save.data <- data.frame()
  ss <- coef(model)
  model.data <- data.frame(Asignature = unique(data.part$test$Asig),
                           TimeLapse = paste(time.start,time.end,sep = '-'),
                           Intercept = ss[1], Grade1 = ss[2], Grade2 = ss[3])
  return(model.data)
}

# =======================================================================================================
# ========================================= ISIS MODELS =================================================
# =======================================================================================================

isis.models <- function(){
  # LINEAR MODEL ISIS
  asig.sistemas <- unique(allData[allData$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
  asig.sistemas.tec <- unique(allData[allData$Area.Asignatura %in% c('ELECTIVAS TECNICAS-SISTEMAS'),]$Codigo.Asignatura)
  asig.humanidades <- unique(allData[allData$Area.Asignatura %in% c('HUMANIDADES E IDIOMAS'),]$Codigo.Asignatura)
  asig.matematicas <- unique(allData[allData$Area.Asignatura %in% c('AREA DE MATEMaTICAS'),]$Codigo.Asignatura)
  asig.ciencias <- unique(allData[allData$Area.Asignatura %in% c('CIENCIAS NATURALES'),]$Codigo.Asignatura)
  years.ini <- c(2009,2010,2011,2012)
  # ASIG SISTEMAS
  models.data <- data.frame(Asignature = c(), TimeLapse = c(), Intercept = c(), Grade1 = c(), Grade2 = c())
  for (year in years.ini) { # BY YEAR WINDOW
    for (asignature in asig.sistemas) { # BY ASIGNATURE
      asig.model <- deploy.lm(asignature,toString(year),toString(year+4))
      models.data <- rbind(models.data, asig.model)
    }
  }
  write.csv(models.data, file = 'Data/Models/isis_lm.csv')
}

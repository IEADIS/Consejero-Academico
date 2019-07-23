library(plotly)
library(plotly)
library(dplyr)
library(cluster)
library(ggplot2)
library(webshot)
source("Analisis/R_Scripts/utils.R")

NOTES = "Data/notas_clean.csv"

# =======================================================================================================
# ================================== KMEANS CALC & PLOT =================================================
# =======================================================================================================

measures_kmeans <- function(model_obj){
  dis <- dist(dataGroup)^2
  sil <- silhouette(model_obj$cluster, dis)
  df_bss <- length(model_obj$size)-1
  df_wss <- length(model_obj$cluster)-length(model_obj$size)
  df_tss <- df_bss + df_wss
  mean_squares <- c(model_obj$betweenss/df_bss, model_obj$tot.withinss/df_wss, NA)
  f_ratio <- c(mean_squares[1]/mean_squares[2],NA,NA)
  anova <- data.frame(SOURCE = c("BSS","WSS","TSS"), DF = c(df_bss,df_wss,df_tss), 
                      SUM_OF_SQUARES = c(model_obj$betweenss,model_obj$tot.withinss,model_obj$totss), 
                      MEAN_SQUARES = mean_squares,  F_RATIO = f_ratio, 
                      P_VALUE = c(1-pf(f_ratio[1],df_bss,df_wss),NA,NA))
  return(list(silhouette = sil, anova_table = anova))
}

BoxPlotByK <- function(model){
  k <- length(model$size)
  pdf(paste("BoxPlotGroup",k,"k.pdf",sep = ""),width=6,height=4,paper='special')
  plot(measures_kmeans(model)$silhouette)
  colorBox <- c("red","red","red","blue","blue","blue","blue","darkgreen","darkgreen","darkgreen","darkgreen","gold","gold","gold","gold")
  for (i in 1:k){
    boxplot.matrix(dataGroup[model$cluster == i,], names = asig, las = 2, col = colorBox)
  }
  dev.off()
}

# =======================================================================================================
# ==================================== ISIS DATA - KMEANS ===============================================
# =======================================================================================================

allData <- read.csv(NOTES, header = TRUE)

asig <- c("TSOR ", "PRON ", "AREM ", "PDSW ", "ARSW ", "COSW ", 
          "SOSW ", "ARQC", "SOPC ", "FRED ", "SEGI")

dataGroup <- getData(allData,asig)

set.seed(2017)

goodness <- (nrow(dataGroup)-1)*sum(apply(dataGroup,2,var))
mod <- list(rep(list(NULL),9))

for ( i in 1:10 ) {
  model <- kmeans( dataGroup , centers = i , iter.max = 30 );
  mod[[i]] <- model;
  goodness[i] <- model$betweenss / model$totss;
}

plot(1:10, goodness, type="b", xlab="Number of Clusters",
     ylab="BSS/TSS")


#Select Best Model
measures_2k <- measures_kmeans(mod[[2]])
measures_3k <- measures_kmeans(mod[[3]])
measures_4k <- measures_kmeans(mod[[4]])


#Hipotesis
BoxPlotByK(mod[[3]])


#ByElbow
BoxPlotByK(mod[[2]])
BoxPlotByK(mod[[3]])


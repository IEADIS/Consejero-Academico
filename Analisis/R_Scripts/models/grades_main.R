source("Analisis/R_Scripts/models/Classification/classification.R")
source("Analisis/R_Scripts/models/Regression/regression.R")
source("Analisis/R_Scripts/models/approach_selection.R")
source("Analisis/R_Scripts/models/approach_benefit.R")

# =====================================================================================================================
# =========================================== DATA ADQUISITION ========================================================
# =====================================================================================================================

data.grades <- read.csv(NOTES, header = TRUE)
asigntatures.consider <- unique(data.grades[data.grades$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
years.consider <- strtoi(unique(sub("-.*","",droplevels(data.grades[data.grades$Codigo.Asignatura %in% asigntatures.consider,]$Periodo.Academico))))
years.consider <- c(years.consider[-c(1,2)],max(years.consider)+1)

# =====================================================================================================================
# ============================================ MODELS TRAINING ========================================================
# =====================================================================================================================

reg.models <- regress.bestModel.train(allData = data.grades,asignatures = asigntatures.consider)
reg.models <- Filter(length,reg.models) # EMPTY MODELS REMOVED
cla.models <- classif.bestModel.train(allData = data.grades,asignatures = asigntatures.consider)
cla.models <- Filter(length,cla.models) # EMPTY MODELS REMOVED


# =====================================================================================================================
# =================================== BEST LEARNING APPROACH SELECTION ================================================
# =====================================================================================================================

best.models <- abs.models.selection(data.grades, reg.models, cla.models, asigntatures.consider, years.consider)

# =====================================================================================================================
# ====================================== MODELS & APPROACHS BENEFIT ===================================================
# =====================================================================================================================

abs.bestModel.benefit.general(data.grades,asigntatures.consider)
abs.bestModel.benefit.asig(data.grades,asigntatures.consider)
regress.bestModel.benefit.general(data.grades,asigntatures.consider)
regress.bestModel.benefit.asig(data.grades,asigntatures.consider)
classif.bestModel.benefit.general(data.grades,asigntatures.consider)
classif.bestModel.benefit.asig(data.grades,asigntatures.consider)
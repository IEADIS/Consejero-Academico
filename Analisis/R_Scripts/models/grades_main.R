source("Analisis/R_Scripts/models/Classification/classification.R")
source("Analisis/R_Scripts/models/Regression/regression.R")
source("Analisis/R_Scripts/models/approach_selection.R")
source("Analisis/R_Scripts/models/approach_benefit.R")

# =====================================================================================================================
# =========================================== DATA ADQUISITION ========================================================
# =====================================================================================================================

data.grades <- read.csv(NOTES, header = TRUE)
asigntatures.consider <- unique(data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS') ,]$Codigo.Asignatura)
asigntatures.consider <- asigntatures.consider[!laply( asigntatures.consider, function(val){ return( grepl("\\(", val) || grepl("\\)", val) ) } ) ]
years.consider <- strtoi(unique(sub("-.*","",droplevels(data.grades[data.grades$Codigo.Asignatura %in% asigntatures.consider,]$Periodo.Academico))))
years.consider <- c(years.consider[-c(1,2)],max(years.consider)+1)

# =====================================================================================================================
# ============================================ MODELS TRAINING ========================================================
# =====================================================================================================================
set.seed(123)
write("# TRAIN CLASSIFICATION", stdout())
# asigntatures.consider[151:length(asigntatures.consider)]
cla.models <- classif.bestModel.train(allData = data.grades,asignatures = asigntatures.consider)
cla.models <- Filter(length,cla.models) # EMPTY MODELS REMOVED
write("# TRAIN REGRESSION", stdout())
reg.models <- regress.bestModel.train(allData = data.grades,asignatures = asigntatures.consider)
reg.models <- Filter(length,reg.models) # EMPTY MODELS REMOVED

# =====================================================================================================================
# =================================== BEST LEARNING APPROACH SELECTION ================================================
# =====================================================================================================================

write("# BEST SELECTION", stdout())
best.models <- abs.models.selection(data.grades, reg.models, cla.models, asigntatures.consider, years.consider)

# =====================================================================================================================
# ====================================== MODELS & APPROACHS BENEFIT ===================================================
# =====================================================================================================================
write("# PLOTS", stdout())
data.grades <- data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS'),]

write("# PLOTS - GENERAL", stdout())
abs.bestModel.benefit.general(data.grades,asigntatures.consider)
abs.bestModel.benefit.asig(data.grades,asigntatures.consider)

write("# PLOTS - REGRESSION", stdout())
regress.bestModel.benefit.general(data.grades,asigntatures.consider)
regress.bestModel.benefit.asig(data.grades,asigntatures.consider)

write("# PLOTS - CLASSIFICATION", stdout())
classif.bestModel.benefit.general(data.grades,asigntatures.consider)
classif.bestModel.benefit.asig(data.grades,asigntatures.consider)

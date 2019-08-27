source("Analisis/R_Scripts/grades/grades_packages.R")
source("Analisis/R_Scripts/grades/grades_utils.R")

source("Analisis/R_Scripts/grades/Classification/classification.R")
source("Analisis/R_Scripts/grades/Regression/regression.R")

source("Analisis/R_Scripts/grades/approach_selection.R")
source("Analisis/R_Scripts/grades/approach_benefit.R")

wd <- commandArgs(trailingOnly = TRUE)
root.dir <- paste(wd, collapse = ' ')
setwd(root.dir)
getwd()
# =====================================================================================================================
# =========================================== DATA ADQUISITION ========================================================
# =====================================================================================================================

paste('Loading Grades:',NOTES)
data.grades <- read.csv(NOTES, header = TRUE)

# asignatatures.consider <- unique(data.grades[data.grades$Area.Asignatura %in% c('SISTEMAS'),]$Codigo.Asignatura)
asignatatures.consider <- unique(data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS') ,]$Codigo.Asignatura)
asignatatures.consider <- asigntatures.consider[!laply( asigntatures.consider, function(val){ return( grepl("\\(", val) || grepl("\\)", val) ) } ) ]

years.consider <- strtoi(unique(sub("-.*","",droplevels(data.grades[data.grades$Codigo.Asignatura %in% asignatatures.consider,]$Periodo.Academico))))
years.consider <- c(years.consider[-c(1,2)],max(years.consider)+1)

# =====================================================================================================================
# ============================================ MODELS TRAINING ========================================================
# =====================================================================================================================
set.seed(123)
# asigntatures.consider[151:length(asigntatures.consider)]
'Regression Training ...'
reg.models <- regress.bestModel.train(allData = data.grades,asignatures = asignatatures.consider)
reg.models <- Filter(length,reg.models) # EMPTY MODELS REMOVED
'Classification Training ...'
cla.models <- classif.bestModel.train(allData = data.grades,asignatures = asignatatures.consider)
cla.models <- Filter(length,cla.models) # EMPTY MODELS REMOVED


# =====================================================================================================================
# =================================== BEST LEARNING APPROACH SELECTION ================================================
# =====================================================================================================================

'Best Model Selection...'
best.models <- abs.models.selection(data.grades, reg.models, cla.models, asignatatures.consider, years.consider)

# =====================================================================================================================
# ====================================== MODELS & APPROACHS BENEFIT ===================================================
# =====================================================================================================================
'Final Plots..'
data.grades <- data.grades[data.grades$Programa.Estudiante %in% c('INGENIERIA DE SISTEMAS'),]

abs.bestModel.benefit.general(data.grades,asignatatures.consider)
abs.bestModel.benefit.asig(data.grades,asignatatures.consider)

regress.bestModel.benefit.general(data.grades,asignatatures.consider)
regress.bestModel.benefit.asig(data.grades,asignatatures.consider)

classif.bestModel.benefit.general(data.grades,asignatatures.consider)
classif.bestModel.benefit.asig(data.grades,asignatatures.consider)

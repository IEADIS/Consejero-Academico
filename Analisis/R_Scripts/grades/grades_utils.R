# =======================================================================================================
# ========================================= PARAMETERS ==================================================
# =======================================================================================================

params <- yaml.load_file('hparams.yaml')

NOTES = params$relevant.files$grades
LM_MODELS = params$models.dir$regression
CL_MODELS = params$models.dir$classification
ABS_MODELS = params$models.dir$absolute

PLOTS_DIR_REG <- params$plots.dir$regression
PLOTS_DIR_CLA <- params$plots.dir$classification
PLOTS_DIR_ABS <- params$plots.dir$absolute

# =======================================================================================================
# ======================================= MODELS SAVE/LOAD ==============================================
# =======================================================================================================

save.model <- function(model,file.name.path){
  saveRDS(model, file = file.name.path)
}

load.models <- function(files.path){
  models.files <- list.files(path = files.path)
  models <- list()
  for (file.model in models.files) {
    model.name <- gsub(".rds","",file.model)
    models[[model.name]] <- load.model(paste(files.path,file.model,sep = ""))
  }
  return(models)
}

load.model <- function(file.model){
  return(readRDS(file.model))
}
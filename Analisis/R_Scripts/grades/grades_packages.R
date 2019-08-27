# =====================================================================================================================
# ========================================= INSTALL/LOAD METHOD =======================================================
# =====================================================================================================================

load.package <- function(package,usr.packages = installed.packages()[,"Package"]){
  if (!(package %in% usr.packages)) install.packages(package, dependencies=TRUE, INSTALL_opts = c('--no-lock'))
  library(package,character.only = TRUE)
}

load.package.github <- function(package.dir,usr.packages = installed.packages()[,"Package"]){
  package <- gsub(".*/","",package.dir)
  if (!(package %in% usr.packages)) devtools::install_github(package.dir)
  library(package,character.only = TRUE)
}

# =====================================================================================================================
# =========================================== LOAD LIBRARIES ========================================================
# =====================================================================================================================

necesarry.packages <- c('yaml',
                        'caret',
                        'plotly',
                        'scatterplot3d',
                        'devtools',
                        'cowplot')
invisible(sapply(necesarry.packages, load.package))

# SPECIAL LIBRARIES
load.package.github("laresbernardo/lares")






#define shared data directory for this project
data_dir = "~/Library/CloudStorage/Dropbox/UMB/Slejko Project/MM Data/MM_DATA/data/"

#set knitr options
options(knitr.table.format="html")
knitr::opts_chunk$set(
  echo = FALSE,
  error = FALSE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)

# Save Function
#save(data,
     #file=paste0(data_dir, "xxx.RData"))
## load functions and data
#source(paste0("code/00_project-setup.R"))
#load(file = paste0(data_dir, "XXX.RData"))


# Packages to load
packages <- c(
  "tidyverse", "readxl", "dplyr", "ggplots"
)
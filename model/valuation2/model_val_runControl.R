# Actuarial valuation for PERF A with 2-tier simplification

rm(list = ls())
source("libraries.R")


#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************
## File path of the run control file

# Path to run control file
dir_runControl <- "model/"
fn_runControl  <- "RunControl.xlsx"
filePath_runControl <- paste0(dir_runControl, fn_runControl)

# Path to amortization and asset smoothing info
dir_planInfo <- "inputs/data_proc/"
filePath_planInfo <- paste0(dir_planInfo, "Data_CalPERS_planInfo_AV2018.RData")

# Output folder  
dir_outputs_val <- "model/valuation2/outputs_val/"


## Import global parameters
# Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
#   filter(!is.na(init_year)) %>% 
#   as.list
 
## Import valuation parameters
# val_paramlist <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
#   filter(!is.na(val_name), include == TRUE) %>% 
#   as.list

## Additinal global variables 

# age and entry age ranges
# Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
# Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)

val_runList <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
  filter(include == TRUE)


#*******************************************************************************
#                    ### Run valuations   ####                      
#*******************************************************************************
#source("model/valuation2/model_val_create_Dev_2tiers(2).R")
source("model/valuation2/model_val_master_multiTier(1).R")

# val_name_run <- val_runList$val_name[1]

for (val_name_run in val_runList$val_name){
  create_val_2tiers(val_name_run)
}











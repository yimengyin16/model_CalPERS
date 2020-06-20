


#*******************************************************************************
#                           ### Initialization ####                      
#*******************************************************************************

## Cleaning up memory
rm(list = ls())
suppressMessages(gc())


## Loading packages
source("NYSERS_0_libraries.R")


## Setting folder paths
dir_modelOutputs <- "Outputs/"



#*******************************************************************************
#                           ### Model Parameters ####                      
#*******************************************************************************
## File path of the run control file
folder_run <- "."
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

## Import global parameters
runList <- read_excel(path_RunControl, sheet="params", skip  = 3) %>% filter(!is.na(runname), include == 1)
runList

# Import return scenarios
returnScenarios <- read_excel(path_RunControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))


# Import global parameters
Global_paramlist <- read_excel(path_RunControl, sheet="GlobalParams") %>% filter(!is.na(init_year)) %>% 
	as.list

# additinal global variables 
Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)
# Global_paramlist$calib_g <- 0.008  # -0.005


#*******************************************************************************
#                         ####  Run Model ####
#*******************************************************************************


# for(runName in runList$runname ){
	
	runName <- runList$runname
	
	## Parameters 
	paramlist <- get_parmsList(runList, runName)
	# paramlist$simTiers <- "separate"  # "joint"(defult) or "separate"
	if(paramlist$nyear.override != 0) Global_paramlist$nyear <- paramlist$nyear.override
	paramlist$seed <- 1234 # For generating investment returns
	paramlist$v <- 1/(1 + paramlist$i)
	
	
	source("NYSERS_0_Master.R")
	save(outputs_list, file = paste0(dir_modelOutputs, "Outputs_", runName, ".RData"))
	
# }

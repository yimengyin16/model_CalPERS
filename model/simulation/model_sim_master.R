# Actuarial valuation for PERF A with 2-tier simplification


# Valuation name
sim_name <- "Dev"


#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************
## File path of the run control file

dir_runControl <- "model/"
fn_runControl  <- "RunControl.xlsx"
filePath_runControl <- paste0(dir_runControl, fn_runControl)

dir_outputs <- "model/simulation/outputs_sim/"


## Import global parameters
Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
  filter(!is.na(init_year)) %>% 
  as.list
 
## Import valuation parameters
sim_paramlist <- read_excel(filePath_runControl, sheet="params_sim", skip  = 3) %>% 
  filter(!is.na(sim_name), sim_name == sim_name) %>% 
  as.list


## Additinal global variables 

# age and entry age ranges
Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)


# turn tier names into a character vector


#*******************************************************************************
#                         Load data                                         ####
#*******************************************************************************

sim_paramlist$run_val <- TRUE
if(sim_paramlist$run_val){
  source(paste0("model/valuation/model_val_create_", sim_paramlist$val_name, ".R"))
}

# Load tier data
dir_val <- "model/valuation/outputs_val/"

# tierData_miscAll

#*******************************************************************************
#              Actual investment return, for all tiers                      ####
#*******************************************************************************
source("model/simulation/model_sim_invReturns.R")
sim_paramlist$seed <- 123
i.r <- gen_returns()
# i.r
# i.r[1:5, 1:5]




#*******************************************************************************
#                          Simulation ####
#*******************************************************************************
source("model/simulation/model_sim_simulation.R")
penSim_results <- run_sim()




#*******************************************************************************
#                        Saving results ####
#*******************************************************************************

outputs_list <- list(sim_paramlist    = sim_paramlist,
                     Global_paramlist = Global_paramlist,
                     results          = penSim_results)


saveRDS(outputs_list, file = paste0(dir_outputs, "sim_", sim_name, ".rds"))





#*******************************************************************************
#                        TEMP: Examine results  ####
#*******************************************************************************


# Display1 Basic examination
var_display1 <- c("sim_name", "sim", "year", 
                  "FR_MA", "FR_AA", "MA", "AA", 
                  "AL", 
                  "AL.active", "AL.nonactive",
                  "PVFB",
                  "PVFB.active",
                  "B",
                  "NC_PR",
                  "ERC_PR",
                  "EEC_PR",
                  # "ADC", 
                  "NC", "ERC", "EEC", "SC", "LG", "i.r", "PR"
)


# Display: Decomposition of AL and PVFB
#var_display_decomp_Liab <- c("sim", "year", "FR_MA", "PVFB.act.laca", "PVFB.act.v", "PVFB.act.disbRet", "PVFB.act.death")

# Display: Decompsition of Benefit
#var_display_decomp_B <- c( "sim", "year", "FR_MA", "AL.act.death", "NC.death", "AL.death", "B.death")

# Display: demograhpics
#var_display_demo <- c("sim", "year", "FR_MA", "nactives", "nla", "nterms", "ndisbRet")
# "n.ca.R1", "n.ca.R0S1", "nterms",
# "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )


penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display1))  %>% print
penSim_results %>% filter(sim == 1)  %>% select(one_of(var_display1))  %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1))  %>% print






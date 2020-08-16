# Actuarial valuation for PERF A with 2-tier simplification
#
# As of 8/13/2020, this function can create valuations for the following cases:
#  - Single tier: miscAll
#  - Single tier: sftyAll
# Next step:
#  1. model miscAll and sftyAll jointly. 
#      - issue: do we want to set workforce growth rate for each tier or for
#        the sum of all tiers?
#  2. model 4 tiers: (miscAll, sftyAll) x (classic, PEPRA)


# val_name_run <- val_runList$val_name[1]
# create_val_2tiers(val_name_run)


create_val_2tiers <- function(val_name_run){

cat("Creating valuation", val_name_run, "\n")
  
#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************

## The following variables are defined outside the function in model_val_runControl.R
  
# # Path to run control file
# dir_runControl <- "model/"
# fn_runControl  <- "RunControl.xlsx"
# filePath_runControl <- paste0(dir_runControl, fn_runControl)
# 
# # Path to amortization and asset smoothing info
# dir_planInfo <- "inputs/data_proc/"
# filePath_planInfo <- paste0(dir_planInfo, "Data_CalPERS_planInfo_AV2018.RData")
# 
# # Output folder  
# dir_outputs_val <- "model/valuation/outputs_val/"


# val_name_run <- "Dev_2tiers_bf2"
  
## Import global parameters
Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
  filter(!is.na(init_year)) %>% 
  as.list
 
## Import valuation parameters
val_paramlist <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
  filter(!is.na(val_name), val_name == val_name_run) %>% 
  as.list



## Additinal global variables 

# age and entry age ranges
Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)


# turn tier names into a character vector
val_paramlist$tier_include <-  rlang::parse_expr(paste0("c(", val_paramlist$tier_include, ")" )) %>% eval




#*******************************************************************************
#                         Load data                                         ####
#*******************************************************************************

# Load tier data
dir_tierData <- "model/tiers/tierData/"


ls_tierData <- list()

for (tierName in val_paramlist$tier_include){
  ls_tierData[[tierName]] <- 
         readRDS(paste0(dir_tierData, "tierData_", tierName,  ".rds" ))
  }


# for (tierName in val_paramlist$tier_include){
#   assign(paste0("tierData_", tierName), 
#          readRDS(paste0(dir_tierData, "tierData_", tierName,  ".rds" )))
#   }

# ls_tierData$sftyAll





#*******************************************************************************
#                          Data preparation                                 ####
#*******************************************************************************
source("model/valuation/model_val_prepDataFuns(2).R", local = TRUE)


## Modify tier parameters as specified in the parameter list   
# !! This should be run before any further operations!!
ls_tierData[[1]] <- adj_tierParams(ls_tierData[[1]])


## 1. Full salary schedule
ls_tierData[[1]] <- add_salary_full(ls_tierData[[1]])


# 2. Distribution of new entrants
ls_tierData[[1]] <- add_entrantsDist(ls_tierData[[1]])


# 3. Adjustments to retirement rates
ls_tierData[[1]] <- adj_retRates(ls_tierData[[1]])


# 4. Create a generational decrement table
ls_tierData[[1]] <- expand_decrements(ls_tierData[[1]])


# 5. apply improvements
# TODO: to be updated 
# This step includes calibration of post-retirement mortality
ls_tierData[[1]] <- apply_decImprovements(ls_tierData[[1]])


# 6. Adjustments to initial members
# This stip includes calibration of benefit payments in year 1
ls_tierData[[1]] <- adj_initMembers(ls_tierData[[1]])



#*******************************************************************************
#                            Demographics                                   ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_demographics_singleTier.R", local = TRUE)


pop <- get_demographics(ls_tierData[[1]])
# Note that the function returns a list


#*******************************************************************************
#      Individual actuarial liabilities, normal costs and benenfits    ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_indivLiab_flexbf(2).R", local = TRUE)

indivLiab <- list()
indivLiab[[val_paramlist$tier_include[1]]] <- get_indivLiab(ls_tierData[[1]])



#*******************************************************************************
#     Aggregate actuarial liabilities, normal costs and benefits        ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_aggLiab.R", local = TRUE)

aggLiab <- list()
aggLiab[[val_paramlist$tier_include[1]]] <- get_aggLiab(pop, indivLiab)





#*******************************************************************************
#   Simplification: Initial vested and inactives who are not in pay status  
#*******************************************************************************

# For initial PVB of terminated vested members 
#  - no corresponding demographic data 
#  - PVB = AL

# - Assume the PVFB for initial vested members are paid up through out the next 50 years. 
# - ALs and Bs of initial terminated vested and inactive members will be added to ALx.v and B.v. 
# - Based on method used in PSERS model. 



if (val_paramlist$estInitTerm){
  AL.init.defrRet <-  val_paramlist$AL_defrRet_pctALservRet * aggLiab[[val_paramlist$tier_include[1]]]$servRet.la[1, "ALx.servRet.la"]
 
  
  df_init.defrRet <- data.frame(
    year = 1:51 + (Global_paramlist$init_year - 1),
    #B.init.v.yearsum = c(0, amort_cd(AL.init.v, paramlist$i, 50, TRUE))) %>% 
    B.init.defrRet = c(0, amort_cp(AL.init.defrRet, val_paramlist$i, 50, val_paramlist$startingSalgrowth, TRUE))) %>% 
    mutate(ALx.init.defrRet = ifelse(year == Global_paramlist$init_year, AL.init.defrRet, 0))
  
  for(i_v in 2:nrow(df_init.defrRet)){
    df_init.defrRet$ALx.init.defrRet[i_v] <- 
      with(df_init.defrRet, (ALx.init.defrRet[i_v - 1] - B.init.defrRet[i_v - 1]) * (1 + val_paramlist$i))
  }
  
  # df_init.vested
  
  aggLiab[[val_paramlist$tier_include[1]]]$defrRet %<>% 
    as.data.frame() %>%
    left_join(df_init.defrRet, by = "year") %>%
    mutate_all(list(na2zero)) %>%
    mutate(ALx.defrRet = ALx.defrRet + ALx.init.defrRet,
           B.defrRet   = B.defrRet   + B.init.defrRet) %>%
    as.matrix
}


# aggLiab[[val_paramlist$tier_include[1]]]$defrRet



# 
# 	AggLiab.sumTiers <-
# 		get_AggLiab_sumTiers(AggLiab_nt6, AggLiab_t6)
# 


#*******************************************************************************
#    plan information associated with this valuation        ####
#*******************************************************************************

load(filePath_planInfo) # %>% print

init_amort_include <- character()

if(any(str_detect(val_paramlist$tier_include, "miscAll"))){
  init_amort_include <- c(init_amort_include, c("misc", "inds"))
  }

if(any(str_detect(val_paramlist$tier_include, "sftyAll"))){
  init_amort_include <- c(init_amort_include, c("sfty", "poff", "chp"))
}


init_amort_raw_val <- init_amort_raw %>% 
  filter(grp %in% init_amort_include)

init_unrecReturns.unadj_val <- init_unrecReturns.unadj

#*******************************************************************************
#  Save outputs          ####
#*******************************************************************************

cat("Saving results...\n")
saveRDS(
    list(
      aggLiab = aggLiab,
      indivLiab = indivLiab,
      pop = pop,
      init_amort_raw = init_amort_raw_val,
      init_unrecReturns.unadj = init_unrecReturns.unadj_val
    ),
  file = paste0(dir_outputs_val, "val_", val_name_run, ".rds")
)
}



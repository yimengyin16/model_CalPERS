# Actuarial valuation for PERF A with 2-tier simplification


# Valuation name
val_name <- "Dev_2tiers"



#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************
## File path of the run control file

dir_runControl <- "model/"
fn_runControl  <- "RunControl.xlsx"
filePath_runControl <- paste0(dir_runControl, fn_runControl)


## Import global parameters
Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
  filter(!is.na(init_year)) %>% 
  as.list
 
## Import valuation parameters
val_paramlist <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
  filter(!is.na(val_name), val_name == val_name) %>% 
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

for (tierName in val_paramlist$tier_include){
  assign(paste0("tierData_", tierName), 
         readRDS(paste0(dir_tierData, "tierData_", tierName,  ".rds" )))
  }

# tierData_miscAll



#*******************************************************************************
#                          Data preparation                                 ####
#*******************************************************************************

source("model/valuation/model_val_prepDataFuns.R")

## 1. Full salary schedule
tierData_miscAll <- add_salary_full(tierData_miscAll)


# 2. Distribution of new entrants
tierData_miscAll <- add_entrantsDist(tierData_miscAll)


# 3. Adjustments to retirement rates
tierData_miscAll <- adj_retRates(tierData_miscAll)


# 4. Create a generational decrement table
tierData_miscAll <- expand_decrements(tierData_miscAll)


# 5. apply improvements
tierData_miscAll <- apply_decImprovements(tierData_miscAll)


# 6. Adjustments to initial members
tierData_miscAll <- adj_initMembers(tierData_miscAll)



#*******************************************************************************
#                            Demographics                                   ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_demographics_singleTier.R")

pop <- get_demographics(tierData_miscAll)



#*******************************************************************************
#      Individual actuarial liabilities, normal costs and benenfits    ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_indivLiab.R")

indivLiab <- get_indivLab(tierData_miscAll)



#*******************************************************************************
#     Aggregate actuarial liabilities, normal costs and benenfits        ####
#*******************************************************************************
source("model/valuation/model_val_aggLiab.R")










invisible(gc())

if(paramlist$tier_Mode == "singleTier"){
	AggLiab_allTiers <- get_AggLiab(paramlist$singleTier_select,
																	liab_allTiers,
																	pop_allTiers)
}

if(paramlist$tier_Mode == "multiTier"){
	AggLiab_nt6 <- get_AggLiab("nt6",
														 liab_nt6,
														 pop_multiTier$pop_nt6)

	AggLiab_t6 <- get_AggLiab("t6",
														 liab_t6,
														 pop_multiTier$pop_t6)

	AggLiab.sumTiers <-
		get_AggLiab_sumTiers(AggLiab_nt6, AggLiab_t6)


}

if(paramlist$tier_Mode == "singleTier") AggLiab <- AggLiab_allTiers
if(paramlist$tier_Mode == "multiTier")  AggLiab <- AggLiab.sumTiers

AggLiab$active %>% as.data.frame %>% select(year, ALx.actAll.yearsum, NCx.actAll.yearsum, PVFBx.laca.yearsum, PVFBx.actAll.yearsum)



#*******************************************************************************
#   Simplification: Initial vested and inactives who are not in pay status  
#*******************************************************************************

# For initial PVB of terminated vested members 
#  - no corresponding demographic data 
#  - PVB = AL

# - Assume the PVFB for initial vested members are paid up through out the next 50 years. 
# - ALs and Bs of initial terminated vested and inactive members will be added to ALx.v and B.v. 
# - Based on method used in PSERS model. 

# paramlist$AL_defrRet_0 <- 3517847922

if (paramlist$estInitTerm){
	AL.init.v <-  paramlist$AL_defrRet_0 
	
	df_init.vested <- data.frame(
		year = 1:51 + (Global_paramlist$init_year - 1),
		#B.init.v.yearsum = c(0, amort_cd(AL.init.v, paramlist$i, 50, TRUE))) %>% 
		B.init.v.yearsum = c(0, amort_cp(AL.init.v, paramlist$i, 50, paramlist$salgrowth_amort, TRUE))) %>% 
		mutate(ALx.init.v.yearsum = ifelse(year == Global_paramlist$init_year, AL.init.v, 0))

	for(i_v in 2:nrow(df_init.vested)){
		df_init.vested$ALx.init.v.yearsum[i_v] <- 
			with(df_init.vested, (ALx.init.v.yearsum[i_v - 1] - B.init.v.yearsum[i_v - 1]) * (1 + paramlist$i))
	}
	
	# df_init.vested
	
	AggLiab$term %<>% 
		as.data.frame() %>%
		left_join(df_init.vested, by = "year") %>%
		mutate_all(funs(na2zero)) %>%
		mutate(ALx.v.yearsum = ALx.v.yearsum + ALx.init.v.yearsum,
					 B.v.yearsum   = B.v.yearsum + B.init.v.yearsum) %>%
		as.matrix
}




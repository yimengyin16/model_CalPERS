
cat("Start running", paramlist$runname, "\n")


#*******************************************************************************
#                         Load data                                         ####
#*******************************************************************************

# AV2018
# Demographics: As of 2018/03/31


run_loadingData <- FALSE

if(run_loadingData){

# Load plan information (unrecognized return)
source("NYSERS_Data_readPlanInfo_AV2018.R")

# Load decrement tables and salary scales in Experience Study 2015
source("NYSERS_Data_readDecrements_ES2015.R")

# Load demographics data in CAFR 2017
source("NYSERS_Data_readMemberData_20180331.R")

# Construct member data
source("NYSERS_Data_imputeMemberData_20180331.R")
}

# Loading data constructed above
dir_data <- "Inputs/"
load(paste0(dir_data, "Data_NYSERS_planInfo_AV2018.RData"))
load(paste0(dir_data, "Data_NYSERS_ES2015.RData"))
load(paste0(dir_data, "Data_NYSERS_memberData_20180331_imputed.RData"))




#*******************************************************************************
#                     Create decrement tables                               ####
#*******************************************************************************

## Decrement tables
source("NYSERS_Model_Decrements_ES2015.R")

if(paramlist$tier_Mode == "multiTier"){
	decrement_model_nt6 <- get_decrements("nt6")
	decrement_model_t6  <- get_decrements("t6")
}


if(paramlist$tier_Mode == "singleTier"){
	decrement_model_allTiers <- get_decrements(paramlist$singleTier_select)
} 




#*******************************************************************************
#              Actual investment return, for all tiers                      ####
#*******************************************************************************
source("NYSERS_Model_InvReturns.R")
i.r <- gen_returns()
i.r
# i.r[1:5, 1:5]



#*******************************************************************************
#                          Create plan data                                 ####
#*******************************************************************************
source("NYSERS_Model_PrepData.R")

## Salary
salary <- get_salary_proc() # same for all tiers


## Initial demographics: single Tier (assuming all members in one tier)
if(paramlist$tier_Mode == "singleTier"){
   benefit_servRet_allTiers <- get_benefit_servRet(filter(init_servRets_tiers, tier == "allTiers"))
	 benefit_disbRet_allTiers <- get_benefit_disbRet(filter(init_disbRets_tiers, tier == "allTiers"))

	init_pop_allTiers <-  get_initPop(filter(init_actives_tiers, tier == "allTiers"),
																		filter(init_servRets_tiers, tier == "allTiers"),
																		filter(init_disbRets_tiers, tier == "allTiers")
																		# filter(init_terms_tiers,   tier == "allTiers")
																		)
}


## Initial demographics: Two tiers (tier 6 and non-tier 6)
if(paramlist$tier_Mode == "multiTier"){
	# Non-tier 6 
	benefit_servRet_nt6  <- get_benefit_servRet(filter(init_servRets_tiers, tier == "nontier6"))
	benefit_disbRet_nt6  <- get_benefit_disbRet(filter(init_disbRets_tiers, tier == "nontier6"))
	
	init_pop_nt6 <-  get_initPop(filter(init_actives_tiers,         tier == "nontier6"),
																		  filter(init_servRets_tiers, tier == "nontier6"),
																		  filter(init_disbRets_tiers, tier == "nontier6")
																	   	# filter(init_terms_tiers,   tier == "allTiers")
	)
	
	# Tier 6
	benefit_servRet_t6  <- get_benefit_servRet(filter(init_servRets_tiers, tier == "tier6"))
	benefit_disbRet_t6  <- get_benefit_disbRet(filter(init_disbRets_tiers, tier == "tier6"))
	
	init_pop_t6 <-  get_initPop(filter(init_actives_tiers,  tier == "tier6"),
															 filter(init_servRets_tiers, tier == "tier6"),
															 filter(init_disbRets_tiers, tier == "tier6")
															 # filter(init_terms_tiers,   tier == "allTiers")
	)
	
}


# Age distribution of new hires
entrants_dist <- get_entrantsDist(filter(init_actives_tiers, tier == "tier6"))
# entrants_dist %>% plot




#*******************************************************************************
#                            Demographics                                   ####
#*******************************************************************************
invisible(gc())

newEnt_byTier <- c(nt6 = 0, t6 = 1)

if(paramlist$tier_Mode == "multiTier"){
	source("NYSERS_Model_Demographics_multiTier.R")

	pop_multiTier <- get_Population(
		init_pop_nt6_         = init_pop_nt6,
		decrement_model_nt6_  = decrement_model_nt6,

		init_pop_t6_         = init_pop_t6,
		decrement_model_t6_  = decrement_model_t6,
    
		newEnt_byTier_    = newEnt_byTier,
		entrants_dist_    = entrants_dist)
}


if(paramlist$tier_Mode == "singleTier"){
	
	source("NYSERS_Model_Demographics_singleTier.R")
	
	pop_allTiers <- get_Population(
		init_pop_         = init_pop_allTiers,
		entrants_dist_    = entrants_dist,
		decrement_model_  = decrement_model_allTiers)
}


#*******************************************************************************
#      Individual actuarial liabilities, normal costs and benenfits ####
#*******************************************************************************

source("NYSERS_Model_IndivLiab_FlexCOLA_calib.R")
invisible(gc())


if(paramlist$tier_Mode == "multiTier"){
	liab_nt6 <- get_indivLab(tier_select_     = "nt6",
													 decrement_model_ = decrement_model_nt6,
													 salary_          = salary,
													 benefit_servRet_ = benefit_servRet_nt6,
													 benefit_disbRet_ = benefit_disbRet_nt6
	)


	liab_t6 <- get_indivLab(tier_select_     = "t6",
													decrement_model_ = decrement_model_t6,
													salary_          = salary,
													benefit_servRet_ = benefit_servRet_t6,
													benefit_disbRet_ = benefit_disbRet_t6
	)
}


if(paramlist$tier_Mode == "singleTier"){
	liab_allTiers <- get_indivLab(tier_select_     = paramlist$singleTier_select,
																decrement_model_ = decrement_model_allTiers,
																salary_          = salary,
																benefit_servRet_ = benefit_servRet_allTiers,
																benefit_disbRet_ = benefit_disbRet_allTiers
	)
}


#*******************************************************************************
#     5. Aggregate actuarial liabilities, normal costs and benenfits        ####
#*******************************************************************************
source("NYSERS_Model_AggLiab.R")
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


#*******************************************************************************
#                          Simulation ####
#*******************************************************************************

# paramlist$cost_method <- "AGG"
# paramlist$amort_type <- "open"
# paramlist$m <- 20
# paramlist$MA_0_pct <- 0.994623958869755
# paramlist$AA_0_pct <- 0.969880528196077

source("NYSERS_Model_Sim(2).R")
penSim_results <- run_sim(AggLiab)




#*******************************************************************************
#                        Saving results ####
#*******************************************************************************

outputs_list <- list(paramlist = paramlist,
										 Global_paramlist = Global_paramlist,
										 results     = penSim_results)



#*******************************************************************************
#                      Examining results ####
#*******************************************************************************

# penSim_results %<>%
# 	mutate(AL.inact   = AL - AL.act, 
# 		     PVFB.tot   = PVFB + AL.la + AL.term + AL.act.disbRet,
# 				 PVFB.inact = PVFB.tot - PVFB,
# 				 PVFB.act   = PVFB) %>% 
# 	as_tibble
	
	

# Display1 Basic examination
var_display1 <- c("runname", "sim", "year", 
									"FR_MA", "FR_AA", "MA", "AA", 
									"AL", 
									"AL.act", "AL.inact",
									"PVFB.tot",
									"PVFB.act",
									"B",
									"NC_PR",
									"ERC_PR_indiv",
									"ERC_PR_AGG",
									"EEC_PR",
									# "ADC", 
									"NC", "ERC", "EEC", "SC", "LG", "i.r", "PR"
									)

# "NC", "SC", "ADC", "C", "EEC", "EEC_PR", "PR", 
# "B.la", "B.death", "B.v", "B.disbRet"
# "AL.act", "AL.la", "AL.term", "AL.disbRet", "AL.death",
# # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
# #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB",
# # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca",
# "PR", "NC_PR", "NC","ERC", "B.la")

# Display: Decomposition of AL and PVFB
var_display_decomp_Liab <- c("sim", "year", "FR_MA", "PVFB.act.laca", "PVFB.act.v", "PVFB.act.disbRet", "PVFB.act.death")

# Display: Decompsition of Benefit
var_display_decomp_B <- c( "sim", "year", "FR_MA", "AL.act.death", "NC.death", "AL.death", "B.death")

# Display: demograhpics
var_display_demo <- c("sim", "year", "FR_MA", "nactives", "nla", "nterms", "ndisbRet")
# "n.ca.R1", "n.ca.R0S1", "nterms",
# "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )



penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1))  %>% print
penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display1))  %>% print
penSim_results %>% filter(sim == 1)  %>% select(one_of(var_display1))  %>% print


penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display_demo))  %>% print


# penSim_results %>% filter(sim == 0) %>% select(year, LG) %>% as.data.frame()


#*******************************************************************************
#                        ##  Calibration   ####
#*******************************************************************************

# Loarding targets
df_calib <- 
	read_excel("RunControl.xlsx", sheet = "targetVals_2018", range = "b5:c59") %>% 
	filter(!is.na(varname)) 
df_calib


# Extract model values 
# calib_varnames <- {c(
#  	PVFB_act_servRet = "PVFB.laca",
# 	PVFB_act_defrRet = "PVFB.v",
# 	PVFB_act_disbRet = "PVFB.disbRet",
# 	PVFB_act_death   = "PVFB.death",
# 	PVFB_act         = "PVFB.act",
# 	
# 	PVFB_defrRet     = "AL.term",
# 	PVFB_servRet     = "AL.la",
# 	PVFB_disbRet     = "AL.disbRet",
# 	
# 	AL_act           = "AL.act",
# 	
# 	MVA_target       = "MA",
# 	AVA_target       = "AA",
# 	FR_MVA_target    = "FR_MA",
# 	FR_AVA_target    = "FR_AA",
# 	
# 	PVFS             = "PVFS",
# 	PVFEEC           = "PVFEEC",
# 	
# 	ERCrate_target   = "ERC_PR_AGG",
# 	
# 	ERC_paid         = "ERC",
#   EEC_paid         = "EEC",
# 	B_paid           = "B",
# 	
# 	PR_AV            = "PR",
# 	
# 	servCost         = 'NC'
# )
# }

{
df_calib_varnames <- 
	tribble(
		~vn,                 ~vn_mod,          ~vn_full,
		"PVFB_act_servRet" , "PVFB.laca",      "Active members: PVFB of service retirement",
		"PVFB_act_defrRet" , "PVFB.v",         "Active members: PVFB of deferred retirement",
		"PVFB_act_disbRet" , "PVFB.disbRet",   "Active members: PVFB of disability retirement",
		"PVFB_act_death"   , "PVFB.death",     "Active members: PVFB of death benefit",
		
		"PVFB_act"         , "PVFB.act",       "PVFB for active members",
		"PVFB_servRet"     , "AL.la",          "PVFB for service retirees",
		"PVFB_defrRet"     , "AL.term",        "PVFB for deferred retirees",
		"PVFB_disbRet"     , "AL.disbRet",     "PVFB for disability retirees",
		
	  "AL_act"           , "AL.act",         "Actuarial liability for active members",
		
		"MVA_target"       , "MA",             "Market value of assets (MVA)",
		"AVA_target"       , "AA",             "Actuarial value of assets (AVA)",
		"FR_MVA_target"    , "FR_MA",          "Funded ratio (MVA)",
		"FR_AVA_target"    , "FR_AA",          "Funded ratio (AVA)",
		
		"PVFS"             , "PVFS",           "Present value of future salaries",
		"PVFEEC"           , "PVFEEC",         "Present value of future employee contributions",
		     
		"ERCrate_target"   , "ERC_PR_AGG",     "Employer contribution rate",
		
		"ERC_paid"         , "ERC",            "Employer contribution paid (previous year)",
		"EEC_paid"         , "EEC",            "Employee contribution paid (previous year)",
		"B_paid"           , "B",              "Benefit payment (previous year)",
		
		"PR_AV"            , "PR",             "Total payroll",
		
		"servCost"         , 'NC',             "Service cost (Normal cost)"
	)
	
calib_varnames <- df_calib_varnames$vn_mod
names(calib_varnames) <- df_calib_varnames$vn
	
}



df_calib_mod <- 
penSim_results %>% 
	filter(sim == 0, year == 2018) %>% 
	select(!!calib_varnames) %>% 
  mutate(PVFB_inact = PVFB_defrRet + PVFB_servRet + PVFB_disbRet,
  			 PVFB_tot   = PVFB_act + PVFB_inact,
  			 AL_inact   = PVFB_inact,
  			 AL_tot     = AL_act + AL_inact
  			 # FR_MVA_target = FR_MVA_target / 100,
  			 # FR_AVA_target = FR_AVA_target / 100,
  			 # ERCrate_target = ERCrate_target / 100
  			 ) %>% t %>% 
	as.data.frame() %>% 
	rownames_to_column("varname") %>% 
	rename(value_model = V1 )
df_calib_mod 

# Comparing model outputs to targets

df_calib_tot <- 
  df_calib %>%
	left_join(df_calib_mod, by = "varname") %>% 
	mutate(diff_pct = 100 * (value_model / target_AV2018 - 1)) %>% 
	filter(!is.na(diff_pct)) %>% 
	left_join(select(df_calib_varnames, varname = vn, vn_full), by = "varname" )
df_calib_tot %>% kable(digits= 2)


## Calibration for PVB by tier

df_calib_mod_nt6 <- 
AggLiab_nt6$active %>% 
	as.tibble() %>% 
	filter(year == 2018) %>% 
	select(c(PVFB_act_servRet_nt6 = "PVFBx.laca.yearsum",
			     PVFB_act_defrRet_nt6 = "PVFBx.v.yearsum",
			     PVFB_act_disbRet_nt6 = "PVFBx.disbRet.yearsum",
			     PVFB_act_death_nt6   = "PVFBx.death.yearsum",
			     PVFB_act_nt6         = "PVFBx.actAll.yearsum",
					 PR_AV_nt6            = "PR.yearsum")) %>% 
	t %>% as.data.frame
df_calib_mod_nt6$varname <- rownames(df_calib_mod_nt6)
rownames(df_calib_mod_nt6) <- NULL
df_calib_mod_nt6 %<>% 
	rename(value_model = V1 ) %>% 
	select(varname, value_model) 
df_calib_mod_nt6

df_calib_mod_t6 <- 
	AggLiab_t6$active %>% 
	as.tibble() %>% 
	filter(year == 2018) %>% 
	select(c(PVFB_act_servRet_t6 = "PVFBx.laca.yearsum",
					 PVFB_act_defrRet_t6 = "PVFBx.v.yearsum",
					 PVFB_act_disbRet_t6 = "PVFBx.disbRet.yearsum",
					 PVFB_act_death_t6   = "PVFBx.death.yearsum",
					 PVFB_act_t6         = "PVFBx.actAll.yearsum",
					 PR_AV_t6            = "PR.yearsum")) %>% 
	t %>% as.data.frame
df_calib_mod_t6$varname <- rownames(df_calib_mod_t6)
rownames(df_calib_mod_t6) <- NULL
df_calib_mod_t6 %<>% 
	rename(value_model = V1 ) %>% 
	select(varname, value_model) 
df_calib_mod_t6


df_calib_nt6 <- 
	df_calib %>% 
	left_join(df_calib_mod_nt6, by = "varname") %>% 
	mutate(diff_pct = 100 * (value_model / target_AV2018 - 1)) %>% 
	filter(!is.na(diff_pct)) 

df_calib_t6 <- 
	df_calib %>% 
	left_join(df_calib_mod_t6, by = "varname") %>% 
	mutate(diff_pct = 100 * (value_model / target_AV2018 - 1)) %>% 
	filter(!is.na(diff_pct)) 


# All tiers
{
df_calib_tot %>% 
	filter(!varname %in% c("FR_MVA_target", "FR_AVA_target", "ERCrate_target")) %>% 
	kable(digits= 2, format.args = list(big.mark = ",")) %>% print
df_calib_tot %>% 
	filter(varname %in% c("ERCrate_target")) %>% 
	kable(digits= 2, format.args = list(big.mark = ",")) %>% print
}

{
df_calib_nt6 %>% kable(digits= 2, format.args = list(big.mark = ",")) %>% print # non-tier 6
df_calib_t6  %>% kable(digits= 2, format.args = list(big.mark = ",")) %>% print # tier 6
}


## saving calibration results for the paper



df_calib_tot 
df_calib_nt6
df_calib_t6

xlsx::write.xlsx2(df_calib_tot %>% select(varname, vn_full, everything()), "Outputs/calibration/calibResults.xlsx", sheetName = "Total", append = F)
xlsx::write.xlsx2(df_calib_nt6, "Outputs/calibration/calibResults.xlsx", sheetName = "nonTier6", append = T)
xlsx::write.xlsx2(df_calib_t6,  "Outputs/calibration/calibResults.xlsx", sheetName = "Tier6",    append = T)






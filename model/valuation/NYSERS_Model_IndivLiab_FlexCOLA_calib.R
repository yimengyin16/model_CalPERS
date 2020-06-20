# This script calculates individdual liabilities and normal costs for NYCTRS 
# version 1
#  - tier 6 and non-tier 6 members


## Notes for calibration
#   Mark for calibration: #calibration


## Road map

#  1.  Preparation

# Service retirement
#  2.1 AL and NC of life annuity and contingent annuity for actives
#  2.2 AL and benefit for retirees with life annuity

# Deferred retirement
#  3.1 AL and NC of deferred benefits for actives
#  3.2 AL and benefits for vested terminated members

# Disability benefit
#  4.1 AL and NC of disability benefit
#  4.2 AL and benefits for disability benefit

# Death benefit
#  5.1 AL and NC of benefit for death before retirement
#  5.2 AL and benefits for vested terminated members

#  6. Selecting variables for the chosen actuarial method



get_indivLab <- function(tier_select_,
                         decrement_model_ = decrement_model,
                         salary_          = salary,
                         benefit_servRet_ = benefit_servRet,
                         benefit_disbRet_ = benefit_disbRet,
                         # init_terms_ = initPop$terms,
                         paramlist_ = paramlist,
                         Global_paramlist_ = Global_paramlist){

# Inputs for development

## Single tier
# tier_select_     = paramlist$singleTier_select
# decrement_model_ = decrement_model_allTiers
# salary_          = salary
# benefit_servRet_ = benefit_servRet_allTiers
# benefit_disbRet_ = benefit_disbRet_allTiers
# 
# #init_terms_     = initPop$terms # get_tierData(init_terms_all, Tier_select)
# #mortality.post.model_ = mortality.post.model
# #liab.ca_         = liab.ca
# #liab.disb.ca_ = liab.disb.ca
# 
# paramlist_       =  paramlist
# Global_paramlist_ =  Global_paramlist
   
##  Multiple tiers

# tier_select_     = "nt6"
# decrement_model_ = decrement_model_nt6
# salary_          = salary
# benefit_servRet_ = benefit_servRet_nt6
# benefit_disbRet_ = benefit_disbRet_nt6
# paramlist_        = paramlist
# Global_paramlist_ = Global_paramlist
	
# tier_select_     = "t6"
# decrement_model_ = decrement_model_t6
# salary_          = salary
# benefit_servRet_ = benefit_servRet_t6
# benefit_disbRet_ = benefit_disbRet_t6
# paramlist_        = paramlist
# Global_paramlist_ = Global_paramlist
	
  
assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(paramlist_,        envir = environment())
  


## Setting tier-dependent parameters

if(tier_select_ == "nt6"){
	fasyears <- 3
	v.year   <- 5 
}


if(tier_select_ == "t6"){
	fasyears <- 3
	v.year   <- 10
}


  
#*******************************************************************************
#                               1. Preparation                        #####                  
#*******************************************************************************
cat("Tier:", tier_select_, "\n")
cat("Preparation")

## Specify the earliest year needed for actives
#  Ealiest year required for actives：the year a (max_retAge - 1) year old active in year 1 entered the workforce at age min_ea 
min_year_actives <- init_year - ((max_retAge - 1) -  min_ea)
min_year <- min_year_actives


#*** Vintage code:    
  ## the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
  ## Track down to the year that is the smaller one of the two below:

   # min_year <- min(init_year - (max_age - (r_max - 1)), 
   #                 init_year - (r_max - 1 - min_ea)) 
   #                # min(init.year - (benefit_$age - (r.min - 1))))
#*** 


## Setup the data frame for active members, with all combos of start_year-ea-age needed.

# Possible combinations of start_year-ea-age for actives need to be supplemented by combinations needed for 
# all types of beneficiaries. 
 # - Service retirees: combinations can be obtained from benefit_servRet_
 # - Disability retirees: combinations can be obtained from benefit_disbRet_

start_year_supl <- 
	union(
		(benefit_servRet_ %>% filter(benefit_servRet != 0, start_year < min_year))$start_year,
	  (benefit_disbRet_ %>% filter(benefit_disbRet != 0, start_year < min_year))$start_year
		)

liab_active <- 
  bind_rows(
  # for actives
  expand.grid(start_year = min_year:(init_year + nyear - 1) , 
              ea         = range_ea, 
              age        = range_age), 
  # for service retirees
  expand.grid(start_year = start_year_supl,
  						ea  = unique(benefit_servRet_$ea), # Should be just one value (assumed ea)
  						age = range_age 
  						)
  )
liab_active <- liab_active[!duplicated(select(liab_active, start_year, ea, age)), ] # Just for safety. 


## Clean up and incorporate data needed
liab_active %<>%
  filter(start_year + max_age - ea >= init_year, # drop redundant combinations of start_year and ea. (delet those who never reach year 1.) 
                                                 # max_age can replaced by max_retAge - 1 if only consider actives
         age >= ea) %>%   
  mutate(year = start_year + age - ea) %>%            
  arrange(start_year, ea, age) %>% 
  left_join(salary_, by =  c("start_year", "ea", "age", "year")) %>%
  left_join(decrement_model_, by = c("start_year", "ea", "age")) %>% 
  # left_join(mortality.post.model_ %>% filter(age == age.r) %>% ungroup %>% select(year, age, ax.r.W)) %>%
  # left_join(liab.ca_      %>% filter(age == age.r)    %>% ungroup %>% select(year, age, liab.ca.sum.1)) %>% 
  # left_join(liab.disb.ca_ %>% filter(age == age.disb) %>% ungroup %>% select(year, age, liab.disb.ca.sum.1 = liab.ca.sum.1)) %>% 
	mutate(tier_select = tier_select_,
				 yos = age - ea)
 

## Create variables needed to calculate liabilities 
liab_active %<>% 
	group_by(start_year, ea) %>% 
   # Calculate salary and benefits
  mutate(
    
  	# #calibration: Calibration for salary history of initial active members
  	# sx = ifelse(start_year <= init_year & year < init_year, 1.1*sx, sx), # sx * (1 + 0.01)^(init_year - year)
  	# sx = ifelse(start_year <= init_year & year < init_year, sx * (1 + 0.012)^(init_year - year), sx), 
  
    # Calculate Final Average Salary
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
    n  = pmin(yos, fasyears),                                      # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    
    # COLA (for compound cola, will not be used for benefits with flexible COLA)
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    
    # Accrued service retirement benefits for tier III/IV
    Bx = case_when(                                  
    	# na2zero(bfactor * yos * fas), # simple formula for accrued benefits, note that only Bx for ages above min retirement age are necessary under EAN.
    	tier_select_ %in% c("nt6") & yos < 20       ~ na2zero(1/60 * yos * fas),  # 1 2/3% * yos
    	tier_select_ %in% c("nt6") & yos %in% 20:30 ~ na2zero(0.02  * yos * fas),
    	tier_select_ %in% c("nt6") & yos > 30	      ~ na2zero( (0.6 + (yos - 30) * 0.015) * fas),
    	
    	tier_select_ %in% c("t6") & yos <  20               ~ na2zero(1/60   * yos * fas),
    	tier_select_ %in% c("t6") & yos >= 20               ~ na2zero( (0.35 + (yos - 20) * 0.02) * fas),
    	
    	TRUE           ~ 0),
  	
  	# Bx = ifelse(start_year <= init_year, Bx * 1.03, Bx), #calibration
  	
    bx = lead(Bx) - Bx,                              # benefit accrual at age x
    
    # Vesting
    v.year = v.year,      # yos needed for vesting
    age.vben = age_vben,  # assumed age for vested members to start receiving benefits
    
    
    # ax.XXX: actuarial present value of future benefit, for $1's benefit in the initial year. 
      # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    
    # ax.servRet  = get_tla(pxm_servRet, i, COLA.scale),    # Note: not needed after flexible benefits are modeled.  Service retirement benefit (will be replaced when contingent retirement beneift is added) ax.r
    ax.term    = get_tla(pxm_term,   i, COLA.scale),      # deferred retirement benefit (actually we only need the value at age_vben)
    ax.disbRet  = get_tla(pxm_disbRet, i, COLA.scale),      # disability retirement benefit
    # ax.r.W.ret is already in mortality.post.model_
    ax.deathBen = get_tla(pxm_servRet, i, COLA.scale),      # beneificiaries of death benefits
    
    
    # Temporary annuity values from age x to retirment age (fixed end)
     
     # for service retirement
    axR = c(get_tla(pxT[age < max_retAge], i), rep(0, max_age - max_retAge + 1)),                          # aT..{x:max_retAge-x-|} discount value of max_retAge at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < max_retAge], i,  sx[age < max_retAge]), rep(0, max_age - max_retAge + 1)),   # ^s_aT..{x:max_retAge-x-|}
    
     
     # for deferred retirement 
    axr = ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i), rep(0, max_age - unique(age_vben) + 1))),                       # Similar to axR, but based  on age_vben. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)       
    axrs= ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i,  sx[age < age_vben]), rep(0, max_age - unique(age_vben) + 1))),  # Similar to axRs, but based on age_vben. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)
    
    
    # Temporary annuity values from a fixed entry age y to x (fixed start) (PV of future salary at entry age w/0 and w/ salary growth)
    ayx = c(get_tla2(pxT[age <= max_retAge], i), rep(0, max_age - max_retAge)),                          # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= max_retAge], i,  sx[age <= max_retAge]), rep(0, max_age - max_retAge)),  # need to make up the length of the vector up to age max.age
    
    
    # axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    # axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    # EEC
    # For now use 55/25 and 55/27 rule
    # 4.85% for yos <=10,
    # 1.85% for yos >= 10
    
    
    EEC = case_when(
    	
    	tier_select_ == "nt6" & yos <= 10 ~ 0.03 * sx, 
    	tier_select_ == "nt6" & yos >  10 ~ 0 * sx,
      
    	# Later: set special rules for tier 5 members: 3% for all years   	

    	# TODO: should be 2-year lag of sx, use sx for now
    	tier_select_ == "t6" & sx <= 45000                ~ 0.03 * sx, 
    	tier_select_ == "t6" & sx >  45000 & sx <=  55000 ~ 45000 * 0.03 + 0.035 * (sx - 45000),
    	tier_select_ == "t6" & sx >  55000 & sx <=  75000 ~ 45000 * 0.03 + 10000 * 0.035 + 0.045 * (sx - 55000),
    	tier_select_ == "t6" & sx >  75000 & sx <= 100000 ~ 45000 * 0.03 + 10000 * 0.035 + 20000 * 0.045 + 0.0575 * (sx - 75000), 
    	tier_select_ == "t6" & sx > 100000                ~ 45000 * 0.03 + 10000 * 0.035 + 20000 * 0.045 + 25000 * 0.0575 + 0.06 * (sx - 100000),
 
    	TRUE ~ 0)
  )
cat("......DONE\n")

# liab.active %>% select(ea, age, ax.disb.la, ax.vben)
# liab.active %>% select(init_year, year, ea, age, PV)


#*******************************************************************************
#          2.1 Service Retirement: AL and benefits for retirees            #####                  
#*******************************************************************************
cat("Service Retirement - retirees")

## Calculating initial benefit payment 
liab_active %<>%   
	mutate(
		
		# Benefit reduction factors
		# TODO: use rules of tier3&4 for tier 5, may want to specify tier 5 rules based on start_year
		benReduction = case_when(
			tier_select_ %in% c("nt6") & age %in% 60:61 ~ 1 - (62 - age) * 0.06,
			tier_select_ %in% c("nt6") & age %in% 55:59 ~ 0.88 - (60 - age) *  0.03,
			tier_select_ %in% c("t6")  & age %in% 55:62 ~ 1 - (63 - age) * 0.065,
			TRUE ~ 1),
		
		# Retirement eligibility and % benefit can be claimed at retirement 
		gx.laca = case_when(
			as.logical(elig_full) ~ 1, 
			as.logical(elig_early) ~ benReduction,
			TRUE ~ 0),
		
		Bx.laca  = gx.laca * Bx # * adj_fct.act.laca
		)


## Benefit payments and ALs for initial service retirees

if(nrow(filter(benefit_servRet_, benefit_servRet != 0)) == 0) {
	liab_la_init <- NULL
	} else {

liab_la_init <- 
	expand.grid(
		# Set up grids for retirees
		# 4 dimensions:
		#  - age_servRet
		#  - age
		#  - ea,
		#  - start_year
		age_servRet= filter(benefit_servRet_, benefit_servRet != 0)$age, # This ensures that year of retirement is year 1.
		age        = range_age[range_age >= min(filter(benefit_servRet_, benefit_servRet != 0)$age)]) %>%
	mutate(ea         = min_age,  # min(benefit_$age) - 1,
				 start_year = init_year - (age_servRet - ea)) %>% 
	filter(# age >= ea, 
		     age >= age_servRet) %>% 
	left_join(benefit_servRet_, by = c("ea", "age", "start_year", "age_servRet")) %>% 
	left_join(liab_active %>% select(start_year, ea, age, pxm_servRet), by = c("age", "ea", "start_year")) %>% 
	group_by(start_year, age_servRet, ea) %>% 
	mutate(year_servRet = start_year + age_servRet - ea, 
				 year = start_year + age - ea,
				 COLA.scale = (1 + cola)^(age - min(age)), # may not be used under flexible COLA
				 
				 # Annual benefit payments
				 
				 # B.la   = ifelse(year_servRet <= init_year, benefit_servRet, Bx.laca),
				 # B.la   = B.la[age == age_servRet] + 100 * (row_number() - 1),
				 
				 # NYSERS COLA
				 # Assumption on the age of initial retirees starting to become eligible for COLA: 65
				 COLA_elig_serRet_init = cumsum(age >= 65) ,
				 B.la    = benefit_servRet[age == age_servRet] + min(benefit_servRet[age == age_servRet], 18000) * ((1 + cola)^COLA_elig_serRet_init - 1),
				 # B.la      = benefit_servRet[age == age_servRet] + min(benefit_servRet[age == age_servRet], 18000) * 0.015 * COLA_elig1,
				 
				 #calibration
				 # B.la   = B.la  * (1 + calib_g)^(year - init_year),
				 
				 ALx.la  = get_tla_cashflow(pxm_servRet, i, B.la)
				  )
}
# liab_la_init


## Benefit payments and ALs for current and future active members
liab_la <- 
	# grids for who retire after year 1.
	expand.grid(ea           = range_ea[range_ea < max_retAge], # Just be safe. Note that this would drop all cells out of this ea range. 
							age_servRet  = min_retAge:max_retAge,
							start_year = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1), # min start_year: entry year for those who have entry age of min_ea and retire in year 2 at max_retAge. 
							age        = range_age[range_age >=min_retAge]) %>%
		filter(age         >= ea,
					 age_servRet >= ea,
					 age         >= age_servRet) %>% 
					 #start_year + (age_servRet - ea) >= init_year,     # retire after year 2, LHS is the year of retirement
					 #start_year + age - ea           >= init_year) %>% # not really necessary since we already have age >= age.r
	data.table(key = "start_year,ea,age_servRet,age")


#liab_la <- liab_la[!duplicated(liab_la %>% select(start_year, ea, age, age_servRet ))]  # should have no duplication, just for safety

# Merging data from liab_active
liab_la <- merge(liab_la,
								 select(liab_active, start_year, ea, age, Bx.laca, pxm_servRet) %>% data.table(key = "ea,age,start_year"),
								 all.x = TRUE, 
								 by = c("ea", "age","start_year")) %>%
	arrange(start_year, ea, age_servRet) %>% 
	mutate(year   = start_year + age - ea) %>% 
	as.data.frame
	# left_join(select(mortality.post.model_, year, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
	# left_join(benefit_servRet_, by = c("ea", "age", "start_year", "age_servRet"))

# Calculate benefit and AL
liab_la %<>% 
	group_by(age_servRet, start_year, ea) %>% 
	mutate(year_servRet = start_year + age_servRet - ea, 
				 COLA.scale = (1 + cola)^(age - min(age)), # may not be used under flexible COLA
		     
				 # Annual benefit payments
				 B.la   = Bx.laca,
				 # B.la   = B.la[age == age_servRet] + 100 * (row_number() - 1),
				 # B.la   = B.la[age == age_servRet] * COLA.scale / COLA.scale[age == age_servRet],
				 
				 # NYSERS COLA
				 COLA_elig_servRet_new = cumsum((age >= 62 & (age - age_servRet) >= 5) | (age >= 55 & (age - age_servRet) >= 10)),
				 B.la      = B.la[age == age_servRet] + min(B.la[age == age_servRet], 18000) * ((1 + cola)^COLA_elig_servRet_new - 1),
				 # B.la      = B.la[age == age_servRet] + min(B.la[age == age_servRet], 18000) * 0.015 * COLA_elig_servRet_new,
				 
				 
				 # #calibration
				 # calibration only applied to current active members
				 
				 # B.la = ifelse(start_year > init_year & tier_select_ == "t6",B.la, B.la * 1.17),
				 # B.la =  ifelse(start_year > init_year & tier_select_ == "t6", B.la, B.la * (1 + 0.02)^(age - age_servRet)),
				 
				 ALx.la  = get_tla_cashflow(pxm_servRet, i, B.la)
				 )


# Save liability upon retirement for later calculations (actives)
df_AL.la_init <-
liab_la %>% 
	ungroup() %>% 
	mutate(year = start_year + age - ea) %>% 
	filter(age == age_servRet
				 ) %>% 
	select(start_year, ea, age, ALx.la) %>% 
	arrange(ea, age)

# df_AL.la_init %>% 
# 	mutate(year = start_year + age -ea ) %>% 
# 	filter(start_year == 1979, ea == 20)



# Combining results
liab_la <- 
   bind_rows(
      liab_la_init,
      liab_la %>% filter(start_year + (age_servRet - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
   									     start_year + age - ea           >= init_year + 1) # years after year 2
   ) 

cat("......DONE\n")
# liab_la %>% head


# detective work: check compatibility 
# df_AL.la_init %>% filter(start_year == 2016, ea == 20)
# 
# liab_active %>% select(year, start_year, ea, age, ax.servRet, Bx) %>% 
# 	filter(start_year == 2016, ea == 20, age %in% 55:70) %>% 
# 	mutate(AL = Bx * ax.servRet) 


# y <- 
# df_AL.la_init %>% 
# 	filter(start_year == 2016, ea == 20)
# y

# liab_la1 %>% 
# 	filter(
# 		age_servRet ==  60,
# 		ea == 22,
# 		start_year == 2016,
# 		age <=101)
# 
# x <- 
# liab_la1 %>% select(year, age_servRet, start_year, ea, age, Bx.laca, pxm_servRet, benefit_servRet, B.la, B_PV) %>% 
# 	mutate(year_servRet = start_year + age_servRet - ea) %>% 
# 	filter(
# 		age_servRet ==  60,
# 		ea == 20,
# 		year_servRet == 2015,
# 	  age <=101) %>% 
# 	
# 	group_by(age_servRet, start_year, ea)
# 
# x 
# x %<>% 
# 	as.tbl %>% 
# 	mutate(#pxm_servRet_adj = c(1, pxm_servRet[-n()]),
# 		
# 		     B.la = Bx.laca[age == age_servRet] + 100 * (row_number() - 1),
# 				 
# 		     #B_PV1 = sum(B.la * order_by(age, cumprod(pxm_servRet_adj)  *  v^(row_number() - 1) )),
# 				 B_PV  = get_tla_cashflow(pxm_servRet, i, B.la)
# 				 )
# 
# x %>% head
# x %>% filter(ea == 20)



#*******************************************************************************
#      2.2  Service Retirement: ALs and NCs for actives #####                  
#*******************************************************************************
cat("Service Retirement - actives")

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab_active %<>%   
	left_join(df_AL.la_init, by = c("start_year", "ea", "age")) %>% 
  mutate(
  	
  	TCx.laca   = qxr.la * lead(ALx.la) * v,
  	# TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
  	# TCx.ca   = lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
  	# TCx.laca =  TCx.la,   # TCx.la + TCx.ca,
  
    # PV of future benefits, PV of future salary, and PV of employee contributions
    PVFBx.laca  = c(get_PVFB(pxT[age <= max_retAge], v, TCx.laca[age <= max_retAge]), rep(0, max_age - max_retAge)),
    PVFSx       = c(get_PVFB(pxT[age <= max_retAge], v, sx[age <= max_retAge]),       rep(0, max_age - max_retAge)),
  	PVFEEC      = c(get_PVFB(pxT[age <= max_retAge], v, EEC[age <= max_retAge]),      rep(0, max_age - max_retAge)),
 
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

  # NC and AL of EAN.CD
  NCx.EAN.CD.laca = ifelse(age < max_retAge, PVFBx.laca[age == min(age)]/ayx[age == max_retAge], 0),
  PVFNC.EAN.CD.laca = NCx.EAN.CD.laca * axR,
  ALx.EAN.CD.laca = PVFBx.laca - PVFNC.EAN.CD.laca,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.laca   = ifelse(age < max_retAge, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
  PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
  ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 

cat("......DONE\n")

#*** Detective work
# x <- liab.active %>%
#   select(start.year, year, age, ea, NCx.EAN.CP.laca, PVFNC.EAN.CP.laca, ALx.EAN.CP.laca, PVFBx.laca, TCx.ca, TCx.la,TCx.laca,Bx.laca, liab.ca.sum.1, sx)
# x %>% filter(start.year == 2017, ea == 30)

# x <- liab.active %>%
#   select(start.year, year, age, ea, NCx.EAN.CP.laca, PVFNC.EAN.CP.laca, ALx.EAN.CP.laca, PVFBx.laca, TCx.la,TCx.laca,Bx.laca, qxr.la, ax.r.W, ayx, axRs, pxT)
# x %>% filter(start.year == 2017, ea == 30) 


# liab_active_newCOLA = liab_active
# save(liab_active_newCOLA, file = "liab_active_newCOLA.RData") 

# liab_active %>% select(NCx.EAN.CD.laca) %>% filter(start_year == 2016, ea == 20) %>%  head

# liab_active_newCOLA1 <- liab_active
# 
# liab_active %>% 
# 	filter(start_year == 1980,
# 		     #year == 2015, 
# 				 ea == 20,
# 				 #ea %in% 20:69,
# 				 age %in% 20:70) %>% 
# 	select(start_year, ea, age, NCx.EAN.CP.laca, ALx.EAN.CP.laca, qxr.la, ALx.la, TCx.laca, gx.laca, elig_early, elig_full) %>% 
# 	ungroup %>% 
# 	arrange(ea, age)
#***

#*******************************************************************************
#    3.1 Deferred Retirement: AL and NC for actives   #####
#*******************************************************************************
cat("Deferred Retirement - actives")

# Calculate normal costs and liabilities of deferred retirement benefits
  # Vested terms begin to receive deferred retirement benefit at age_vben, 
  # After they start receiving benefits, our understanding is that they are considered as retirees in the AV.
  # In the model, however, they always stay in the category of vested terms. 

# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age age_vben - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after min_retAge can get vested, when PVFB is only amortized up to age min_retAge


liab_active %<>%
  mutate(gx.v = yos >= v.year,  # Note that v.year is now a variable in liab_actives # ifelse(elig_vest == 1, 1, 0),  # actives become vested after reaching v.yos years of yos
         
  			 Bx.v = ifelse(ea < age_vben, 
  			 							 gx.v * Bx,    # For NYSERS, deferred retirement benefits accrue the same way as the service retirement benefits
   			 							 0),           # initial annuity amount when the vested term retires at age age_vben, when a employee is vested at a certain age. 
  			                             # May be unnecessary since we have set qxt = 0 for age>= age_vben. Left for safety. 
  			 
  			 # Bx.v = Bx.v * 0.644,  #calibration
  			 
         #TCx.v  = ifelse(ea < r.vben, Bx.v * qxt * lead(px_r.vben_m) * v^(r.vben - age) * ax.r.W[age == r.vben], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         TCx.v   = ifelse(ea < age_vben, qxt * lead(px_r.vben_m) * v^(age_vben - age) * (lead(Bx.v) * ax.term[age == age_vben])  , 0), # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         
         PVFBx.v = ifelse(ea < age_vben, c(get_PVFB(pxT[age < age_vben], v, TCx.v[age < age_vben]), rep(0, max_age - unique(age_vben) + 1)), 0),  # To be compatible with the cases where workers enter after age age_vben, max_retAge is used instead of min_retAge, which is used in textbook formula(winklevoss p115).
         
         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < age_vben, PVFBx.v[age == min(age)]/ayx[age == age_vben], 0),
  			 PVFNC.EAN.CD.v = NCx.EAN.CD.v * axr,
         ALx.EAN.CD.v = PVFBx.v - PVFNC.EAN.CD.v,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < age_vben, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == age_vben]) * sx, 0), 
         PVFNC.EAN.CP.v = NCx.EAN.CP.v * axrs,
         ALx.EAN.CP.v = PVFBx.v - PVFNC.EAN.CP.v
  ) 
  
# x <- liab_active %>% filter(start_year == 2017, ea == 20) %>% select(start_year, ea, age, gx.v, Bx.v, TCx.v, PVFBx.v,  NCx.EAN.CP.v, ALx.EAN.CD.v, px_r.vben_m, ax.terms, qxd)
# x$qxd

cat("......DONE\n")



#*******************************************************************************
#        3.2 Deferred Retirement:  AL for vested terminated members      #####
#*******************************************************************************
cat("Deferred Retirement - retirees")

## Calculate AL and benefit payment for initial vested terms.
#  This part will not be needed if an simplified approach is taken for initial vested members.


# init_terminated_ %<>%  
#   mutate(year = init.year,
#          age.term = age - 1)         # assume all terms are terminated in init.year - 1.
#          #yos = age - ea,
#          #start.year = year - (age - ea))
# 
# # init_terminated_
# 
# 
# # liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
# #                               age.term   = unique(init_terminated_$age.term),
# #                               start.year = unique(init_terminated_$start.year),
# #                               age = range_age) %>%
# 
# liab.term.init <- expand.grid(age.term = unique(init_terminated_$age.term),
#                               age = range_age) %>% 
#   mutate(ea   = min(init_terminated_$ea),
#          year = init.year + (age - age.term - 1),
#          start.year = year - (age - ea)
#          ) %>% 
#   filter(start.year + age - ea >= 1,
#          age >= ea,
#          age.term >= ea) %>%
#   left_join(init_terminated_ %>% select(age.term, age, benefit.term = benefit)) %>%
#   left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m, ax.vben)) %>%
#   left_join(decrement.model_ %>% select(start.year, ea, age, pxm.term)) %>% 
#   group_by(start.year, ea, age.term) %>%
# 
#   mutate(
#     year = start.year + age - ea,
#     age.ben =  ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 
#     year.term = year[age == age.term],
# 
#     COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
#     ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     
#     
#     Bx.v  = benefit.term,
#     
#     B.v   = ifelse(age.ben > r.vben, 0,    ifelse(age >= r.vben, Bx.v[age == unique(age.term) + 1]  * COLA.scale/COLA.scale[age == r.vben],  0)), # Benefit payment after r.vben, for age.ben == r.vben
#     B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term) + 1] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
#     ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term) + 1] * ax.vben[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
#                    B.v * ax.vben)
#     ) %>%                                                                                     # liab after receiving benefits      
#   ungroup %>%
#   select(ea, age, start.year, year, year.term, B.v, ALx.v, ax.vben, pxm.term) %>%
#   filter(year %in% seq(init.year, len = nyear),
#          year.term == init.year - 1)


##  Calculate AL and benefit payment for vested terms that terminates after year 1.
#   Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab_term <- expand.grid(
                         start_year   = min_year:(init_year + nyear - 1),   # (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
                         ea  = range_ea[range_ea < age_vben ],
                         age = range_age,
                         age_term = range_age[range_age <= age_vben]) %>%
  filter(start_year + max_age - ea >= init_year,
         age >= ea, 
  			 age_term >= ea,
         age >= age_term) %>% # drop redundant combinations of start_year and ea.
  data.table(key = "ea,age,start_year,age_term")


liab_term <- merge(liab_term,
                   # select(liab_active, start_year, year, ea, age, Bx.v, pxRm, px_r.vben_m, ax.vben, pxm.term) %>% data.table(key = "ea,age,start_year"),
									 select(liab_active, start_year, year, ea, age, Bx.v, px_r.vben_m, ax.term, pxm_term) %>% data.table(key = "ea,age,start_year"),
                   all.x = TRUE, by = c("ea", "age","start_year")) %>% as.data.frame


liab_term %<>%
  group_by(start_year, ea, age_term) %>%
  mutate(year_term = year[age == age_term],

         COLA.scale = (1 + cola)^(age - min(age)),       # COLA.scale in liab.active does not trace back long enough
         ax.term   = get_tla(pxm_term, i, COLA.scale),   # COLA.scale in liab.active does not trace back long enough

         B.v   = ifelse(age >= age_vben, Bx.v[age == unique(age_term)] * COLA.scale/COLA.scale[age == age_vben], 0),  # Benefit payment after age_vben
         ALx.v = ifelse(age <  age_vben, Bx.v[age == unique(age_term)] * ax.term[age == age_vben] * px_r.vben_m * v^(age_vben - age),
                        B.v * ax.term)

  ) %>%
  ungroup  %>%
  select(ea, age, age_term, start_year, year, year_term, B.v, ALx.v, Bx.v, ax.term, px_r.vben_m) %>%
  filter(year %in% seq(init_year, len = nyear))

cat("......DONE\n")


#*** Detective work START ******************************************************
# liab_term %>% filter(start_year == 2006, ea == 20, age_term == 49)
# 
# liab_active %>%
# 	filter(start_year == 1991, ea == 20, year %in% 2016:2018) %>%
# 	select(start_year, year, ea, age, ALx.EAN.CD.v, NCx.EAN.CD.v, Bx.v, TCx.v, qxm_terms, qxt) %>% 
# 	mutate(TCx.v_head = TCx.v/ (qxt*v) )
# 
# 
# liab_term %>% filter(start_year == 1991, year %in% 2016:2045, ea == 20, age_term %in% 46) %>% arrange(age_term)



# liab.term %<>% mutate(B.v   = ifelse(year.term == init.year - 1, 0, B.v),
#                       ALx.v = ifelse(year.term == init.year - 1, 0, ALx.v))

# liab.term %>% filter(start.year ==2016, ea == 30, year.term == 2025) %>%
#   select(start.year, ea, year.term, age, year, B.v, ALx.v, ax.vben,  px_r.vben_m, pxm.term)



# liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
#                              filter(liab.term, year.term != init.year - 1)))


# liab.term %>% filter(year.term == 2014, start.year == 1980) %>% head
# liab.term[!duplicated(liab.term %>% select(start.year, ea, age, year.term)),]
#   any(T)


# Placeholder
# liab_term <- expand.grid(# start.year   = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
#                          start_year   = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
#                          ea = range_ea[range_ea < max_retAge],
#                          age = range_age,
#                          age_term = range_age[range_age < max_retAge]) %>%
#   filter(start_year + max_age - ea >= init_year,
#          age >= ea, 
#   			 age_term >= ea,
#          age >= age_term) %>% 
# 	mutate(year      = start_year + age - ea,
# 				 year_term = start_year + age_term - ea,
# 		     B.v = 0,
# 				 ALx.v = 0) %>% 
# 	filter(year %in% seq(init_year, len = nyear))
#*** Detective work END*********************************************************




#*******************************************************************************
#         4.1  Disability Retirement: ALs and NCs for actives            #####
#*******************************************************************************
cat("Disability Retirement - actives")


# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages
liab_active %<>% 
  mutate( gx.disbRet  = yos >= 10,
  				yos_proj_disbRet = ifelse(age <= 60, 60 - ea, yos), 
          
  				# NYSERS disability benefit policy
  				Bx.disbRet  = gx.disbRet * pmax(
          	                              pmin(1/3 * fas, 1/60 * yos_proj_disbRet * yos), 
          																1/60 * yos * fas, 
          																na.rm = TRUE),
          
  				# Bx.disbRet = Bx.disbRet * adj_fct.act.disbRet,
  				 
          # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x.
  				TCx.disbRet = qxd * v * lead(Bx.disbRet) *  lead(ax.disbRet), 
  				
  				# TCx.disb.la = qxd.la * v * lead(Bx.disb) *  lead(ax.disb.la) , # term cost of life annuity at the disability age x (start to claim benefit at age x + 1)
          # TCx.disb.ca = qxd.ca * v * lead(Bx.disb) *  lead(liab.disb.ca.sum.1),
          # TCx.disb.laca = TCx.disb.la + TCx.disb.ca,

          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.disbRet  = c(get_PVFB(pxT[age <= max_retAge ], v, TCx.disbRet[age <= max_retAge]), rep(0, max_age - max_retAge)),

          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),

          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

          # NC and AL of EAN.CD
          NCx.EAN.CD.disbRet = ifelse(age < max_retAge, PVFBx.disbRet[age == min(age)]/ayx[age == max_retAge], 0),
  				PVFNC.EAN.CD.disbRet =  NCx.EAN.CD.disbRet * axR,
  				ALx.EAN.CD.disbRet = PVFBx.disbRet - PVFNC.EAN.CD.disbRet,

          # NC and AL of EAN.CP
          NCx.EAN.CP.disbRet   = ifelse(age < max_retAge, sx * PVFBx.disbRet[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
  				PVFNC.EAN.CP.disbRet = NCx.EAN.CP.disbRet * axRs,
          ALx.EAN.CP.disbRet   = PVFBx.disbRet - PVFNC.EAN.CP.disbRet
  )

# liab_active %>% filter(start_year == 2016, ea == 20) %>% 
# 	select(start_year, ea, age, gx.disbRet, fas, Bx.disbRet)

cat("......DONE\n")

#*******************************************************************************
#     4.2 Disability Benefit: ALs and benefits for disabled retirees       #####
#*******************************************************************************

cat("Disability Retirement - retirees")
 

if(nrow(filter(benefit_disbRet_, benefit_disbRet != 0)) == 0) {
	liab_disbRet_init <- NULL
} else {

liab_disbRet_init <- 
	
	# grids for initial disability retirees in year 1
	# To simplified the calculation, it is assmed that all initial disabled entered the workforce at age min.age and
	# become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligiblility rules,
	# but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial disabled.
	
	expand.grid(age_disbRet = filter(benefit_disbRet_, benefit_disbRet != 0)$age, # This ensures that year of retirement is year 1.
							age         = range_age[range_age >= min(filter(benefit_disbRet_, benefit_disbRet != 0)$age)]) %>%
	mutate(ea            = unique(benefit_disbRet_$ea),
				 start_year    = init_year - (age_disbRet - ea)) %>%
	filter(age >= age_disbRet)
}	

liab_disbRet <-
  # grids for who die after year 1.
  expand.grid(ea           = range_ea[range_ea < max_retAge],
              age_disbRet  = min_age:max_retAge,
              start_year   = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
              age          = range_age) %>%
    filter(age         >= ea,
           age_disbRet >= ea,
           age         >= age_disbRet,
           start_year + (age_disbRet - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
           start_year + age - ea >= init_year + 1)           # not really necessary since we already have age >= age.r


liab_disbRet <- 
	bind_rows(
		liab_disbRet_init,
		liab_disbRet
	) %>%
  data.table(key = "start_year,ea,age_disbRet,age")


# liab_disb <- liab_disb[!duplicated(liab_disb %>% select(start_year, ea, age, age_disbRet ))]
# x <- liab.disb.la %>% mutate(year   = start.year + age - ea, year.disb = start.year + age.disb - ea) %>% filter(year == 2015, year.disb == 2015)
# x
# benefit.disb_%>% mutate(year   = start.year + age - ea, year.disb = start.year + age.disb - ea)


liab_disbRet <- merge(liab_disbRet,
                      select(liab_active, start_year, ea, age, Bx.disbRet, COLA.scale, gx.disbRet, ax.disbRet) %>% data.table(key = "ea,age,start_year"),
                      all.x = TRUE,
                      by = c("ea", "age","start_year")) %>%
  arrange(start_year, ea, age_disbRet) %>%
  as.data.frame %>%
  left_join(benefit_disbRet_, by = c("ea", "age", "start_year", "age_disbRet")) %>%
  left_join(decrement_model_ %>% select(start_year, ea, age, pxm_disbRet), by = c("start_year", "ea", "age"))  # need to add start_year if we use generational table in the future

#%>%
# left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore.

#liab.disb.la %>% as.data.frame %>% mutate(year = start.year + age - ea) %>%
 # filter(year == 2015, age.disb == age)

liab_disbRet %<>% as.data.frame  %>%
  group_by(start_year, ea, age_disbRet) %>%
  mutate(
    year = start_year + age - ea,

    COLA.scale    = (1 + cola)^(age - min(age)),          # COLA.scale in liab.active does not trace back long enough, may not be necessary now
    ax.disbRet    = get_tla(pxm_disbRet, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough, may not be necessary now
    year_disbRet  = start_year + age_disbRet - ea,        # year of disability of the active

    Bx.disbRet    = ifelse(is.na(Bx.disbRet), 0, Bx.disbRet),  # just for safety
    B.disbRet     = ifelse(year_disbRet <= init_year,
                           benefit_disbRet[year == init_year] * COLA.scale / COLA.scale[year == init_year],  # Benefits for initial disability retirees
                           Bx.disbRet[age == age_disbRet] * COLA.scale / COLA.scale[age == age_disbRet]),    # Benefits for disability retirees after year 1
    ALx.disbRet   = B.disbRet * ax.disbRet                                                                   # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA

  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init_year, len = nyear) ) %>%
  select(year, ea, age, year_disbRet, age_disbRet, start_year, B.disbRet, ALx.disbRet)
  #%>%   arrange(age.disb, start.year, ea, age)


# liab.disb %>% ungroup %>% arrange(start.year, ea, year.disb, age) %>%  head(100)

cat("......DONE\n")


#*******************************************************************************
#   5.1  Death benefit (before retirement): ALs, NCs for actives           #####
#*******************************************************************************


# TRS Death benefit 1: the greater of 
# 1 month's salary for each yos, up to max 3 years' salary, or 
# If eligible to unreduced retirement benefit, lump sum actuarially equivalent to retirement allowance. (PV of all future payments? )  


cat("Death Benefits - actives")
# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages
liab_active %<>%
	mutate( gx.death  = 1,
					
					# NYSERS death benefit policy
					Bx.death = pmax(0, gx.death * sx * 3 - 50000),
					
					
					# Bx.death = gx.death * sx * pmin(3, yos), # annuity that would have been effective if the member retired on the
					# Bx.death = gx.death * pmax(Bx.death, elig_full * Bx.laca * ax.servRet, na.rm = TRUE), 
					# Bx.death = gx.death * pmax(Bx.death, elig_full * Bx.laca, na.rm = TRUE), 
					
					# Bx.death = Bx.death * adj_fct.act.death,
					
					# This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x.
					# For TRS: 1. Lump sum death benefit equal to PV of future benefit (Bx.death * ax.deathBen);
					#            2. Death benefit are assumed to be claimed 1 year after death
					TCx.death = qxm_active * v * lead(Bx.death) , # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
					
					# TCx.r = Bx.r * qxr.a * ax,
					PVFBx.death  = c(get_PVFB(pxT[age <= max_retAge], v, TCx.death[age <= max_retAge]), rep(0, max_age - max_retAge)),
					
					## NC and AL of UC
					# TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
					# NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
					# ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
					
					# # NC and AL of PUC
					# TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost
					# NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
					# ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
					
					
					## Under EAN methods, costs are spread up to r.max
					# NC and AL of EAN.CD
					NCx.EAN.CD.death = ifelse(age < max_retAge, PVFBx.death[age == min(age)]/ayx[age == max_retAge], 0),
					PVFNC.EAN.CD.death =  NCx.EAN.CD.death * axR,
					ALx.EAN.CD.death = PVFBx.death - PVFNC.EAN.CD.death,
					
					# NC and AL of EAN.CP
					NCx.EAN.CP.death   = ifelse(age < max_retAge, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
					PVFNC.EAN.CP.death = NCx.EAN.CP.death * axRs,
					ALx.EAN.CP.death   = PVFBx.death - PVFNC.EAN.CP.death
	)
cat("......DONE\n")

# liab_active %>% filter(start_year == 2016, ea == 30) %>% 
# 	select(start_year, year, ea, age, yos, Bx.death, Bx.death1, elig_full, Bx.laca, ax.servRet)



#*******************************************************************************
#  5.2   Death benefit (before retirement): AL and benefit for recipients  #####
#*******************************************************************************

cat("Death Benefits - beneficiaries")

liab_death <- rbind(
	# grids for who die after year 1.
	expand.grid(ea        = range_ea[range_ea < max_retAge],
							age_death = min_age:max_retAge,
							start_year   = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
							age          = min_age:max_retAge) %>% # ONLY good for lump sum benefit!
		filter(age   >= ea,
					 age_death >= ea,
					 age   >= age_death,
					 start_year + (age_death - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
					 start_year + age - ea >= init_year + 1) # not really necessary since we already have age >= age.r
)

# %>%
# data.table(key = "start.year,ea,age.death,age")
# liab.death %<>% mutate(B.death = 0, ALx.death = 0)


liab_death <- merge(liab_death,
										select(liab_active, start_year, ea, age, Bx.death, gx.death, ax.deathBen) %>% data.table(key = "ea,age,start_year"),
										all.x = TRUE,
										by = c("ea", "age","start_year")) %>%
	# arrange(start_year, ea, age_death) %>%
	as.data.frame
# %>%
# left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore.
# left_join(benefit_)


liab_death %<>% as.data.frame  %>%
	group_by(start_year, ea, age_death) %>%
	mutate(
		
		# COLA.scale = (1 + cola)^(age - min(age)),         # COLA.scale in liab.active does not trace back long enough
		# ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale), # COLA.scale in liab.active does not trace back long enough
		
		year       = start_year + age - ea,
		year_death = start_year + age_death - ea, # year of death of the active
		Bx.death   = ifelse(is.na(Bx.death), 0, Bx.death),  # just for safety
		
		# For TRS: Lump sum death benefit 
		B.death    = ifelse(age == age_death, Bx.death, 0),   # Bx.death[age == age.death] * COLA.scale / COLA.scale[age == age.death],               # Benefits for retirees after year 1
		ALx.death  = ifelse(age == age_death, B.death, 0)                   # B.death * ax.deathBen                                                                # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
		
	) %>% ungroup %>%
	# select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
	filter(year %in% seq(init_year, len = nyear) ) %>%
	select(year, ea, age, year_death, age_death, start_year, B.death, ALx.death) %>%
	arrange(age_death, start_year, ea, age)

# liab.death %>% ungroup %>% arrange(start.year, ea, year.death, age) %>%  head(100)
cat("......DONE\n")




#*******************************************************************************
#                  6.  Constructing final outputs                          #####
#*******************************************************************************


liab_active %<>% ungroup %>% select(start_year, year, ea, age, everything())


# Choosing AL and NC variables corresponding to the chosen acturial method
ALx.laca.method     <- paste0("ALx.", actuarial_method, ".laca")
NCx.laca.method     <- paste0("NCx.", actuarial_method, ".laca")
PVFNC.laca.method   <- paste0("PVFNC.", actuarial_method, ".laca")

ALx.death.method   <- paste0("ALx.", actuarial_method, ".death")
NCx.death.method   <- paste0("NCx.", actuarial_method, ".death")
PVFNC.death.method <- paste0("PVFNC.", actuarial_method, ".death")

ALx.disbRet.method     <- paste0("ALx.", actuarial_method, ".disbRet")
NCx.disbRet.method     <- paste0("NCx.", actuarial_method, ".disbRet")
PVFNC.disbRet.method   <- paste0("PVFNC.", actuarial_method, ".disbRet")

ALx.v.method    <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method    <- paste0("NCx.", actuarial_method, ".v")
PVFNC.v.method  <- paste0("PVFNC.", actuarial_method, ".v")


var.names <- c(ALx.laca.method,    NCx.laca.method,    PVFNC.laca.method,
							 ALx.v.method,       NCx.v.method,       PVFNC.v.method,
               ALx.death.method,   NCx.death.method,   PVFNC.death.method,
               ALx.disbRet.method, NCx.disbRet.method, PVFNC.disbRet.method,
                
							 "sx", 
							 "EEC",
							 
						   "PVFBx.laca", 
						   "PVFBx.v", 
						   "PVFBx.death", 
						   "PVFBx.disbRet",
						   
							 "PVFSx",
						   "PVFEEC"
						   )

liab_active %<>% 
  filter(year %in% seq(init_year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename("ALx.laca"   = ALx.laca.method,  
  			 "NCx.laca"   = NCx.laca.method,  
  			 "PVFNC.laca" = PVFNC.laca.method, 
          
  			 "ALx.v"      = ALx.v.method,     
  			 "NCx.v"      = NCx.v.method,     
  			 "PVFNC.v"    = PVFNC.v.method,
          
  			 "ALx.death"  = ALx.death.method, 
  			 "NCx.death"  = NCx.death.method, 
  			 "PVFNC.death"= PVFNC.death.method,
          
  			 "ALx.disbRet"   = ALx.disbRet.method,  
  			 "NCx.disbRet"   = NCx.disbRet.method,  
  			 "PVFNC.disbRet" = PVFNC.disbRet.method
          )   # Note that dplyr::rename_ is used. 


## Final outputs
  # liab.active
  # liab.la
  # liab.term
  # B.LSC

liab <- list(active = liab_active, 
             la = liab_la, 
             term = liab_term,
             disbRet = liab_disbRet,
						 death = liab_death
						 )

}


# liab <- get_indivLab("t4a")


# liab <- get_indivLab(decrement.ucrp,
#                      salary,
#                      benefit,
#                      bfactor,
#                      init_terminated.t76)



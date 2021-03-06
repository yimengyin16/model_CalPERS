## This script conducts the simulation of the finance of the plan

## What's new in version (4)
#    - contingent COLA with multiple tiers. 

# Temp: initial cola_acual = 0.02



run_sim <- function(i.r_ = i.r,
                    sim_paramlist_ = sim_paramlist,
                    Global_paramlist_ = Global_paramlist){

  # Run the section below when developing new features.
  
  # i.r_ = i.r
  # sim_paramlist_ = sim_paramlist
  # Global_paramlist_ = Global_paramlist

  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(sim_paramlist_,    envir = environment())
  
  
  valData <- readRDS(paste0(dir_val, "val_", sim_paramlist_$val_name, ".rds"))
  # note that "dir_val" is defined outside the function
  #tn <- names(valData$aggLiab)[1] # this will be replaced by the tag for aggregate results, like "agg" 
  tn <- "sumTiers"
  tier_names <- names(valData$indivLiab)
  
  #*****************************************************************************
  #              Special settings for modeling CalPERS PERF A      ####
  #***************************************************************************** 
  
  # Set initial total amortization basis
  #  
  
  if(use_baselineUAAL){
    df_baseline <- readRDS(paste0(dir_outputs, "sim_", sim_name_baseline, ".rds"))$results
    UAAL.year1.baseline <- df_baseline %>% filter(sim == 0, year == init_year) %>% pull(UAAL)
  }
  
  
  # Set initial assets
  
  if(use_baselineMA){
    df_baseline <- readRDS(paste0(dir_outputs, "sim_", sim_name_baseline, ".rds"))$results
    
    init_MA_type <- "MA0"
    init_AA_type <- "AA0"
    
    MA_0 <- df_baseline %>% filter(sim == 0, year == init_year) %>% pull(MA)
    AA_0 <- df_baseline %>% filter(sim == 0, year == init_year) %>% pull(AA)
    
  }
  
  
  
  
  #*****************************************************************************
  #                       Preparation for contingent COLA                   ####
  #***************************************************************************** 
  # Only apply contingent COLA to service retirees

  ls_servRet0 <- list()
  
  for(tierName in tier_names){
  
  ls_servRet0[[tierName]] <-
    left_join(
      valData$pop[[tierName]]$wf_servRet.la,
      valData$indivLiab[[tierName]]$servRet.la,
      by =  c("ea", "age", "year", "year_servRet")
    ) %>%
    mutate(across(everything(), na2zero)) %>%
    mutate(start_year  = year - (age - ea),
           age_servRet = age - (year - year_servRet)
           ) %>%
    select(start_year, ea, age, age_servRet, year_servRet, year, B.servRet.la, ALx.servRet.la, n_servRet.la, ax.servRet)

  ls_servRet0[[tierName]] %<>%
    mutate(B.servRet.la   = ifelse(year_servRet == year , B.servRet.la, 0),
           ALx.servRet.la = ifelse(year == init_year , ALx.servRet.la, 0)) %>%
    filter(age >= age_servRet) %>%
    arrange(start_year, ea, age_servRet, age) %>% 
    
    group_by(start_year, ea, age_servRet) %>% 
    mutate(n_sum = sum(n_servRet.la)) %>% 
    ungroup() %>% 
    filter(n_sum != 0)
   
  }

  # ls_servRet0$misc_classic
    
       
  #*****************************************************************************
  #                       Defining variables in simulation ####
  #***************************************************************************** 
  
  # Now we do the actuarial valuations 
  # In each period, following values will be caculated:
  # AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
  # NC: Normal Cost  
  # MA: Market value of assets.
  # AA: Actuarial value of assets.
  # EAA:Expected actuarial value of assets.
  # UAAL: Unfunded accrued actuarial liability, defined as AL - NC
  # EUAAL:Expected UAAL.
  # PR: payroll 
  # LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
  # AM: Amount to be amortized at period t. 
  # i is assumed interest rate. ELs of each period will be amortized seperately.  
  # SC: Supplement cost 
  # ADC: actuarially required contribution by employer. NC + SC - EEC
  # C : Actual contribution
  # C_ADC: shortfall in paying ADC
  # B : Total beneift Payment   
  # Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
  # Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
  # Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
  # I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
  # Funded Ratio: AA / AL
  # C_PR: contribution as % of payroll
  
  # Formulas
  # AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
  # MA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
  # EAA(t+1)= AA(t) + EI(t)
  # AA(t+1) = (1-w)*EAA(t+1) + w*MA(t+1)
  # I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
  # Ia(t) = i * AA(t)
  # Ib(t) = i * B(t)
  # Ic(t) = i * C(t)
  # EI(t) = Ia(t) - Ib(t) + Ic(t)
  # ADC   = NC(t) + SC(t)
  # ADC.ER = NC(t) + SC(t) - EEC(t)
  # C(t) = NC(t) + SC(t)
  # UAAL(t) = AL(t) - AA(t)
  # EUAAL(t) = [UAAL(t-1) + NC(t-1)](1+i(t-1)) - C(t-1) - Ic(t-1)
  # LG(t) =   UAAL(t) - EUAAL for t>=2 ; LG(1) = -UAAL(1) (LG(1) may be incorrect, need to check)
  # More on LG(t): When LG(t) is calculated, the value will be amortized thourgh m years. This stream of amortized values(a m vector) will be 
  # placed in SC_amort[t, t + m - 1]
  # SC = sum(SC_amort[,t])
  # ExF = B(j) - C(j)
  
  # About gains and losses
  # In this program, the only source of gain or loss is the difference between assumed interest rate i and real rate of return i.r,
  # which will make I(t) != Ia(t) + Ic(t) - Ib(t)
  
  # Set up data frame
  penSim0 <- data.frame(year = init_year:(init_year + nyear - 1)) %>%
    mutate(
    	     # standard variables
    	     AL   = 0, #
           MA   = 0, #
           AA   = 0, #
           EAA  = 0, #
           FR   = 0, #
           ExF  = 0, # 
           UAAL = 0, #
           EUAAL= 0, #
           LG   = 0, #
           Amort_basis  = 0, # amount to be amortized: AM(t) = LG(t) + [ADC(t - 1) - C(t-1)]*[1 + i(t-1)], i.e. actuarial loss/gain plus shortfall in paying NC+SC in last period(plus interests) 
           
           NC   = 0, #
           SC   = 0, #
           EEC  = 0, #
           ERC  = 0, #
           ADC  = 0, #
           ADC.ER = 0, #
           C    = 0, #
           C_ADC= 0, #
           
    	     B    = 0, #                        
           
    	     I.r  = 0, #                        
           I.e  = 0, #
           I.dif= 0,
           Ia   = 0, #                         
           Ib   = 0, #                         
           Ic   = 0, #  
           i    = i,
           i.r  = 0,
           
    	     PR   = 0,
    	     
    	     nactives  = 0,
           nretirees = 0,
           nterms    = 0,
    			 
    	     ADC_PR = 0,
    	     C_PR = 0,
    			 
    	     # Contingent COLA
    	     cola_actual = 0.02,
    	     #AL_lowerDR  = 0,
    	     FR_MA_lowerDR  = 0
    	     
    			 ## Additional/plan specific variables


    			    			 )
  # penSim0 <- as.list(penSim0)
  
  #valData$aggLiab[[tn]]$active

  #*****************************************************************************
  #                      Defining variables in simulation  ####
  #*****************************************************************************
 
  # AL(j)
   # AL for active members 
  penSim0$AL.active.servRet <- valData$aggLiab[[tn]]$active[, "ALx.servRet.laca"]
  penSim0$AL.active.defrRet <- valData$aggLiab[[tn]]$active[, "ALx.defrRet"]
  penSim0$AL.active.disbRet <- valData$aggLiab[[tn]]$active[, "ALx.disbRet"]
  penSim0$AL.active.death   <- valData$aggLiab[[tn]]$active[, "ALx.death"]
  penSim0$AL.active <-  
    with(penSim0, AL.active.servRet + 
                  AL.active.defrRet + 
                  AL.active.disbRet + 
                  AL.active.death)
  
   # AL for members in pay status
  penSim0$AL.servRet    <- valData$aggLiab[[tn]]$servRet.la[,   "ALx.servRet.la"]
  penSim0$AL.defrRet    <- valData$aggLiab[[tn]]$defrRet[, "ALx.defrRet"]
  penSim0$AL.disbRet    <- valData$aggLiab[[tn]]$disbRet[, "ALx.disbRet"]
  penSim0$AL.death      <- valData$aggLiab[[tn]]$death[,   "ALx.death"]
  #penSim0$AL.ca    <- AggLiab_$ca[,   "liab.ca.yearsum"]
  penSim0$AL.nonactive <- 
    with(penSim0, AL.servRet + 
                  AL.defrRet + 
                  AL.disbRet + 
                  AL.death)
   # Total AL
  penSim0$AL <- with(penSim0, AL.active + AL.nonactive)
  
  # NC(j)
  penSim0$NC.servRet <- valData$aggLiab[[tn]]$active[, "NCx.servRet.laca"]
  penSim0$NC.defrRet <- valData$aggLiab[[tn]]$active[, "NCx.defrRet"]
  penSim0$NC.disbRet <- valData$aggLiab[[tn]]$active[, "NCx.disbRet"]
  penSim0$NC.death   <- valData$aggLiab[[tn]]$active[, "NCx.death"]
  
  penSim0$NC         <- with(penSim0, NC.servRet + NC.defrRet + NC.disbRet + NC.death)
  
  # PVFB(j)
  penSim0$PVFB.active.servRet <- valData$aggLiab[[tn]]$active[, "PVFBx.servRet.laca"]
  penSim0$PVFB.active.defrRet <- valData$aggLiab[[tn]]$active[, "PVFBx.defrRet"]
  penSim0$PVFB.active.disbRet <- valData$aggLiab[[tn]]$active[, "PVFBx.disbRet"] 
  penSim0$PVFB.active.death   <- valData$aggLiab[[tn]]$active[, "PVFBx.death"]

  penSim0$PVFB.active    <- with(penSim0, PVFB.active.servRet + PVFB.active.defrRet + PVFB.active.disbRet + PVFB.active.death) 
  penSim0$PVFB.nonactive <- with(penSim0, AL.nonactive)
  penSim0$PVFB           <- with(penSim0, PVFB.active + PVFB.nonactive) 
  
  # Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # PVFNC(j)
  penSim0$PVFNC.servRet   <- valData$aggLiab[[tn]]$active[, "PVFNCx.servRet.laca"]
  penSim0$PVFNC.defrRet   <- valData$aggLiab[[tn]]$active[, "PVFNCx.defrRet"]
  penSim0$PVFNC.disbRet   <- valData$aggLiab[[tn]]$active[, "PVFNCx.disbRet"] 
  penSim0$PVFNC.death     <- valData$aggLiab[[tn]]$active[, "PVFNCx.death"]
 
  penSim0$PVFNC <-  with(penSim0, PVFNC.servRet + PVFNC.defrRet + PVFNC.disbRet + PVFNC.death)
  
  # B(j)
  penSim0$B.servRet <- valData$aggLiab[[tn]]$servRet.la[, "B.servRet.la"]
  penSim0$B.defrRet <- valData$aggLiab[[tn]]$defrRet[, "B.defrRet"]
  penSim0$B.disbRet <- valData$aggLiab[[tn]]$disbRet[, "B.disbRet"]
  penSim0$B.death   <- valData$aggLiab[[tn]]$death[,   "B.death"]
  
  # penSim0$B.ca    <- valData$aggLiab[[tn]]$ca[, "B.ca"]
  
  penSim0$B          <- with(penSim0, B.servRet + B.defrRet + B.disbRet + B.death) 
  
  # PR(j)
  penSim0$PR  <- valData$aggLiab[[tn]]$active[, "PR"]
  
  # EEC(j)
  penSim0$EEC <- valData$aggLiab[[tn]]$active[, "EEC"]
  
  # PVFEEC(j)
  penSim0$PVFEEC <- valData$aggLiab[[tn]]$active[, "PVFEEC"]

  # PVFS(j)
  penSim0$PVFS <- valData$aggLiab[[tn]]$active[, "PVFSx"]
 
  # Number of members
  penSim0$n_actives  <- valData$aggLiab[[tn]]$active[, "nactives"]
  penSim0$n_servRet <- valData$aggLiab[[tn]]$servRet.la[, "n_servRet.la"]
  penSim0$n_defrRet <- valData$aggLiab[[tn]]$defrRet[, "n_defrRet.vest"]
  penSim0$n_disbRet <- valData$aggLiab[[tn]]$disbRet[, "n_disbRet"]
  penSim0$n_deathBen <- valData$aggLiab[[tn]]$death[, "n_deathBen"]
  
  #penSim0$ndisb.ca.R0S1 <- valData$aggLiab[[tn]]$disb.ca[,  "n.disb.R0S1"]
  #penSim0$n.ca.R1   <- valData$aggLiab[[tn]]$ca[, "n.R1"]
  #penSim0$n.ca.R0S1 <- valData$aggLiab[[tn]]$ca[, "n.R0S1"]
  
  # Convert penSim0 to a list when all preps are done.
  # It is faster to extract elements from lists than from frame data frames.
  penSim0 <- as.list(penSim0) 

  #
  
  
  
  
  #*****************************************************************************
  #                 Setting up initial amortization payments ####
  #***************************************************************************** 
  
  # Matrix representation of amortization of amort payment schedule: better visualization but larger size
   # Rows:    schedule of amortization payments for losses/gains occurred in a certain year. 
   # Columns: each column contains all the amortization payments that need to be paid in a simulation year. 
   #          Column i corresponds to year i. 
   #          Column sums give the total amortization payments. 
  
  # Set up the matrix for SC starting from year 1
  m.max     <- max(valData$init_amort_raw$year.remaining, m) # max number of rows and columns needed
  SC_amort0 <- matrix(0, nyear + m.max, nyear + m.max)
  # SC_amort0
  
  
  # Amortization payment amounts for losses/gains occured piror to year 1 (from plan docs). Set up the matrix. 
  SC_amort.init <- matrix(0, nrow(valData$init_amort_raw), nyear + m.max)
 
   
  # Adjustment factor for initial amortization payments 
      # Factor is defined as the initial model UAAL as a proportion of UAAL in AV.
      # WARNING: Does not work with "method 2" for AA.

   MA.year1.model <- switch(init_MA_type, 
   									    MA0 = MA_0,                         # Use preset value
   									    AL = penSim0$AL[1],                   # Assume inital fund equals inital liability.
   									    AL_pct = penSim0$AL[1] * MA_0_pct) # Inital MA is a proportion of inital AL
   
   AA.year1.model <- switch(init_AA_type, 
   												 AA0         = AA_0,                         # Use preset value
   												 noSmoothing = MA.year1.model,       # Assume inital fund equals inital liability.
   												 AL_pct      = penSim0$AL[1] * AA_0_pct)  # Inital MA is a proportion of inital AL
   
   AL.year1.model <- penSim0$AL[1]
   
   UAAL.year1.model <- AL.year1.model - AA.year1.model
   
   
   # factor.initAmort <- UAAL.year1.model / [replace with UAAL from plan doc]
   # Notes: Theoretically, the AV UAAL should be equal to the sum of outsftanding amortization balance. 
   #        Need to check the document
   # Source of the demoninator: AV2016lag page n14, sum of outstanding amortization basis
 
   # CalPERS:
   
   if(use_baselineUAAL){
     factor.initAmort <- UAAL.year1.baseline / sum(valData$init_amort_raw$balance)
   } else {
     factor.initAmort <- UAAL.year1.model    / sum(valData$init_amort_raw$balance)
   }
   
   
   
   if(useAVamort){
   	SC_amort.init.list <- mapply(amort_LG, 
   															 p = valData$init_amort_raw$balance * factor.initAmort , 
   															 m = valData$init_amort_raw$year.remaining, 
   															 method = valData$init_amort_raw$amort.method,
   															 skipY1 = valData$init_amort_raw$skipY1,
   															 MoreArgs = list(i = i, g = salgrowth_amort, end = FALSE), SIMPLIFY = F)
   	
   	for(j in 1:nrow(SC_amort.init)){
   		SC_amort.init[j, 1:valData$init_amort_raw$year.remaining[j]] <- SC_amort.init.list[[j]]
   	}
   }
  # SC_amort.init
  

  # Comibining matrices of initial amortization and new amortization
  nrow.initAmort <- nrow(SC_amort.init)
  SC_amort0 <- rbind(SC_amort.init, SC_amort0)
  
  # Notes:
  #   The amortization basis of year j should be placed in row nrow.initAmort + j - 1. 
  #   This creates the issue for year 1 that whether we should use the amortization from the document or that calculated from the model
  #   Need to think this through. 
  #   For sim -1, which is for check model consistency, all amort payments for past gains/losses are set to 0. 
  
  ## save(SC_amort0, file = "SC_amort0.RData")  
  
  
  
  #*****************************************************************************
  #                            Asset smoothing   ####
  #*****************************************************************************
  
  ## Case 1: Recognizing equal amounts every year
  
  # In year Y, The ith element of s.vector is the proportion of the investment
  # gains/losses in year Y - (s.year - i + 1) to be EXCLUDED from AA in year Y. 
  
  # E.g.The asset smoothing period is 5 (5 elements in s.vector) and the last element is 0.8,
  #     then only 20% of the investment gain/loss in year Y-1 will be recognized 
  #     in the calculation of actuarial value of assets in year Y. 
  
  s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector   
  
  
  ## Case 2: Recognizing unequal amounts every year
  # s.vector <- c(0, 0.2, 0.4, 0.55, 0.7)
  # if(length(s.vector) != s.year) warning("Incorrect asset smoothing period.") 
  
  
  ## Adjusting initil unrecognized returns for the MA-AA difference in the model

  init_unrecReturns.adj <-  mutate(valData$init_unrecReturns.unadj,
  																 DeferredReturn = DeferredReturn * (MA.year1.model -  AA.year1.model)/sum(DeferredReturn),
  																 DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
                                   ) %>% 
                            mutate(across(.fns = na2zero))
  # init_unrecReturns.adj
  
  
  
  #*****************************************************************************
  #                              Simuation  ####
  #*****************************************************************************
    
  cl <- makeCluster(ncore) 
  registerDoParallel(cl)
  
  penSim_results <- foreach(k = -2:nsim, .packages = c("dplyr", "tidyr", "purrr")) %dopar% {
    # k <- 0
    # initialize
    penSim   <- penSim0
    SC_amort <- SC_amort0
    ls_servRet <- ls_servRet0
    
    if(k == -1) SC_amort[,] <- 0
    
    penSim[["i.r"]] <- i.r_[, as.character(k)]
    # penSim[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
    
    source("functions.R")
    
    for (j in 1:nyear){
        
    # it_j <- iterators::iter(1:nyear)
    # j    <- iterators::nextElem(it_j); j
      
    	
    	#***********************************
      #   1. MA(j), AA(j) and UAAL(j)   **
      #***********************************
    	
    	# Year 1
    	if(j == 1) {penSim$MA[j]  <- ifelse(k == -1, penSim$AL[j],                      # k = -1 is for testing model consistency
                                          switch(init_MA_type, 
                                          			 MA0    = MA_0,                       # Use preset value
                                                 AL     = penSim$AL[j],               # Assume inital fund equals inital liability.
                                                 AL_pct = penSim$AL[j] * MA_0_pct)    # Inital MA is a percentage of the inital AL
                                          )
                  
    	            penSim$AA[j]  <- ifelse(k == -1, penSim$MA[j],                       # AA = MA  (always true for if k == -1 regardless of the value of init_AA_type)
                  												switch(init_AA_type,
                  															 AA0         = AA_0,                   # Use preset value
                  															 noSmoothing = penSim$MA[j],           # Intial AA is equal to MA
                  															 AL_pct      = penSim$AL[j] * AA_0_pct # Inital AA is a proportion of inital AL
                  															 
                  															 )
                  												)
                 #** Vintage code. 
                 # penSim$AA[j]  <- ifelse(init_AA_type == "AL_pct" & k != -1, penSim$AL[j] * AA_0_pct,     # Inital MA is a proportion of inital AL;  
                 #                             ifelse(init_AA_type == "AA0" & k != -1, AA_0,                # Use preset value 
                 #                                    switch(smooth_method,
                 #                                           method1 =  with(penSim, MA[j]),                # AA = MA  (always true for if k == -1 regardless of the value of init_AA_type)
                 #                                           method2 =  with(penSim, MA[j])
                 #                                    			 ) 
                 #                             			 )
                 # 												)
                 #***                
                 
      # Year 2 and after                      
      } else {
        penSim$MA[j]  <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim$EAA[j] <- with(penSim, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),  # MA minus unrecognized losses and gains
                                method2 = with(penSim, MA[j]) 
        
        )
      }

      ## Incorporating initial unrecognized returns
       # - The unrecognized returns/losses, which are the differences between initial MA and AA, 
    	 #   will be recognized over time. 
    	 # - The schedule is constructed based on info in plan document and the asset smoothing policy
    	 # - The schedule from the plan document is adjusted for the MA-AA difference in the model, which may not be always equal to the document value. 
    	
    	if((init_AA_type %in% c("AL_pct", "AA0")) & useAVunrecReturn & k != -1 ){
       
        # Adjust AA for inital unrecognized returns
        if((j - 1 + init_year) %in% init_unrecReturns.adj$year) penSim$AA[j] <- penSim$AA[j] - with(init_unrecReturns.adj, DeferredReturn.annualTot[year == (j - 1 + init_year)])
      }
       
    	
      ## Apply corridor for MA, MA must not deviate from AA by more than 20%.
    	
    	if(corridor){
        penSim$AA[j] <- with(penSim, ifelse(AA[j] > s.upper * MA[j], s.upper * MA[j], AA[j])) 
        penSim$AA[j] <- with(penSim, ifelse(AA[j] < s.lower * MA[j], s.lower * MA[j], AA[j]))
    	}

      
      #******************************************
      #    Liability and funded status    **
      #******************************************
      
      # j <- 2018
      # cola_actual <- 0.01
    
    if(useContingentCOLA){  
        
      if(j > 1) {
        
        for(tierName in tier_names){
        
        ls_servRet[[tierName]] <- 
          mutate(ls_servRet[[tierName]],
                 B.servRet.la   = ifelse(year == (init_year + j - 1) & year > year_servRet, 
                                         lag(B.servRet.la, 1, 0) * (1 + penSim$cola_actual[j-1]), 
                                         B.servRet.la),
                 ALx.servRet.la = B.servRet.la * ax.servRet)
        }
      }
        
      #ls_servRet %>% filter(start_year == 2000, ea ==34, age_servRet == 53)
      
      # Calculate total benefit and AL for service retirees
      
      penSim$AL.servRet[j] <- 
        ls_servRet %>% 
        map(~ (filter(.x, year == init_year + j - 1) %>% summarise(AL.servRet.la = sum(ALx.servRet.la * n_servRet.la)))$AL.servRet.la) %>% 
        unlist %>% 
        sum()
      
      penSim$B.servRet[j] <- 
      ls_servRet %>% 
        map(~ (filter(.x, year == init_year + j - 1) %>% summarise(B.servRet.la  = sum(B.servRet.la * n_servRet.la)))$B.servRet.la) %>% 
        unlist %>% 
        sum()
      
      
      # penSim$AL.servRet[j] <- (filter(ls_servRet, year == init_year + j - 1) %>% summarise(AL.servRet.la = sum(ALx.servRet.la * n_servRet.la)))$AL.servRet.la
      # penSim$B.servRet[j]  <- (filter(ls_servRet, year == init_year + j - 1) %>% summarise(B.servRet.la  = sum(B.servRet.la * n_servRet.la)))$B.servRet.la
     
        
      # Total liability and benefit: actives and retirees
      
      penSim$AL.nonactive[j] <- with(penSim, AL.servRet[j] + 
                                             AL.defrRet[j] + 
                                             AL.disbRet[j] + 
                                             AL.death[j])
      
      penSim$AL[j] <- with(penSim, AL.active[j] + AL.nonactive[j])
      
      penSim$B[j]  <- with(penSim, B.servRet[j] + B.defrRet[j] + B.disbRet[j] + B.death[j]) 
    
    }
    
    # Funded ratios
    penSim$FR_MA[j] <- with(penSim, MA[j] / AL[j])
    penSim$FR_AA[j] <- with(penSim, AA[j] / AL[j])
    
    # if(!is.na(use_lowerDR)){
    #   penSim$FR_MA_lowerDR[j] <- with(penSim, MA[j] / AL[j] * cola_lowerDR_fixedALratio )
    #   
    # }
      
    # UAAL(j)
    penSim$UAAL[j]    <- with(penSim, AL[j] - AA[j])
  
      
    #**********************************************
    #       Determining (contingent) COLA         # 
    #**********************************************
      
      # funded ratio based COLA
      if(useContingentCOLA){
        
        if(!is.na(use_lowerDR)){
          
          penSim$FR_MA_lowerDR[j] <- with(penSim, MA[j] / (AL[j] * cola_lowerDR_fixedALratio))
          if(penSim$FR_MA_lowerDR[j] >= 0.995) penSim$cola_actual[j] <- cola_max_FR else penSim$cola_actual[j] <- cola_min_FR # use 99.99 to avoid rounding issue
          
        } else {
          
          if(penSim$FR_MA[j] >= 0.995) penSim$cola_actual[j] <- cola_max_FR else penSim$cola_actual[j] <- cola_min_FR # use 99.99 to avoid rounding issue
          
        }
        
       
      }
      
           
      #******************************************
      #   2. Losses/gains and amoritization    **
      #******************************************
      
      # Notes on useAVamort
      # if useAVamort is FALSE: It is assumed that the entire UAAL in year 1 is the loss/gain that occurs in year 0
      #                         It is then amortized using the amortization policy. 
      #
      # if useAVamort is TRUE:  Use the schedule of losses/gains and amortization payments from the plan documeht. 
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim$EUAAL[j]       <- 0
        penSim$LG[j]          <- with(penSim, UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim$Amort_basis[j] <- with(penSim, LG[j])    
        
      } else {
        penSim$EUAAL[j]       <- with(penSim, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        penSim$LG[j]          <- with(penSim,  UAAL[j] - EUAAL[j])
        penSim$Amort_basis[j] <- with(penSim,  LG[j]   - (C_ADC[j - 1]) * (1 + i[j - 1]))
      }   
      
      
      # Amortize LG(j)
      if(j > ifelse(useAVamort, 1, 0)){
        # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model.
        if(amort_type == "closed") SC_amort[nrow.initAmort + j - 1, j:(j + m - 1)] <- amort_LG(penSim$Amort_basis[j], i, m, salgrowth_amort, end = FALSE, method = amort_method, skipY1 = FALSE)
        }
      
      # Supplemental cost in j
      penSim$SC[j] <- switch(amort_type,
                             closed = sum(SC_amort[, j]),
                             open   = amort_LG(penSim$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method, skipY1 = FALSE)[1])
      
      
     
      #************************************************************************
      #   3a. ADC, ERC, and total contribution with individual cost method **
      #************************************************************************
      
      
      # Notes on nonNegC and EEC_fixed
     
      # EEC(j)
      # penSim$EEC[j] <- with(penSim, PR[j] * EEC_rate)
      
      # ADC(j)
      
      if(nonNegC){
      	penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
      	penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
      	
      	# Adjustment of EEC
      	if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
      	
      } else {
      	# Allow for negative ADC and C  
      	penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
      	
      	if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
      	# EEC is not fixed
      	# 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
      	} else if(with(penSim, ADC[j] > EEC[j])) {
      		penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
      		# 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
      	} else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
      		penSim$ADC.ER[j] <- 0
      		penSim$EEC[j]    <- with(penSim, ADC[j])
      		# 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
      	} else if(with(penSim, ADC[j] <= 0)) {
      		penSim$ADC.ER[j] <- with(penSim, ADC[j])
      		penSim$EEC[j]    <- 0
      	}
      }

      
      # ERC
      penSim$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
      
     
      
    
      #************************************************************************
      #   3c. Choose which cost method to apply   **
      #************************************************************************
      
      #if(cost_method == "EAN"){
      # C(j)
      penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
      
      # C(j) - ADC(j)
      penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
      # }
  
      
      #******************************************
      #   4. Investment income                 **
      #******************************************
      
      # Ia(j), Ib(j), Ic(j) Components of expected investment income
      penSim$Ia[j] <- with(penSim,  MA[j] * i[j])
      penSim$Ib[j] <- with(penSim,  B[j] * i[j])
      penSim$Ic[j] <- with(penSim,  C[j] * i[j])
      
      # I.e(j) Expected investment income
      penSim$I.e[j] <- with(penSim, i[j] * (MA[j] + C[j] - B[j] ))
      
      # I.r(j) Actual investment income
      penSim$I.r[j] <- with(penSim, i.r[j] *( MA[j] + C[j] - B[j]))   # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
  
      # I.dif(j) = I.r(j) - I.e(j): Difference between expected and actual investment incomes (for asset smoothing)
      penSim$I.dif[j] <- with(penSim, I.r[j] - I.e[j])
      
      }
    
    as.data.frame(penSim)
  }
  
  stopCluster(cl)
  

  
  #*****************************************************************************
  #                    Combining results into a data frame.   ####
  #*****************************************************************************
  
  
  penSim_results <- bind_rows(penSim_results) %>% 
    mutate(
    	     ## Standard output variables
    	     sim      = rep(-2:nsim, each = nyear),
           sim_name = sim_paramlist_$sim_name,
    	     val_name = sim_paramlist_$val_name,
           # tier_Mode = tier_Mode,
    			 # singleTier_select = singleTier_select,
    			 # return_scenario = return_scenario, 
           
    			 FR_AA   = 100 * AA / exp(log(AL)),
           FR_MA   = 100 * MA / exp(log(AL)),
           
    			 NC_PR   = 100 * NC / PR,
    			 SC_PR   = 100 * SC / PR, 
           
    			 ERC_PR  = 100 * ERC     / PR,
    			 EEC_PR  = 100 * EEC / PR,
           C_PR    = 100 * C / PR,
           B_PR    = 100 * B / PR,
    			 
           ExF     = C - B,
           ExF_PR  = 100 * ExF / PR,
           ExF_MA  = 100 * ExF / MA, 
           PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA)
    			 
    			 ## Plan specific variables
    			 # 
    			 
    			 ## Optional ratios
    			 # UAAL_PR = 100 * UAAL / PR,
    			 # MA_PR   = 100 * MA / PR,
    			 # AA_PR   = 100 * AA / PR,
    			 # AL_PR   = 100 * AL / PR,
    			 # AL.act_PR   = 100 * AL.act / PR,
    			 # AL.la_PR    = 100 * AL.la / PR, 
    			 # # AL.ca_PR    = 100 * AL.ca / PR, 
    			 # AL.term_PR   = 100 * AL.term / PR, 
    			 # #AL._PR    = 100 * AL.Ben / PR,
    			 # ADC_PR  = 100 * ADC / PR,
    			 # NC.laca_PR    = 100 * NC.laca / PR,
    			 # NC.v_PR   = 100 * NC.v / PR
           ) %>%
    relocate(sim_name, val_name, sim, year)
  
  return(penSim_results)
}



# penSim_results %>% 
#   filter(sim == 0) %>% 
#   select(year, NC_PR, ERC_PR, PVFB.tot, PVFS, PVFEEC, AA) %>% 
#   mutate(
#          NCrate_AGG  = 100* (PVFB.tot - AA)/PVFS,
#          ERCrate_AGG = 100* (PVFB.tot - PVFEEC - AA)/PVFS                        )
#   



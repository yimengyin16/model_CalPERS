# This script calculate aggregate annual ALs, NCs and benefit payments.


get_AggLiab <- function(pop_,
                        indivLiab_, 
                        val_paramlist_    =  val_paramlist,
                        Global_paramlist_ =  Global_paramlist
){

  # This function calculates total AL, NC and benefits.

    
  # Run the section below when developing new features.  
   pop_ = pop
   indivLiab_ = indivLiab
   val_paramlist_    =  val_paramlist
   Global_paramlist_ =  Global_paramlist
   
   
   assign_parmsList(Global_paramlist_, envir = environment())
   assign_parmsList(paramlist_,        envir = environment())
   
   
 

   
## Notes on naming conventions:
   # "cellsum" means total AL/NA/... in each year x age x ea cell
   # "yearsum" means sum of AL/NA/... across age x ea in each year. 
   # "actAll"  menas sum of variables related to active members, including life annuity, contingent annuity, and term benefits for actives. (now plus death benefit and disability benefit)
   
#*******************************************************************************
#               ## Liabilities and NCs for actives   ####
#*******************************************************************************

  # Join population data frames and liability data frames. 
  liab_$active         <- left_join(pop_$active, liab_$active, by = c("year", "ea", "age")) 
  liab_$active[-(1:3)] <- colwise(na2zero)(liab_$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 
  
  liab_$active %<>%  
    mutate(ALx.laca.cellsum     = ALx.laca * number.a,
           ALx.v.cellsum        = ALx.v    * number.a,
    			 ALx.disbRet.cellsum  = ALx.disbRet * number.a,
           ALx.death.cellsum    = ALx.death * number.a,
           ALx.actAll.cellsum   = ALx.laca.cellsum + ALx.v.cellsum + ALx.disbRet.cellsum + ALx.death.cellsum, 
           
           NCx.laca.cellsum     = NCx.laca * number.a,
           NCx.v.cellsum        = NCx.v    * number.a,
           NCx.disbRet.cellsum  = NCx.disbRet * number.a,
    			 NCx.death.cellsum    = NCx.death * number.a,
           NCx.actAll.cellsum   = NCx.laca.cellsum + NCx.v.cellsum + NCx.disbRet.cellsum + NCx.death.cellsum,
           
           PVFBx.laca.cellsum   = PVFBx.laca * number.a,
           PVFBx.v.cellsum      = PVFBx.v    * number.a,
    			 PVFBx.disbRet.cellsum= PVFBx.disbRet * number.a,
           PVFBx.death.cellsum  = PVFBx.death * number.a,
           PVFBx.actAll.cellsum = PVFBx.laca.cellsum + PVFBx.v.cellsum + PVFBx.disbRet.cellsum + PVFBx.death.cellsum,
    			 
    			 PVFNC.laca.cellsum   = PVFNC.laca * number.a,
    			 PVFNC.v.cellsum      = PVFNC.v    * number.a,
    			 PVFNC.disbRet.cellsum= PVFNC.disbRet * number.a,
    			 PVFNC.death.cellsum  = PVFNC.death * number.a,
    			 PVFNC.actAll.cellsum = PVFNC.laca.cellsum + PVFNC.v.cellsum + PVFNC.disbRet.cellsum + PVFNC.death.cellsum,
           
           PR.cellsum     = sx * number.a,
           EEC.cellsum    = EEC * number.a,
    			 PVFEEC.cellsum = PVFEEC * number.a,
    			 PVFSx.cellsum  = PVFSx * number.a,
    			 
           runname = runname)
  
  active.agg <- liab_$active %>%  
    group_by(year) %>% 
    summarise(
      ALx.laca.yearsum    = sum(ALx.laca.cellsum,  na.rm = TRUE),
      ALx.v.yearsum       = sum(ALx.v.cellsum,     na.rm = TRUE),
      ALx.disbRet.yearsum = sum(ALx.disbRet.cellsum, na.rm = TRUE),
      ALx.death.yearsum   = sum(ALx.death.cellsum, na.rm = TRUE),
      ALx.actAll.yearsum  = sum(ALx.actAll.cellsum,  na.rm = TRUE), 
      
      NCx.laca.yearsum    = sum(NCx.laca.cellsum, na.rm = TRUE),
      NCx.v.yearsum       = sum(NCx.v.cellsum,    na.rm = TRUE),
      NCx.disbRet.yearsum = sum(NCx.disbRet.cellsum, na.rm = TRUE),
      NCx.death.yearsum   = sum(NCx.death.cellsum,na.rm = TRUE),
      NCx.actAll.yearsum  = sum(NCx.actAll.cellsum,   na.rm = TRUE),
      
      PVFBx.laca.yearsum    = sum(PVFBx.laca.cellsum, na.rm = TRUE),
      PVFBx.v.yearsum       = sum(PVFBx.v.cellsum,    na.rm = TRUE),
      PVFBx.disbRet.yearsum = sum(PVFBx.disbRet.cellsum, na.rm = TRUE),
      PVFBx.death.yearsum   = sum(PVFBx.death.cellsum,na.rm = TRUE),
      PVFBx.actAll.yearsum  = sum(PVFBx.actAll.cellsum,   na.rm = TRUE),
      
      PVFNC.laca.yearsum    = sum(PVFNC.laca.cellsum, na.rm = TRUE),
      PVFNC.v.yearsum       = sum(PVFNC.v.cellsum,    na.rm = TRUE),
      PVFNC.disbRet.yearsum = sum(PVFNC.disbRet.cellsum, na.rm = TRUE),
      PVFNC.death.yearsum   = sum(PVFNC.death.cellsum,na.rm = TRUE),
      PVFNC.actAll.yearsum  = sum(PVFNC.actAll.cellsum,   na.rm = TRUE),
      
      PR.yearsum     = sum(PR.cellsum,  na.rm = TRUE),
      EEC.yearsum    = sum(EEC.cellsum, na.rm = TRUE),
      PVFEEC.yearsum = sum(PVFEEC.cellsum, na.rm = TRUE),
      PVFSx.yearsum  = sum(PVFSx.cellsum, na.rm = TRUE),
      
      nactives  = sum(number.a,  na.rm = TRUE)) %>% 
      as.matrix # extracting elements from matrices is much faster than from data.frame
  
  # active.agg %>% as.data.frame()


#*******************************************************************************
#         ## Liabilities and benefits for retirees (life annuitants)        ####
#*******************************************************************************
  
  liab_$la  <- data.table(liab_$la, key = "ea,age,year,year_servRet")
  pop_$la   <- data.table(pop_$la,  key = "ea,age,year,year_servRet")
  liab_$la  <- merge(pop_$la, liab_$la, by = c("ea", "age","year", "year_servRet"), all.x = TRUE)
  liab_$la  <- as.data.frame(liab_$la)
  
  liab_$la %<>% 
    mutate(ALx.la.cellsum  = ALx.la * number.la,
           B.la.cellsum    = B.la   * number.la,
           runname         = runname)
  
  la.agg <- liab_$la %>% 
    group_by(year) %>% 
    summarise(ALx.la.yearsum = sum(ALx.la.cellsum, na.rm = TRUE),
              B.la.yearsum   = sum(B.la.cellsum  , na.rm = TRUE),
              nla            = sum(number.la , na.rm = TRUE)) %>% 
    as.matrix
  
  #*** Detective work for debugging
  # Check the future development of a single cell of the initial retirees 
  # x <- liab_$la %>% filter(number.la != 0, 
  # 												 year_servRet == 2015,
  # 												 start_year == 1966)
  
  # check initial retires in year 1
  # x <- liab_$la %>% filter(number.la != 0, 
  # 												 year_servRet == 2015,
  # 												 year == 2015)
  # la.agg %>% as.data.frame
  
  # 
  # liab_   = liab.tCD
  # la.agg %>% as.data.frame
  # 
  # x <- pop_$la %>% filter(year == 2015)
  # x$number.la %>% sum
  # 
  # y <- liab_$la %>% filter(year == 2015, year.r == 2015)
  # y
  # liab_$la %>% filter(year == 2015) %>% select(year, ea, age, year.r, ALx.la, B.la) %>% filter(year.r == 2015)
  # pop$la   %>% filter(year == 2015) %>% filter(number.la !=0)
  # 
  # liab_$la$
  # 
  # x <- pop_$la  %>% filter(year == 2015number.la)
  # x$number.la %>% sum
  #***

  
#*******************************************************************************
#               ## Liabilities and benefits for vested terms.   ####
#*******************************************************************************
  
  # Save 10 seconds by using data.table to merge. Update 2/2016: the latest version of left_join looks fast enough.
  
  liab_$term <- left_join(pop_$term, liab_$term, by = c("year", "year_term", "ea", "age"))
  
  
  liab_$term %<>%
  	mutate(ALx.v.cellsum = ALx.v * number.v,
  				 B.v.cellsum   = B.v   * number.v,
  				 runname = runname)
  
  
  term.agg <- liab_$term %>%
  	group_by(year) %>%
  	summarise(ALx.v.yearsum   = sum(ALx.v.cellsum, na.rm = TRUE),
  						B.v.yearsum     = sum(B.v.cellsum  , na.rm = TRUE),
  						nterms          = sum(number.v , na.rm = TRUE)) %>%
  	# mutate(runname = runname) %>%
  	as.matrix
  
  # term.agg
  # liab_$term %>% filter(year.term == 2029, year == 2029, number.v !=0)
  # liab_$term %>% filter(start.year == 2018, year.term == 2028, ea == 25)
  
  
  
#*******************************************************************************
#    ## Liabilities and benefits for disability benefit (life annuitants)   ####
#*******************************************************************************
  
  liab_$disbRet  <- data.table(liab_$disbRet, key = "ea,age,year,year_disbRet")
  pop_$disbRet   <- data.table(pop_$disbRet,  key = "ea,age,year,year_disbRet")
  liab_$disbRet  <- merge(pop_$disbRet, liab_$disbRet, by = c("ea", "age","year", "year_disbRet"), all.x = TRUE)
  liab_$disbRet  <- as.data.frame(liab_$disbRet)


  liab_$disbRet %<>%
    mutate(ALx.disbRet.cellsum = ALx.disbRet * number.disbRet,
           B.disbRet.cellsum   = B.disbRet   * number.disbRet,
           runname = runname)

  disbRet.agg <- liab_$disbRet %>%
    group_by(year) %>%
    summarise(ALx.disbRet.yearsum   = sum(ALx.disbRet.cellsum, na.rm = TRUE),
              B.disbRet.yearsum     = sum(B.disbRet.cellsum  , na.rm = TRUE),
              ndisbRet              = sum(number.disbRet , na.rm = TRUE)) %>%
    # mutate(runname = runname) %>%
    as.matrix
  
  # disbRet.agg %>% as.data.frame()
  
  # 
  # 
  # l1 <- liab_$disbRet.la %>% filter(year == 2016, year.disb == 2016, ea == 20) %>% 
  #   mutate(AL.next = ((ALx.disbRet.la.tot - B.disbRet.la.tot) * 1.0725),
  #          age = age + 1) %>% 
  #   select(ea, age, year.disb, AL.next)
  #   
  # l2 <- liab_$disbRet.la %>% filter(year == 2017, year.disb == 2016, ea == 20) %>% 
  #   select(ea, age, year.disb, ALx.disbRet.la.tot)
  # 
  # left_join(l1, l2) %>% 
  #   mutate(diff = 100*AL.next / ALx.disbRet.la.tot)
  # 
  # l1 %>% summarise(sum(AL.next, na.rm = TRUE))
  # l2 %>% summarise(sum(ALx.disbRet.la.tot, na.rm = TRUE))
  # 
  # liab_$disbRet.la %>% filter(year == 2016, year.disb == 2016, ea == 20) %>% summarise(AL = sum(ALx.disbRet.la.tot, na.rm = T),
  #                                                                                   B  = sum(B.disbRet.la.tot, na.rm = T))
  # liab_$disbRet.la %>% filter(year == 2017, year.disb == 2016, ea == 20) %>% summarise(AL = sum(ALx.disbRet.la.tot, na.rm = T))
  # 
  # decrement.model.tCD
  # 
  # liab_$disbRet.la %>% filter(year == 2016, year.disb == 2016, ea == 20)
  # liab_$disbRet.la %>% filter(year == 2017, year.disb == 2016, ea == 20)
  # 

#*******************************************************************************
#             ## Liabilities and benefits for death benefit   ####
#*******************************************************************************
  
  liab_$death   <- data.table(liab_$death,    key = "ea,age,year,year_death")
  pop_$deathBen <- data.table(pop_$deathBen,  key = "ea,age,year,year_death")
  liab_$death   <- merge(pop_$deathBen, liab_$death, by = c("ea", "age","year", "year_death"), all.x = TRUE)
  liab_$death   <- as.data.frame(liab_$death)
  
  liab_$death %<>%
  	mutate(ALx.death.cellsum = ALx.death * number.deathBen,
  				 B.death.cellsum   = B.death   * number.deathBen,
  				 runname = runname)
  
  death.agg <- liab_$death %>%
  	group_by(year) %>%
  	summarise(ALx.death.yearsum   = sum(ALx.death.cellsum, na.rm = TRUE),
  						B.death.yearsum     = sum(B.death.cellsum  , na.rm = TRUE),
  						ndeathBen       = sum(number.deathBen , na.rm = TRUE)) %>%
  	# mutate(runname = runname) %>%
  	as.matrix
  
  #death.agg %>% as.data.frame
  # 
  # liab_$death %>% filter(year.death == 2019, year %in% 2018:2019, number.deathBen !=0)
  # liab_$death %>% filter(year == 2019, number.deathBen !=0)
  # 
  
  

#*******************************************************************************
#     ## Liabilities and benefits for contingent annuitants and survivors of service retirees ####
#*******************************************************************************
  
  # Part 1:Initial beneficiaries
   # Assumptions about the retirement ages of initial beneficiaries:
   # age.r = 60, if age in 2015 is greater than or equal to 60.
   # age.r = age in 2015,  if age in 2015 is smaller than 60. 

  # init_beneficiaries_ %<>% mutate(init.age = age, 
  #                                 age.r = age, #ifelse(age >= 60, 60, age), 
  #                                 year = init.year, 
  #                                 age = NULL)
  # 
  # init.ca.agg <- expand.grid(init.age = unique(init_beneficiaries_$init.age), age = min(init_beneficiaries_$init.age):max.age) %>%
  #                filter(age >= init.age) %>% 
  #                left_join(init_beneficiaries_) %>% 
  #                group_by(init.age) %>% 
  #                arrange(init.age, age) %>% 
  #                mutate(age.r = age.r[age == init.age]) %>% # necessary?
  #                left_join(select(mortality.post.model_, age, age.r, ax.r.W.ben = ax.r.W, pxm.post.W)) %>% 
  #                mutate(year     =  init.year + age -  init.age,
  #                       n.R0S1   = nbeneficiaries * cumprod(ifelse(age == init.age, 1, lag(pxm.post.W))),
  #                       B.ca     = benefit * (1 + cola)^(age - init.age),
  #                       B.ca.sum = B.ca * n.R0S1,
  #                       liab.ca  = B.ca * ax.r.W.ben,
  #                       liab.ca.sum = liab.ca * n.R0S1
  #                       ) %>% 
  #                group_by(year) %>% 
  #                summarise(n.R0S1.init      = sum(n.R0S1, na.rm = TRUE),
  #                          B.ca.sum.init    = sum(B.ca.sum, na.rm = TRUE),
  #                          liab.ca.sum.init = sum(liab.ca.sum, na.rm = TRUE)) %>% 
  #                filter(year %in% init.year:(init.year + nyear - 1))
  #   
  # # init.ca.agg
  # 
  # 
  # # Part 2: Initial retirees who are assumed to be contingent annuitants
  #   # Assume they retiree at their current ages. (age.r = age)
  # 
  # init_retirees.ca_ %<>% 
  #   mutate(#init.age = age, 
  #   age.r = age, #ifelse(age >= 60, 60, age), 
  #   year = init.year) %>% 
  #   select(-age)
  # 
  # init.ret.ca.agg <- expand.grid(age.r = unique(init_retirees.ca_$age.r), age = min(init_retirees.ca_$age.r):max.age) %>%
  #   filter(age >= age.r) %>% 
  #   left_join(init_retirees.ca_) %>%
  #   left_join(liab.ca_) %>% 
  #   group_by(age.r) %>% 
  #   arrange(age.r, age) %>% 
  #   # mutate(age.r = age.r[age == init.age]) %>% # necessary?
  #   # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ben = ax.r.W, pxm.post.W)) %>% 
  #   mutate(year     =  init.year + age - age.r,
  #          liab.ret.ca.sum = nretirees.ca * benefit * liab.ca.sum.1,
  #          B.ret.ca.sum    = nretirees.ca * benefit * B.ca.sum.1,
  #          n.ret.ca.R1     = nretirees.ca * (n.R1S0.1 + n.R1S1.1),
  #          n.ret.ca.R0S1   = nretirees.ca * n.R0S1.1
  #   ) %>% 
  #   group_by(year) %>% 
  #   summarise(n.ret.ca.R1.init        = sum(n.ret.ca.R1, na.rm = TRUE),
  #             n.ret.ca.R0S1.init      = sum(n.ret.ca.R0S1, na.rm = TRUE),
  #             B.ret.ca.sum.init       = sum(B.ret.ca.sum, na.rm = TRUE),
  #             liab.ret.ca.sum.init    = sum(liab.ret.ca.sum, na.rm = TRUE)) %>% 
  #   filter(year %in% init.year:(init.year + nyear - 1))
  # 
  # 
  # # Part 3: contingent annuitants who retiree after the inital year. 
  # 
  # ca.agg <- expand.grid(year.r = init.year:(init.year + nyear - 1), age.r = range_age.r, ea = range_ea, age = range_age) %>% 
  #           mutate(year = year.r + age - age.r) %>% 
  #            filter(age >= ea,
  #                  age >= age.r,
  #                  age.r > ea,
  #                  year <= max(year.r)) %>%
  # 
  #           left_join(liab_$active %>% filter(age %in% range_age.r) %>% select(year.r = year, ea, age.r = age, Bx.laca, sx)) %>% 
  #           left_join(pop_$new_ca %>% select(year.r = year, ea, age.r = age, new_ca)) %>% 
  #           left_join(liab.ca_) %>% 
  #           mutate(new_ca = na2zero(new_ca),
  #                  liab.ca.sum = new_ca * Bx.laca * liab.ca.sum.1,
  #                  B.ca.sum    = new_ca * Bx.laca * B.ca.sum.1,
  #                  n.R1        = new_ca * (n.R1S0.1 + n.R1S1.1), # Total number of living contingent annuitants
  #                  n.R0S1      = new_ca * n.R0S1.1               # Total number of survivors
  #                  ) %>%          
  #                  #filter(year.r == 2025, age.r == 50, ea == 20) %>%            
  #           group_by(year) %>% 
  #           summarise(liab.ca.sum = sum(liab.ca.sum, na.rm = TRUE),
  #                     B.ca.sum    = sum(B.ca.sum, na.rm = TRUE),
  #                     n.R1        = sum(n.R1, na.rm = TRUE),
  #                     n.R0S1      = sum(n.R0S1, na.rm = TRUE),
  #                     n.new_ca    = sum(new_ca, na.rm = TRUE)
  #                     ) 
  # 
  # 
  # 
  #  # Combine the initial beneficiaries and new beneficiaries
  #  ca.agg %<>% left_join(init.ca.agg) %>%
  #              left_join(init.ret.ca.agg) %>% 
  #             colwise(na2zero)() %>% 
  #             mutate(n.R1     = n.R1 + n.ret.ca.R1.init,
  #                    n.R0S1   = n.R0S1 + n.R0S1.init + n.ret.ca.R0S1.init,
  #                    B.ca.sum = B.ca.sum + B.ca.sum.init + B.ret.ca.sum.init,
  #                    liab.ca.sum = liab.ca.sum + liab.ca.sum.init + liab.ret.ca.sum.init) %>% 
  #             as.matrix
  # 
  #  ca.agg
  #  
  #  
  #  #*******************************************************************************************************************************
  #  #      ## Liabilities and benefits for DISABILITY contingent annuitants and survivors    ####
  #  #*******************************************************************************************************************************  
  #  
  #  
  #  # Part 1: Initial disability contingent annuitants
  #  init_disb.ca_ %<>% 
  #    mutate(#init.age = age, 
  #      age.disb = age, #ifelse(age >= 60, 60, age), 
  #      year = init.year) %>% 
  #    select(-age)
  #  
  #  init.disb.ca.agg <- expand.grid(age.disb = unique(init_disb.ca_$age.disb), age = min.age:max.age) %>%
  #    filter(age >= age.disb) %>% 
  #    left_join(init_disb.ca_) %>%
  #    left_join(liab.disb.ca_) %>% 
  #    group_by(age.disb) %>% 
  #    arrange(age.disb, age) %>% 
  #    # mutate(age.r = age.r[age == init.age]) %>% # necessary?
  #    # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ben = ax.r.W, pxm.post.W)) %>% 
  #    mutate(year     =  init.year + age - age.disb,
  #           liab.disb.ca.sum = ndisb.ca * benefit * liab.ca.sum.1,
  #           B.disb.ca.sum    = ndisb.ca * benefit * B.ca.sum.1,
  #           n.disb.ca.R1     = ndisb.ca * (n.R1S0.1 + n.R1S1.1),
  #           n.disb.ca.R0S1   = ndisb.ca * n.R0S1.1
  #    ) %>% 
  #    group_by(year) %>% 
  #    summarise(n.disb.ca.R1.init        = sum(n.disb.ca.R1, na.rm = TRUE),
  #              n.disb.ca.R0S1.init      = sum(n.disb.ca.R0S1, na.rm = TRUE),
  #              B.disb.ca.sum.init       = sum(B.disb.ca.sum, na.rm = TRUE),
  #              liab.disb.sum.init   = sum(liab.disb.ca.sum, na.rm = TRUE)) %>% 
  #    filter(year %in% init.year:(init.year + nyear - 1))
  #  
  #  
  #  # Part 2: Disability contingent annuitants who become disabled after the initial year. 
  #  range_age.disb <- min(range_age):max(range_age.r)
  #  
  #  disb.ca.agg <- expand.grid(year.disb = init.year:(init.year + nyear - 1), age.disb = range_age.disb, ea = range_ea, age = range_age) %>% 
  #    mutate(year = year.disb + age - age.disb) %>% 
  #    filter(age >= ea,
  #           age >= age.disb,
  #           age.disb > ea,
  #           year <= max(year.disb)) %>%
  #    
  #    left_join(liab_$active %>% 
  #               filter(age %in% range_age.disb) %>% 
  #               select(year.disb = year, ea, age.disb = age, Bx.disb)) %>% 
  #    left_join(pop_$new_disb.ca %>% select(year.disb = year, ea, age.disb = age, new_disb.ca)) %>% 
  #    left_join(liab.disb.ca_) %>% 
  #    mutate(new_disb.ca = na2zero(new_disb.ca),
  #           liab.disb.ca.sum = new_disb.ca * Bx.disb * liab.ca.sum.1,
  #           B.disb.ca.sum    = new_disb.ca * Bx.disb * B.ca.sum.1,
  #           n.disb.R1        = new_disb.ca * (n.R1S0.1 + n.R1S1.1), # Total number of living contingent annuitants
  #           n.disb.R0S1      = new_disb.ca * n.R0S1.1) %>%          # Total number of survivors
  #    #filter(year.r == 2025, age.r == 50, ea == 20) %>%            
  #    group_by(year) %>% 
  #    summarise(ALx.disb.ca.sum  = sum(liab.disb.ca.sum, na.rm = TRUE),
  #              B.disb.ca.sum    = sum(B.disb.ca.sum, na.rm = TRUE),
  #              n.disb.R1        = sum(n.disb.R1, na.rm = TRUE),
  #              n.disb.R0S1      = sum(n.disb.R0S1, na.rm = TRUE),
  #              n.new_disb.ca    = sum(new_disb.ca, na.rm = TRUE)) 
  #  
  #  
  # 
  #  
  #  # Combine the initial beneficiaries and new beneficiaries
  #  disb.ca.agg %<>% 
  #    left_join(init.disb.ca.agg) %>%
  #    colwise(na2zero)() %>%
  #    mutate(n.disb.R1     = n.disb.R1 + n.disb.ca.R1.init,
  #           n.disb.R0S1   = n.disb.R0S1 + n.disb.ca.R0S1.init,
  #           ALx.disb.ca.sum = ALx.disb.ca.sum + liab.disb.sum.init,
  #           B.disb.ca.sum   = B.disb.ca.sum + B.disb.ca.sum.init) %>%
  #    as.matrix
   
  
  # disb.ca.agg %>% data.frame
  # active.agg %>% data.frame
  
  #return(
  AggLiab <- list(active = active.agg, 
                  la     = la.agg,
                  #ca     = ca.agg, 
                  term   = term.agg,
                  death  = death.agg,
                  disbRet = disbRet.agg
                  )
  
              # ind_active  = if(paramlist$save.indiv) .liab$active  else "Not saved", 
              # ind_retiree = if(paramlist$save.indiv) .liab$retiree else "Not saved",
              # ind_term    = if(paramlist$save.indiv) .liab$term    else "Not saved")
    
    #)
}




#*************************************************************************************************************
#                                     ## Summing up tier values to get total values of a plan   ####
#*************************************************************************************************************

get_AggLiab_sumTiers <- function(...){
 # This function create list of aggregate values of a plan from list of tiers. 
 # ... :  a series data list of tiers.   
  
 #  AggLiab.list <- list(AggLiab.t76, AggLiab.t13, AggLiab.tm13)
  
  AggLiab.list <- list(...)
  
  AggLiab.list %<>% lapply( function(List) lapply(List, as.data.frame)) 

  nTiers <- length(AggLiab.list)
  nTypes <- length(AggLiab.list[[1]])
  TypeNames <- names(AggLiab.list[[1]])
  
  AggLiab.list2 <- vector("list", nTypes)
  names(AggLiab.list2) <- TypeNames
  
  for (j in TypeNames){
    AggLiab.list2[[j]] <- lapply(AggLiab.list, function(df){df[[j]]}) 
  }
  
  sum_tiers <- function(df){ df %<>% group_by(year) %>% 
      summarise_each(funs(sum))
  } 
  
  AggLiab_sumTiers <- AggLiab.list2 %>% 
                      lapply(bind_rows) %>% 
                      lapply(sum_tiers) %>% 
                      lapply(as.matrix)
  
  return(AggLiab_sumTiers)
}
  
  
  
  






  
  

# start_time_prep_loop <-  proc.time()
# 
# AggLiab <- get_AggLiab()
# 
# end_time_prep_loop <-  proc.time()
# Time_prep_loop <- end_time_prep_loop - start_time_prep_loop



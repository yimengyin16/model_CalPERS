# Simulation of the demograhics for a single tier of NYCTRS

## Modifications on the original model
  # 1. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
  # 2. The mortality for retirees are now retirement age dependent. (PSERS)


get_Population <- function(init_pop_         = init_pop,
                           entrants_dist_    = entrants_dist,
                           decrement_model_  = decrement_model,
                           paramlist_        = paramlist,
                           Global_paramlist_ = Global_paramlist){

## Inputs
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - decrement.model:  Decrement table, from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 4) life annuitants
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   
   # init_pop_         = init_pop_allTiers
   # entrants_dist_    = entrants_dist
   # decrement_model_  = decrement_model_allTiers
   # paramlist_        = paramlist
   # Global_paramlist_ = Global_paramlist


 assign_parmsList(Global_paramlist_, envir = environment())
 assign_parmsList(paramlist_,        envir = environment())  


#*************************************************************************************************************
#                                     Creating arrays for each status ####
#*************************************************************************************************************

## In each 3D array, 
#  dimension 1(row) represents entry age, 
#  dimension 2(column) represents attained age,
#  dimension 3(depth) represents number of year, 
#  dimension 4, if applicable, represents the year of termination/retirement/disablity/death. 

# The array of actives has 3 dimensions: ea x age x year 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, 
										range_age, 
										init_year:(init_year + nyear - 1))

# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.la <- list(range_ea, 
											 range_age, 
											 init_year:(init_year + nyear - 1), 
											 init_year:(init_year + nyear - 1))

# The array of terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.term      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.term <- list(range_ea, 
												 range_age, 
												 init_year:(init_year + nyear - 1), 
												 init_year:(init_year + nyear - 1))

# The array of disability retirees has 4 dimensions: ea x age x year x year of disability
wf_dim.disb      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disb <- list(range_ea, 
												 range_age, 
												 init_year:(init_year + nyear - 1), 
												 init_year:(init_year + nyear - 1))

# The array of death beneficiaries has 4 dimensions: ea x age x year x year of death (of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, 
														 range_age, 
														 init_year:(init_year + nyear - 1), 
														 init_year:(init_year + nyear - 1))



wf_active   <- array(0, wf_dim, dimnames = wf_dimnames)
wf_dead     <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term     <- array(0, wf_dim.term, dimnames = wf_dimnames.term)
wf_la       <- array(0, wf_dim.la, dimnames = wf_dimnames.la)
wf_deathBen <- array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
wf_disbRet  <- array(0, wf_dim.disb,     dimnames = wf_dimnames.disb)


newDeath.act  <- numeric(nyear)
newDeath.ret  <- numeric(nyear)
newDeath.term <- numeric(nyear)

newDisb.act <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting initial population  ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at age 54 and retireed in year 1. 
# Altough this may produce yos greater than r.max - ea.min, it is irrelevant to the calculation since we do not care about initial retirees' yos.  
# 
wf_active[, , 1]      <- init_pop_$actives 
wf_la[, , 1, 1]       <- init_pop_$servRet
wf_term[, , 1, 1]     <- 0 # init_pop_$terms   # note that the initial terms are assigned to year.term = init_year - 1
wf_disbRet[, , 1, 1]  <- init_pop_$disbRet



#*************************************************************************************************************
#                                     Defining population dynamics  ####
#*************************************************************************************************************

## Transition matrices ####
#   Assume the actual decrement rates are the same as the rates in decrement tables.
#   Later we may allow the actual decrement rates to differ from the assumed rates. 

decrement_wf <- decrement_model_ %>% mutate_all(funs(na2zero)) # just for safety 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select(age, ea, !!qx) %>% ungroup %>% spread(ea, !!qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}

# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 

# Where do the active go
p_active2term    <- make_dmat("qxt", df = filter(decrement_wf, start_year == min(start_year)))
p_active2disbRet <- make_dmat("qxd", df = filter(decrement_wf, start_year == min(start_year)))
p_active2dead    <- make_dmat("qxm_active", df = filter(decrement_wf, start_year == min(start_year)))
p_active2servRet <- make_dmat("qxr", df = filter(decrement_wf, start_year == min(start_year)))
p_active2la      <- make_dmat("qxr", df = filter(decrement_wf, start_year == min(start_year)))
p_active2deathBen<- make_dmat("qxm_active", df = filter(decrement_wf, start_year == min(start_year))) # * pct.QSS


# Where do the terminated go
# p_term2dead    <- make_dmat("qxm_terms", df = filter(decrement_wf, start_year == min(start_year))) 


p_term2dead <- expand.grid(ea        = range_ea, 
													 age       = range_age, 
													 year      = init_year:(init_year + nyear - 1), 
													 year_term = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_term = age - (year - year_term)) %>% 
	left_join(decrement_wf %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_term) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_term = na2zero(qxm_term)) %>% 
	#left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_term, age, ea)





# Where do the disabled go
#p_disbRet2dead    <- make_dmat("qxm_disbRet", df = filter(decrement_wf, start_year == min(start_year)))

p_disbRet2dead <- expand.grid(ea           = range_ea, 
												      age          = range_age, 
												      year         = init_year:(init_year + nyear - 1), 
												      year_disbRet = init_year:(init_year + nyear - 1)) %>%
	# filter(age >= ea) %>% 
	mutate(age_disbRet = age - (year - year_disbRet)) %>% 
	left_join(decrement_wf %>% 
							mutate(year = start_year + age - ea) %>% 
							select(year, ea, age, qxm_disbRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_disbRet = na2zero(qxm_disbRet)) %>% 
	# left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
	# mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
	arrange(year, year_disbRet, age, ea) 






# Where do the death beneficiaries go
p_deathBen2dead <- make_dmat("qxm_servRet", df = filter(decrement_wf, start_year == min(start_year))) # Simplified: weighted average of male and female mortality




# Where do the retirees go 
# Before we find better approach, the age_servRet(retriement age) dependent mortality for retirees are given in a data frame containing all combos 
# of year, year_servRet (year of retirement), ea, and age that exist in wf_la. 

# N/A in TRS model V1 

p_la2dead <- expand.grid(ea           = range_ea, 
                         age          = range_age, 
                         year         = init_year:(init_year + nyear - 1), 
                         year_servRet = init_year:(init_year + nyear - 1)) %>%
  # filter(age >= ea) %>% 
  mutate(age_servRet = age - (year - year_servRet)) %>% 
  left_join(decrement_wf %>% 
  					  mutate(year = start_year + age - ea) %>% 
  					  select(year, ea, age, qxm_servRet) , by = c("year", "ea", "age")) %>% 
	mutate(qxm_servRet = na2zero(qxm_servRet)) %>% 
  #left_join(mortality.post.model %>% select(age.r, age, qxm.post.W)) %>%
  # mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  arrange(year, year_servRet, age, ea)


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 


#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************


# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(max_retAge - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0, entrants_dist), na.rm = T)



#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************

# Now the next slice of the array (array[, , i + 1]) is defined: 
 # wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
 # i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
  # j <- 1
  # compute the inflow to and outflow
  active2term    <- wf_active[, , j] * p_active2term     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  active2servRet <- wf_active[, , j] * p_active2servRet  # This will be used to calculate the number of actives leaving the workforce
  active2la      <- wf_active[, , j] * p_active2la
  active2disbRet <- wf_active[, , j] * p_active2disbRet
  active2dead    <- wf_active[, , j] * p_active2dead
  active2deathBen<- wf_active[, , j] * p_active2deathBen 
  
  # Where do the terminated_vested go
  # term2dead  <- wf_term[, , j, ] * as.vector(p_term2dead)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  term2dead  <- wf_term[, , j, ] * (p_term2dead %>% filter(year == j + init_year - 1))[["qxm_term"]] 
  
  # Where do the retired go
  la2dead   <- wf_la[, , j, ] * (p_la2dead %>% filter(year == j + init_year - 1))[["qxm_servRet"]]  #[["qxm.post.W"]]  # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
  # Where do the disabled la go
  # disbRet2dead      <- wf_disbRet[, , j, ] * as.vector(p_disbRet2dead)
  disbRet2dead  <- wf_disbRet[, , j, ] * (p_disbRet2dead %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
  
  # Where do the death beneficiaries go
  deathBen2dead  <- wf_deathBen[, , j, ] * as.vector(p_deathBen2dead)
  
  
  # Total inflow and outflow for each status
  out_active   <- active2term + active2disbRet + active2servRet + active2dead 
  new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, dist = entrants_dist_, no.entrants = no_entrants) # new entrants
  
  out_term <- term2dead    # This is a 3D array 
  in_term  <- active2term  # This is a matrix
  
  out_disbRet <- disbRet2dead
  in_disbRet  <- active2disbRet
  
  out_la <- la2dead        # This is a 3D array (ea x age x year.retire)
  in_la  <- active2la     # This is a matrix
  
  out_deathBen <- deathBen2dead        # This is a 3D array (ea x age x year.retire)
  in_deathBen  <- active2deathBen     # This is a matrix
  
  in_dead <- active2dead +                                             
             apply(term2dead,    c(1,2), sum) +   # 
             apply(la2dead,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
             apply(disbRet2dead, c(1,2), sum) 
  
  
  
  # Calculate workforce for next year. 
  wf_active[, , j + 1]  <- (wf_active[, , j] - out_active) %*% A + new_entrants
  
  wf_term[, , j + 1, ]      <- apply((wf_term[, , j, ] - out_term), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term[, , j + 1, j + 1] <- in_term %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  
  wf_la[, ,j + 1, ]       <- apply((wf_la[, , j, ] - out_la), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la[, , j + 1, j + 1] <- in_la %*% A
  
  wf_disbRet[, , j + 1, ]      <- apply((wf_disbRet[, , j, ] - out_disbRet), 3, function(x) x %*% A) %>% array(wf_dim.disb[-3])
  wf_disbRet[, , j + 1, j + 1] <- in_disbRet %*% A
  
  
  wf_dead[, ,   j + 1]    <- (wf_dead[, , j] + in_dead) %*% A
  
  wf_deathBen[, , j + 1, ]      <- apply((wf_deathBen[, , j, ] - out_deathBen), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
  wf_deathBen[, , j + 1, j + 1] <- in_deathBen %*% A
  
 

  newDeath.act[j]  <- sum(active2dead)
  newDeath.ret[j]  <- sum(la2dead)
  newDisb.act[j]   <- sum(active2disbRet)
  
  #active2la
  #wf_active[,,j]
  
}



#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

wf_active <- adply(wf_active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, number.a, -ea, -year) %>% 
  mutate(year = f2n(year), age = as.numeric(age)) %>% 
  filter(age >= ea)


wf_la <- data.frame(expand.grid(ea           = range_ea, 
                                age          = range_age, 
                                year         = init_year:(init_year + nyear - 1), 
                                year_servRet = init_year:(init_year + nyear - 1)),
                    number.la = as.vector(wf_la)) %>% 
         filter(age >= ea)


wf_term <- data.frame(expand.grid(ea   = range_ea, 
                                  age  = range_age, 
                                  year = init_year:(init_year + nyear - 1), 
                                  year_term = (init_year):(init_year + nyear - 1)),
                      number.v = as.vector(wf_term)) %>% 
         filter(age >= ea)


wf_disbRet <- data.frame(expand.grid(ea   = range_ea, 
                                     age  = range_age, 
                                     year = init_year:(init_year + nyear - 1), 
                                     year_disbRet = (init_year):(init_year + nyear - 1)),
                          number.disbRet = as.vector(wf_disbRet)) %>% 
               filter(age >= ea)


wf_deathBen <- data.frame(expand.grid(ea = range_ea, 
																			age = range_age, 
																			year = init_year:(init_year + nyear - 1), 
																			year_death = (init_year):(init_year + nyear - 1)),
                          number.deathBen = as.vector(wf_deathBen)) %>%
  filter(age >= ea)


# wf_deathBen %>% filter(number.deathBen != 0) 

# # summarize term across termination year. Resulting data frame will join .Liab$active as part of the output. 
# term_reduced <- wf_term %>% group_by(year, age) %>% summarise(number.v = sum(number.v, na.rm = TRUE))



#*************************************************************************************************************
#                                     Number of new contingent annuitants   ####
#*************************************************************************************************************

# wf_new.ca <- wf_active %>% left_join(decrement_wf %>% select(age, ea, qxr.ca)) %>% 
#              mutate(new_ca  = number.a * qxr.ca,
#                     year = year + 1,
#                     age  = age + 1)
# 
# 
# wf_new.disb.ca <- wf_active %>% left_join(decrement_wf %>% select(age, ea, qxd.ca)) %>% 
#   mutate(new_disb.ca  = number.a * qxd.ca,
#          year = year + 1,
#          age  = age + 1)

# wf_new.disb.ca %>% group_by(year) %>% summarize(new_disb.ca = sum(new_disb.ca))
# wf_new.ca %>% group_by(year) %>% summarize(new_ca = sum(new_ca))

# Final outputs
pop <-  list(active  = wf_active, 
             term    = wf_term, 
             disbRet = wf_disbRet, 
             la = wf_la, 
             dead = wf_dead,
             deathBen = wf_deathBen
             #new_ca = wf_new.ca
)

return(pop)

}


# pop <- get_Population()






# pop$term %>% filter(year == 2016) %>% select(number.v) %>% sum



# # Spot check the results
# wf_active %>% group_by(year) %>% summarise(n = sum(number.a)) %>% mutate(x = n == 1000) %>% data.frame # OK
# wf_active %>% filter(year == 2025) %>% spread(age, number.a)
# 
# 
# wf_la %>% group_by(year) %>% summarise(n = sum(number.la)) %>% data.frame  
# 
# wf_la %>% filter(year.r == 2016, year == 2018, age==65) %>% mutate(number.la_next = number.la * 0.9945992) %>% 
#   left_join(wf_la %>% filter(year.r == 2016, year == 2019, age==66) %>% select(year.r, ea, number.la_true = number.la)) %>% 
#   mutate(diff = number.la_true - number.la_next) # looks ok.
# 
# mortality.post.ucrp %>% filter(age.r == 63)
# 
# 
# 
# 
# # check retirement
# wf_active %>% filter(year == 2020, ea == 30) %>% select(-year) %>% 
# left_join(wf_la     %>% filter(year == 2021, year.r == 2021, ea == 30)) %>% 
# left_join(wf_LSC.ca %>% filter(year == 2021, ea == 30) %>% select(year, ea, age, new_LSC, new_ca)) %>% 
# left_join(decrement_wf %>% filter(ea == 30) %>% select(ea, age, qxr, qxr.la, qxr.ca, qxr.LSC)) %>% 
# filter(age >= 49 & age <=75) %>% 
# mutate(diff.la = lag(number.a *qxr.la) - number.la,
#        diff.ca = lag(number.a *qxr.ca) - new_ca,
#        diff.LSC= lag(number.a *qxr.LSC) - new_LSC,
#        diff.r  = lag(number.a *qxr) - (new_ca + new_LSC + number.la))
#   # looks ok.
#





# wf_active %>% group_by(year) %>% summarise(n = sum(number.a))
# 
# wf_active %>% head
# 



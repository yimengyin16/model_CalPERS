# Simulation of the demograhics for a single tier of NYCTRS

## Modifications on the original model
  # 1. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
  # 2. The mortality for retirees are now retirement age dependent. (PSERS)


get_demographics <- function(tierData, 
                           Global_paramlist_ = Global_paramlist,
                           val_paramlist_    = val_paramlist){

## TODO: update
# Inputs 
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


# tierData          <- tierData_miscAll
# Global_paramlist_ <- Global_paramlist
# val_paramlist_    <- val_paramlist

assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(val_paramlist_, envir = environment())  



#*******************************************************************************
#     Creating a list that stores all demographic data                      ####
#*******************************************************************************

ls_demo <- list()



#*******************************************************************************
#                  Creating arrays for each status                          ####
#*******************************************************************************

## In each 3D array, 
#  dimension 1(row) represents entry age, 
#  dimension 2(column) represents attained age,
#  dimension 3(depth) represents number of year, 
#  dimension 4, if applicable, represents the year of termination/retirement/disability/death. 

# The array of actives has 3 dimensions: ea x age x year 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, 
										range_age, 
										init_year:(init_year + nyear - 1))

# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.servRet.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.servRet.la <- list(range_ea, 
										        	 range_age, 
											         init_year:(init_year + nyear - 1), 
											         init_year:(init_year + nyear - 1))


# The array of vested terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.defrRet.vest      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.defrRet.vest <- list(range_ea, 
									          		 range_age, 
												         init_year:(init_year + nyear - 1), 
												         init_year:(init_year + nyear - 1))


# The array of terminated with refund has 4 dimensions: ea x age x year x year of termination
wf_dim.defrRet.refund       <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.defrRet.refund  <- list(range_ea, 
                                    range_age, 
                                    init_year:(init_year + nyear - 1), 
                                    init_year:(init_year + nyear - 1))


# The array of disability retirees has 4 dimensions: ea x age x year x year of disability
wf_dim.disbRet      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disbRet <- list(range_ea, 
									    			range_age, 
												    init_year:(init_year + nyear - 1), 
												    init_year:(init_year + nyear - 1))

# The array of death beneficiaries has 4 dimensions: ea x age x year x year of death (of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, 
														 range_age, 
														 init_year:(init_year + nyear - 1), 
														 init_year:(init_year + nyear - 1))




ls_demo[[tierData$tier_name]] <- list(
  wf_active         = array(0, wf_dim, dimnames = wf_dimnames),
  wf_dead           = array(0, wf_dim, dimnames = wf_dimnames),
  
  wf_defrRet.vest   = array(0, wf_dim.defrRet.vest,   dimnames = wf_dimnames.defrRet.vest),
  wf_defrRet.refund = array(0, wf_dim.defrRet.refund, dimnames = wf_dimnames.defrRet.refund),
  
  wf_servRet.la     = array(0, wf_dim.servRet.la, dimnames = wf_dimnames.servRet.la),
  wf_disbRet        = array(0, wf_dim.disbRet,  dimnames = wf_dimnames.disbRet),
  wf_deathBen       = array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
)


# ls_demo


# wf_active   <- array(0, wf_dim, dimnames = wf_dimnames)
# wf_dead     <- array(0, wf_dim, dimnames = wf_dimnames)
# wf_term     <- array(0, wf_dim.term, dimnames = wf_dimnames.term)
# wf_la       <- array(0, wf_dim.la, dimnames = wf_dimnames.la)
# wf_deathBen <- array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
# wf_disbRet  <- array(0, wf_dim.disb,     dimnames = wf_dimnames.disb)


# newDeath.act  <- numeric(nyear)
# newDeath.ret  <- numeric(nyear)
# newDeath.term <- numeric(nyear)
# newDisb.act <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting initial population  ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at min_age and retired in year 1. 
# Altough this may produce yos greater than retAge_max - min_ea, it is irrelevant to the calculation since we do not care about initial retirees' yos.  


 ls_demo[[tierData$tier_name]]$wf_active[, , 1] <- 
  expand.grid(ea = range_ea, age = range_age) %>% 
  left_join(select(tierData$df_n_actives, ea, age, nactives), by = c("ea", "age")) %>% 
  spread(age, nactives, fill = 0) %>% 
  select(-ea) %>% 
  as.matrix


ls_demo[[tierData$tier_name]]$wf_servRet.la[, , 1, 1] <- 
  expand.grid(ea = range_ea, age = range_age) %>% 
  left_join(select(tierData$df_n_servRet, ea, age, n_servRet), by = c("ea", "age")) %>% 
  spread(age, n_servRet, fill = 0) %>% 
  select(-ea) %>% 
  as.matrix


ls_demo[[tierData$tier_name]]$wf_disbRet[, , 1, 1] <- 
  expand.grid(ea = range_ea, age = range_age) %>% 
  left_join(select(tierData$df_n_disbRet, ea, age, n_disbRet), by = c("ea", "age")) %>% 
  spread(age, n_disbRet, fill = 0) %>% 
  select(-ea) %>% 
  as.matrix
  

ls_demo[[tierData$tier_name]]$wf_defrRet.vest[, , 1, 1]   <- 0 # TODO that the initial terms are assigned to year.term = init_year - 1
ls_demo[[tierData$tier_name]]$wf_defrRet.refund[, , 1, 1] <- 0 # TODO that the initial terms are assigned to year.term = init_year - 1




#*************************************************************************************************************
#                                     Defining population dynamics  ####
#*************************************************************************************************************

## Transition matrices ####
#   Assume the actual decrement rates are the same as the rates in decrement tables.
#   Later we may allow the actual decrement rates to differ from the assumed rates. 

tierData$decrements_expanded



# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transition matrix
  df %<>% select(age, ea, !!qx) %>% ungroup %>% spread(ea, !!qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transition matrices. 

# Where do the actives go
ls_demo[[tierData$tier_name]]$p_active2defrRet.refund  <- make_dmat("qxt.refund", df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2defrRet.vest    <- make_dmat("qxt.vest",   df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2servRet.la      <- make_dmat("qxr",        df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2disbRet         <- make_dmat("qxd",        df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2dead            <- make_dmat("qxm.pre",    df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2deathBen        <- make_dmat("qxm.pre",    df = filter(tierData$decrements_expanded, year == init_year))



# Where do the terminated go
# p_term2dead    <- make_dmat("qxm_terms", df = filter(decrement_wf, start_year == min(start_year))) 

ls_demo[[tierData$tier_name]]$p_defrRet.vest2dead <- 
  expand_grid(ea        = range_ea, 
							age       = range_age, 
							year              = init_year:(init_year + nyear - 1), 
							year_defrRet.vest = init_year:(init_year + nyear - 1)) %>%
	mutate(age_defrRet.vest = age - (year - year_defrRet.vest)) %>% 
	left_join(tierData$decrements_expanded %>% 
						select(year, ea, age, qxm.post) , 
						by = c("year", "ea", "age")) %>% 
	mutate(qxm.defrRet.vest = na2zero(qxm.post)) %>% 
  select(year, year_defrRet.vest, age, ea, qxm.defrRet.vest) %>% 
	arrange(year, year_defrRet.vest, age, ea)  # Order must be maintained!


ls_demo[[tierData$tier_name]]$p_defrRet.refund2dead <- 
  expand_grid(ea        = range_ea, 
              age       = range_age, 
              year                = init_year:(init_year + nyear - 1), 
              year_defrRet.refund = init_year:(init_year + nyear - 1)) %>%
  mutate(age_defrRet.refund = age - (year - year_defrRet.refund)) %>% 
  left_join(tierData$decrements_expanded %>% 
              select(year, ea, age, qxm.post) , 
            by = c("year", "ea", "age")) %>% 
  mutate(qxm.defrRet.refund = na2zero(qxm.post)) %>% 
  select(year, year_defrRet.refund, age, ea, qxm.defrRet.refund) %>% 
  arrange(year, year_defrRet.refund, age, ea)  # Order must be maintained!


# Where do the disabled go
#p_disbRet2dead    <- make_dmat("qxm_disbRet", df = filter(decrement_wf, start_year == min(start_year)))

ls_demo[[tierData$tier_name]]$p_disbRet2dead <- 
  expand_grid(ea        = range_ea, 
              age       = range_age, 
              year         = init_year:(init_year + nyear - 1), 
              year_disbRet = init_year:(init_year + nyear - 1)) %>%
  mutate(age_disbRet = age - (year - year_disbRet)) %>% 
  left_join(tierData$decrements_expanded %>% 
              select(year, ea, age, qxmd.post) , 
            by = c("year", "ea", "age")) %>% 
  mutate(qxmd.post = na2zero(qxmd.post)) %>% 
  select(year,  year_disbRet, age, ea, qxmd.post) %>% 
  arrange(year, year_disbRet, age, ea)  # Order must be maintained!



# Where do the retirees go 
# Before we find better approach, the age_servRet(retriement age) dependent mortality for retirees are given in a data frame containing all combos 
# of year, year_servRet (year of retirement), ea, and age that exist in wf_la. 

ls_demo[[tierData$tier_name]]$p_servRet.la2dead <- 
  expand_grid(ea        = range_ea, 
              age       = range_age, 
              year         = init_year:(init_year + nyear - 1), 
              year_servRet.la = init_year:(init_year + nyear - 1)) %>%
  mutate(age_servRet.la = age - (year - year_servRet.la)) %>% 
  left_join(tierData$decrements_expanded %>% 
              select(year, ea, age, qxm.post) , 
            by = c("year", "ea", "age")) %>% 
  mutate(qxm.post = na2zero(qxm.post)) %>% 
  select(year,  year_servRet.la, age, ea, qxm.post) %>% 
  arrange(year, year_servRet.la, age, ea) # Order must be maintained!




# Where do the death beneficiaries go
ls_demo[[tierData$tier_name]]$p_deathBen2dead <- make_dmat("qxm.post", df = filter(tierData$decrements_expanded, year == init_year)) # Simplified: weighted average of male and female mortality



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
calc_entrants <- function(wf0, wf1, delta, dist, new_entrants = TRUE){
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
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0 * (1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1   # number of workers need to hire
  ne <- size_hire * dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0
  NE[, ] <- 0
  
  if (new_entrants){ 
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
    
  } else {
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

# How the next slice of the array (array[, , i + 1]) is defined: 
 # wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
 # i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
  # j <- 1
  # compute the inflow to and outflow
  # with(ls_demo[[tierData$tier_name]], wf_active[, , 1] * p_active2servRet.la)
  
  
  ### Store all changes in run j in a temporary list
  ls_demo[[tierData$tier_name]]$temp <- list(
    
    ## Where do the actives go
    active2servRet.la     = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2servRet.la),
    # active2servRet      = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2servRet),  # This will be used to calculate the number of actives leaving the workforce
    active2defrRet.vest   = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2defrRet.vest),     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2defrRet.refund = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2defrRet.refund), 
    active2disbRet        = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2disbRet),
    
    active2dead           = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2dead),
    active2deathBen       = with(ls_demo[[tierData$tier_name]], wf_active[, , j] * p_active2deathBen), 
    
    
    ## Where do the terminated go
    # term2dead  <- wf_term[, , j, ] * (p_term2dead %>% filter(year == j + init_year - 1))[["qxm_term"]] 
    defrRet.vest2dead    = with(ls_demo[[tierData$tier_name]], wf_defrRet.vest[, , j, ]   * (p_defrRet.vest2dead   %>% filter(year == j + init_year - 1))[["qxm.defrRet.vest"]]), 
    defrRet.refund2dead  = with(ls_demo[[tierData$tier_name]], wf_defrRet.refund[, , j, ] * (p_defrRet.refund2dead %>% filter(year == j + init_year - 1))[["qxm.defrRet.refund"]]), 
    
    ## Where do the retired go
    servRet.la2dead    = with(ls_demo[[tierData$tier_name]], wf_servRet.la[, , j, ]   * (p_servRet.la2dead   %>% filter(year == j + init_year - 1))[["qxm.post"]]), 
    
    
    ## Where do the disabled la go
    # disbRet2dead  <- wf_disbRet[, , j, ] * (p_disbRet2dead %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
    disbRet2dead    = with(ls_demo[[tierData$tier_name]], wf_disbRet[, , j, ]   * (p_disbRet2dead   %>% filter(year == j + init_year - 1))[["qxmd.post"]]), 
    
    
    ## Where do the death beneficiaries go
    deathBen2dead  = with(ls_demo[[tierData$tier_name]],  wf_deathBen[, , j, ] * as.vector(p_deathBen2dead))
  )
  
  
  
  # Total inflow and outflow for non-active members
  
  ls_demo[[tierData$tier_name]]$temp$out_defrRet.vest <- ls_demo[[tierData$tier_name]]$temp$defrRet.vest2dead    # This is a 3D array 
  ls_demo[[tierData$tier_name]]$temp$in_defrRet.vest  <- ls_demo[[tierData$tier_name]]$temp$active2defrRet.vest  # This is a matrix
  
  ls_demo[[tierData$tier_name]]$temp$out_defrRet.refund <- ls_demo[[tierData$tier_name]]$temp$defrRet.refund2dead    # This is a 3D array 
  ls_demo[[tierData$tier_name]]$temp$in_defrRet.refund  <- ls_demo[[tierData$tier_name]]$temp$active2defrRet.refund  # This is a matrix
  
  
  ls_demo[[tierData$tier_name]]$temp$out_servRet.la <- ls_demo[[tierData$tier_name]]$temp$servRet.la2dead       # This is a 3D array (ea x age x year.retire)
  ls_demo[[tierData$tier_name]]$temp$in_servRet.la  <- ls_demo[[tierData$tier_name]]$temp$active2servRet.la     # This is a matrix
  
  ls_demo[[tierData$tier_name]]$temp$out_disbRet <- ls_demo[[tierData$tier_name]]$temp$disbRet2dead    # This is a 3D array 
  ls_demo[[tierData$tier_name]]$temp$in_disbRet  <- ls_demo[[tierData$tier_name]]$temp$active2disbRet  # This is a matrix
  

  ls_demo[[tierData$tier_name]]$temp$out_deathBen <- ls_demo[[tierData$tier_name]]$temp$deathBen2dead        # This is a 3D array (ea x age x year.retire)
  ls_demo[[tierData$tier_name]]$temp$in_deathBen  <- ls_demo[[tierData$tier_name]]$temp$active2deathBen     # This is a matrix
  
  
  ls_demo[[tierData$tier_name]]$temp$in_dead <- 
    ls_demo[[tierData$tier_name]]$temp$active2dead +                                             
    apply(ls_demo[[tierData$tier_name]]$temp$defrRet.vest2dead,    c(1,2), sum) +   # 
    apply(ls_demo[[tierData$tier_name]]$temp$defrRet.refund2dead,  c(1,2), sum) +   # 
    apply(ls_demo[[tierData$tier_name]]$temp$servRet.la2dead,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
    apply(ls_demo[[tierData$tier_name]]$temp$disbRet2dead, c(1,2), sum) 
  
  
  # Total inflow and outflow for active members
  ls_demo[[tierData$tier_name]]$temp$out_active  <- with(ls_demo[[tierData$tier_name]]$temp, active2defrRet.vest + active2defrRet.refund + active2disbRet + active2servRet.la + active2dead) 
  ls_demo[[tierData$tier_name]]$temp$new_entrants <- 
    calc_entrants(ls_demo[[tierData$tier_name]]$wf_active[, , j], 
                  ls_demo[[tierData$tier_name]]$wf_active[, , j] - ls_demo[[tierData$tier_name]]$temp$out_active, 
                  val_paramlist_$wf_growth, 
                  dist = tierData$entrants_dist, 
                  new_entrants = val_paramlist$new_entrants) # new entrants

  
  ## Workforce for the next year. 
  
  # active members  
  ls_demo[[tierData$tier_name]]$wf_active[, , j + 1] <- 
    ((ls_demo[[tierData$tier_name]]$wf_active[, , j] - ls_demo[[tierData$tier_name]]$temp$out_active) %*% A + 
      ls_demo[[tierData$tier_name]]$temp$new_entrants) 
  
  # service retirees
  ls_demo[[tierData$tier_name]]$wf_servRet.la[, , j + 1, ] <- 
    apply((ls_demo[[tierData$tier_name]]$wf_servRet.la[, , j, ] - ls_demo[[tierData$tier_name]]$temp$out_servRet.la), 3, function(x) x %*% A) %>% 
    array(wf_dim.servRet.la[-3])
  
  ls_demo[[tierData$tier_name]]$wf_servRet.la[, , j + 1, j + 1] <- ls_demo[[tierData$tier_name]]$temp$in_servRet.la %*% A
  
  
  # terminated with vested benefits
  ls_demo[[tierData$tier_name]]$wf_defrRet.vest[, , j + 1, ] <- 
    apply((ls_demo[[tierData$tier_name]]$wf_defrRet.vest[, , j, ] - ls_demo[[tierData$tier_name]]$temp$out_defrRet.vest), 3, function(x) x %*% A) %>% 
    array(wf_dim.defrRet.vest[-3])
  
  ls_demo[[tierData$tier_name]]$wf_defrRet.vest[, , j + 1, j + 1] <- ls_demo[[tierData$tier_name]]$temp$in_defrRet.vest %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  
  
  # terminated with refund
  ls_demo[[tierData$tier_name]]$wf_defrRet.refund[, , j + 1, ] <- 
    apply((ls_demo[[tierData$tier_name]]$wf_defrRet.refund[, , j, ] - ls_demo[[tierData$tier_name]]$temp$out_defrRet.refund), 3, function(x) x %*% A) %>% 
    array(wf_dim.defrRet.refund[-3])
  
  ls_demo[[tierData$tier_name]]$wf_defrRet.refund[, , j + 1, j + 1] <- ls_demo[[tierData$tier_name]]$temp$in_defrRet.refund %*% A     # Note that termination year j = 1 correponds to init_year - 1?

  
  # disability retirees
  ls_demo[[tierData$tier_name]]$wf_disbRet[, , j + 1, ] <- 
    apply((ls_demo[[tierData$tier_name]]$wf_disbRet[, , j, ] - ls_demo[[tierData$tier_name]]$temp$out_disbRet), 3, function(x) x %*% A) %>% 
    array(wf_dim.disbRet[-3])
  
  ls_demo[[tierData$tier_name]]$wf_disbRet[, , j + 1, j + 1] <- ls_demo[[tierData$tier_name]]$temp$in_disbRet %*% A
  
  
  # dead members 
  ls_demo[[tierData$tier_name]]$wf_dead[, ,   j + 1]    <- (ls_demo[[tierData$tier_name]]$wf_dead[, , j] + ls_demo[[tierData$tier_name]]$temp$in_dead) %*% A
  
  
  # death beneficiaries
  ls_demo[[tierData$tier_name]]$wf_deathBen[, , j + 1, ] <- 
    apply((ls_demo[[tierData$tier_name]]$wf_deathBen[, , j, ] - ls_demo[[tierData$tier_name]]$temp$out_deathBen), 3, function(x) x %*% A) %>% 
    array(wf_dim.deathBen[-3])
  
  ls_demo[[tierData$tier_name]]$wf_deathBen[, , j + 1, j + 1] <- ls_demo[[tierData$tier_name]]$temp$in_deathBen %*% A
  
  # newDeath.act[j]  <- sum(active2dead)
  # newDeath.ret[j]  <- sum(la2dead)
  # newDisb.act[j]   <- sum(active2disbRet)

}

# Remove the temporary list
ls_demo$miscAll$temp <- NULL


#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

ls_demo[[tierData$tier_name]]

# Active members
ls_demo[[tierData$tier_name]]$wf_active <- 
  adply(ls_demo[[tierData$tier_name]]$wf_active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, nactives, -ea, -year) %>% 
  mutate(year = f2n(year), 
         age = as.numeric(age),
         yos = age - ea) %>% 
  filter(age >= ea) %>% 
  relocate(year, ea, age, yos)


# service retirees
ls_demo[[tierData$tier_name]]$wf_servRet.la <- 
  data.frame(
    expand.grid(
      ea           = range_ea,
      age          = range_age,
      year         = init_year:(init_year + nyear - 1),
      year_servRet = init_year:(init_year + nyear - 1)
    ),
    n_servRet.la = as.vector(ls_demo[[tierData$tier_name]]$wf_servRet.la)
  ) %>%
  filter(age >= ea)


# terminated with vested benefits
ls_demo[[tierData$tier_name]]$wf_defrRet.vest <- 
  data.frame(
    expand.grid(
      ea   = range_ea,
      age  = range_age,
      year = init_year:(init_year + nyear - 1),
      year_defrRet.vest = (init_year):(init_year + nyear - 1)
    ),
    n_defrRet.vest = as.vector(ls_demo[[tierData$tier_name]]$wf_defrRet.vest)
  ) %>%
  filter(age >= ea)


# terminated with refund
ls_demo[[tierData$tier_name]]$wf_defrRet.refund <- 
  data.frame(
    expand.grid(
      ea   = range_ea,
      age  = range_age,
      year = init_year:(init_year + nyear - 1),
      year_defrRet.refund = (init_year):(init_year + nyear - 1)
    ),
    n_defrRet.refund = as.vector(ls_demo[[tierData$tier_name]]$wf_defrRet.refund)
  ) %>%
  filter(age >= ea)


# disability retirees
ls_demo[[tierData$tier_name]]$wf_disbRet <-
  data.frame(
    expand.grid(
      ea   = range_ea,
      age  = range_age,
      year = init_year:(init_year + nyear - 1),
      year_disbRet = (init_year):(init_year + nyear - 1)
    ),
    n_disbRet = as.vector(ls_demo[[tierData$tier_name]]$wf_disbRet)
  ) %>%
  filter(age >= ea)


ls_demo[[tierData$tier_name]]$wf_deathBen <-
  data.frame(
    expand.grid(
      ea = range_ea,
      age = range_age,
      year = init_year:(init_year + nyear - 1),
      year_death = (init_year):(init_year + nyear - 1)
    ),
    n_deathBen = as.vector(ls_demo[[tierData$tier_name]]$wf_deathBen)
  ) %>%
  filter(age >= ea)



#*******************************************************************************
#                  Number of new contingent annuitants                      ####
#*******************************************************************************

# wf_new.ca <- wf_active %>% left_join(decrement_wf %>% select(age, ea, qxr.ca)) %>% 
#              mutate(new_ca  = number.a * qxr.ca,
#                     year = year + 1,
#                     age  = age + 1)


# wf_new.disb.ca <- wf_active %>% left_join(decrement_wf %>% select(age, ea, qxd.ca)) %>% 
#   mutate(new_disb.ca  = number.a * qxd.ca,
#          year = year + 1,
#          age  = age + 1)

# wf_new.disb.ca %>% group_by(year) %>% summarize(new_disb.ca = sum(new_disb.ca))
# wf_new.ca %>% group_by(year) %>% summarize(new_ca = sum(new_ca))



# Final outputs
# pop <-  list(active  = wf_active, 
#              term    = wf_term, 
#              disbRet = wf_disbRet, 
#              la = wf_la, 
#              dead = wf_dead,
#              deathBen = wf_deathBen
#              #new_ca = wf_new.ca
# )

return(ls_demo)

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



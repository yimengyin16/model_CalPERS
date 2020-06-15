




# Inputs:
#	 inputs/data_proc/Data_CalPERS_demographics_20180630_raw.RData
#     - df_nactives_raw
#     - df_nretirees_raw
#     
#     - agecuts_actives
#     - yoscuts_actives
#     
#     - agecuts_retirees


# Outputs:
#    -  imputed member data in tidy format




#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data  <- "inputs/data_proc/"



#*******************************************************************************
#                      ##  Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_CalPERS_demographics_20180630_raw.RData"))



#*******************************************************************************
#                      ##  Exploring spreading the cells    ####
#*******************************************************************************

# Interpolation of actives
fillin.actives.spreadyos.splineage <- function(lactives) {
	# salary:
	#   first spread uniformly within age.cell-yos.cell group (same salary for all)
	#   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
	#   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
	#   then add ea to the data
	# nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
	
	lactives
	
	adf <- lactives$actives.yos
	agecuts <- lactives$agecuts
	yoscuts <- lactives$yoscuts
	#eacuts <- lactives$eacuts
	minage <- min(agecuts$agelb)
	maxage <- max(agecuts$ageub)
	minyos <- min(yoscuts$yoslb)
	maxyos <- max(yoscuts$yosub)
	
	planname <- paste0(adf$planname[1])
	
	# adf %>% select(age, ea, salary) %>% spread(ea, salary)
	# adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
	
	# create a master grouped data frame
	adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
		mutate(pay.cell=nactives.cell * salary.cell) %>%
		mutate(ageidx = findInterval(age.cell, agecuts$agelb),
					 age.lb = agecuts$agelb[ageidx],
					 age.ub = agecuts$ageub[ageidx],
					 yosidx = findInterval(yos.cell, yoscuts$yoslb),
					 yos.lb = yoscuts$yoslb[yosidx],
					 yos.ub = yoscuts$yosub[yosidx]) %>%
		select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  adf.g	

	# expand the grouped data frame to all allowable age-yos combinations ####
	xpnd <- function(df) {
		# expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
		# keep the recrods with max ea
		df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
			mutate(ea=age - yos) %>%
			filter((ea >= 20) | (ea<20 & ea==max(ea))) %>%
			select(-ea)
		return(df2)
	}
	
	adf.x <- adf.g %>% rowwise() %>%
		do(cbind(., xpnd(.))) %>%
		ungroup %>%  # get rid of rowwise
		group_by(age.cell, yos.cell) %>%
		mutate(n.cell=n()) %>%
		select(age, yos, everything()) %>%
		arrange(age, yos)
	
	
	# work with the expanded data ####
	
	# we have to anchor the endpoints with reasonable values BEFORE computing the spline
	adjustends <- function(age, salary) {
		# the basic idea is that if an endpoint is NA, insert a plausible value
		
		# simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
		# otherwise use the capped value
		firstnonna <- salary[which.min(is.na(salary))]
		lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
		bound <- .5
		firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
		lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
		cap <- function(sal, range) {
			cappedval <- max(sal, range[1])
			cappedval <- min(cappedval, range[2])
			return(cappedval)
		}
		
		salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
		salary.adjusted <- salary
		
		if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
		ilast <- length(salary)
		if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)
		
		return(salary.adjusted)
	}
	
	# test out adjustends
	# fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
	# # various salaries to try out
	# salary <- seq(20, 50, length.out = 10)
	# salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
	# salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
	# salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
	# salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
	# salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
	# age <- 21:30
	# d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
	#   mutate(sal.spline=fs(age, salary),
	#          saladj.spline=fs(age, saladj))
	# d
	# qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)
	
	
	spline.y2 <- function(age, salary, safesalary) {
		# safesalary is what we use if salary has no data
		if(all(is.na(salary))) {
			print("AllNA")
			salary <- safesalary
		}
		salary.adjusted <- adjustends(age, salary)
		
		sp.out <- spline(age, salary.adjusted, xout=age)
		salout <- sp.out$y
		return(salout)
	}
	
	adf.x3 <- adf.x %>% ungroup %>% # MUST be ungrouped or ifelse won't work if there is only one rec in a group
		mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
					 salary.group=ifelse(age==age.cell & yos==yos.cell, salary.cell, NA),
					 salary.group=ifelse(salary.group==0, NA, salary.group),
					 salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
		group_by(yos) %>%
		arrange(age) %>%
		mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
		group_by(age.cell, yos.cell) %>%
		mutate(planname=planname,
					 pay.unadj=sum(salary.spline.adjep * nactives),
					 adjust=pay.cell / pay.unadj,
					 salary.final=salary.spline.adjep * adjust,
					 pay.adj=sum(salary.final * nactives),
					 ea=age - yos
					 #ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)]
		)
	
	return(adf.x3)
}

# data structure of the input list of fillin.actives.spreadyos.splineag
	# list name: lactives
	# $agecuts:
	#   - age.cell
	#   - agelb
	#   - ageub
	# $yoscuts:
	#   - yos.cell
	#   - yoslb
	#   - yosub
	# $actives.yos
	#   - age.cell
	#   - yos.cell
	#   - age
	#   - yos
	#   - planname
	#   - nactives
	#   - salary


# Interpolation of retirees
fillin.retirees <- function(list_data) {
	
	rdf <- select(list_data$data, planname, age, N, V) # keep only the vars we want
	agecuts <- list_data$agecuts
	
	planname <- paste0(rdf$planname[1], "_fillin")
	name_N <- list_data$varNames["name_N"]
	name_V <- list_data$varNames["name_V"]
	
	# add group ranges to the retirees data frame
	combo <- rdf %>%
		mutate(totben=N * V) %>%
		mutate(ageidx=findInterval(age, agecuts$agelb),
					 age.lb=agecuts$agelb[ageidx],
					 age.ub=agecuts$ageub[ageidx]) %>%
		arrange(age)
	
	# get avg benefits by age, via spline
	avgben <- splong(select(combo, age, V), "age", min(combo$age.lb):max(combo$age.ub))
	# force benefit to be non-negative DJB added 10/30/2015
	avgben <- avgben %>% mutate(V=ifelse(V<0, 0, V))
	
	guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
		mutate(ageidx=findInterval(age, agecuts$agelb),
					 age.cell=combo$age[match(ageidx, combo$ageidx)],
					 N.cell=combo$N[match(ageidx, combo$ageidx)],
					 V.cell=combo$V[match(ageidx, combo$ageidx)]) %>%
		group_by(age.cell) %>%
		mutate(n.cell=n(),
					 N=N.cell / n.cell, # spread nretirees evenly
					 adjV=avgben$V[match(age, avgben$age)], # get the spline-based avg benefit
					 adjtotben=N * adjV)
	
	# refine the guess by adjusting ensure that we hit the right total benefits in each group
	guessdf2 <- guessdf %>% group_by(age.cell) %>%
		mutate(adjust=mean(N.cell * V.cell) / sum(adjtotben),
					 V=adjV*adjust,
					 totben=N * V)
	
	rdf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
		select(planname, age.cell, age, N, V) %>%
		ungroup
	#plyr::rename(c("N" = list_data$varNames["name_N"])))
	
	names(rdf.fillin)[names(rdf.fillin) == "N"] <- name_N
	names(rdf.fillin)[names(rdf.fillin) == "V"] <- name_V
	
	return(rdf.fillin)
}

# data structure of the input list of fillin.actives.spreadyos.splineag
  # list name: ldata
  # $agecuts:
  #   - age.cell
  #   - agelb
  #   - ageub
  # $yoscuts:
  #   - yos.cell
  #   - yoslb
  #   - yosub
  # $data
  #   - age.cell
  #   - age
  #   - planname
  #   - N
  #   - V
  # $varNames
  #   - name_N
  #   - name_V  


get_agecuts <- function(df){
	df %>% 
		mutate(age.cell = (age_lb + age_ub)/ 2) %>% 
		select(age.cell, agelb = age_lb, ageub = age_ub) 
}


#*******************************************************************************
#                    1.  Initial actives   ####
#*******************************************************************************

# The output data frame includes active members of all tiers.
# Will need to break it down later when modeling tiers separately. 

df_nactives
agecuts_actives
yoscuts_actives



# Prepare data for the interpolation function


make_lactives <- function(df, agecuts, yoscuts){
  lactives <- list(
  	agecuts = agecuts,
  	yoscuts = yoscuts,
  	actives.yos = 
  		df %>%
  		select(
  			planname = tier,
  			age.cell,
  			yos.cell,
  			nactives,
  			salary,
  		) %>%
  		mutate(age = age.cell, yos = yos.cell)
  )
}

fillin_actives <- function(df){

		fillin.actives.spreadyos.splineage(df) %>% 
		ungroup %>%
		select(planname, age, yos, ea,
					 #age.cell, yos.cell,
					 nactives, 
					 salary = salary.final) %>% 
		mutate_at(vars(nactives, salary), funs(na2zero))
	
}


lactives_allTiers <- make_lactives(df_nactives, agecuts_actives, yoscuts_actives)
lactives_allTiers

actives_fillin_allTiers <- fillin_actives(lactives_allTiers)
actives_fillin_allTiers


# Examine results
actives_fillin_spread <- actives_fillin_allTiers %>% 
	select(ea, age, nactives) %>% 
	spread(age, nactives)

salary_fillin_spread <- actives_fillin_allTiers %>% 
	select(ea, age, salary) %>% 
	spread(age, salary)

# actives_fillin %>%
# 	summarise(avg.sal = sum(nactives * salary) / sum(nactives))


#*******************************************************************************
#              1.1 Merging external salary distribution schedules  ####
#*******************************************************************************

# Confirm that the NMR schedule cover the entire age and yos ranges
actives_fillin_allTiers$age %>% range
actives_fillin_allTiers$yos %>% range
salSchedule_NMR$age %>% range
salSchedule_NMR$yos %>% range

actives_fillin_allTiers %<>% 
	left_join(salSchedule_NMR, by = c("age", "yos")) %>% 
	mutate(salary = na2zero(salary_NMR),
				 salary_NMR = NULL)


#*******************************************************************************
#              1.2  Separating Tier 6 members and inactive members  ####
#*******************************************************************************

# Tier 6 members
# Members who joined the plan after April 1, 2012 are in Tier 4
# The number of Tier 6 members (active and non-active) as of 2018/03/31
# - 205020 out of 616906 (33.2%)
# How to separate Tier 6 members from the original member matrix
# YOS of Tier 6 members: 0-5
# Issue: the number of member within this YOS range is greater than the target (205020)
# - Original table: 224,465 members in the 0-4 YOS group
# - filled table  : 245,867 members with YOS <= 5.
# This implies that some members with YOS <= 4(and 5) are not Tier 6 members
# Possible reasons: the number includes Tier 3-5 separated members with YOS <= 4(and 5). (assuming the YOS inactive members is frozen when separated.) 


# Inactive members
# The number of inactive members: 115,961 out of 616906 (18.8%)
# How the separate inactive members from the original member matrix


# Plan 1
# YOS <= 5: 245866.5
# Tier 6 active     168005
# Tier 6 inactive    37015
# Tier 3-5 inactive (should be age >= 26) 40846.5 ( = 245866.5 - 168005 - 37015)
# YOS > 5: 371039.5
# Tier 3-5 active   332940
# Tier 3-5 inactive 38099.5 ( = 411886-332940-40846.5)

# Tier 6 total   =   205020
# Tier 3-5 total =   411886

# Step 1: Separating members with YOS <=5 and YOS > 5. 
# Step 2: estimate numbers of active members for Tier 6 and Tier 1-5
# TEMP: use this method for now

# Plan 2
# Separate all inactive members based on separation rate
# Separate Tier 6 active members from Tier 3-5 active members with YOS <= 5
# TODO: may want to adjust the original grouped table first. 


## Look at data
member_byTier_ERS
member_byStatus_ERS


actives_fillin_allTiers %>% 
	filter(yos <=5, age >=26) %>% 
	pull(nactives) %>% sum

actives_fillin_allTiers %>% 
	filter(yos <=4) %>% 
	summarise(N = sum(nactives))


actives_fillin_allTiers$yos %>% range
actives_fillin_allTiers$age %>% range


## Construct separation rates by age and yos
df_qxt_full <-
	expand.grid(age = 20:69, yos = 0:39) %>% as_tibble() %>%
	left_join(df_qxt, by = "age") %>%
	mutate(qxt = case_when(yos < 2 ~ yos_lt2,
												 yos == 2 ~ yos_e2,
												 yos == 3 ~ yos_e3,
												 yos == 4 ~ yos_e4,
												 yos>=5&yos<=9 ~ yos_5to9,
												 yos>=10 ~ yos_ge10,
												 TRUE ~ NA_real_
	)) %>%
	select(age, yos, qxt)


## Constants
N_inactives <- filter(member_byStatus_ERS, year == 2018)$ninactives

N_actives_t6   <- 168005
N_inactives_t6 <- 37015

N_actives_nont6   <- 332940
N_inactives_nont6 <- 78946


## Implementing method 1

# Step 1: Separating members with YOS <=5 and YOS > 5. 
# YOS <= 5
# Tier 6 active 168k
# Tier 6 inactive 37k
# Tier 3-5 inactive (should be age >= 26) 41k
# YOS > 5
# Tier 3-5 active 333k 
# Tier 3-5 inactive 38k

df <- 
	actives_fillin_allTiers %>% 
	left_join(df_qxt_full, by = c("age", "yos")) %>% 
	rename(nmembers = nactives) %>% 
	mutate(nmember_yos5minus = ifelse(yos <=5, nmembers, 0),
				 nmember_yos5plus  = ifelse(yos >5,  nmembers, 0),
				 
				 salary_yos5minus = ifelse(yos <=5, salary, 0),
				 salary_yos5plus  = ifelse(yos >5,  salary, 0),
				 
	) 

df$nmember_yos5minus %>% sum
df$nmember_yos5plus  %>% sum

# Step 2: estimate numbers of active members for Tier 6 and Tier 1-5
df %<>% 
	mutate(
		nactives_tier6    = nmember_yos5minus - nmember_yos5minus * qxt * (sum(nmember_yos5minus) - N_actives_t6) / sum(nmember_yos5minus * qxt),
		nactives_nontier6 = nmember_yos5plus  -  nmember_yos5plus * qxt * (sum(nmember_yos5plus)  - N_actives_nont6) / sum(nmember_yos5plus * qxt)
	)

df$nactives_tier6 %>% sum
df$nactives_nontier6 %>% sum


actives_fillin_t6 <- 
	df %>% select(planname, age, yos, ea, nactives = nactives_tier6, salary = salary_yos5minus) %>% 
	mutate(planname = "tier6")

actives_fillin_nont6 <- 
	df %>% select(planname, age, yos, ea, nactives = nactives_nontier6, salary = salary_yos5plus) %>% 
	mutate(planname = "nontier6")

# actives_fillin_nont6$nactives %>% sum
# actives_fillin_t6$nactives %>% sum


#*******************************************************************************
#                    1.3 Applying assumed salary distribution   ####
#*******************************************************************************

# Constraints for the estimation
# 1. yos/age distribution, based on other large general employee plans or NMR2011
# 2. total salaries of non-Tier 6 and Tier 6 members
# non-Tier 6: 20,740,475,035 
# Tier 6:      5,232,194,578 

# TEMP: For now, use NMR2011 distribution

salary_tot_nontier6 <- 20740475035
salary_tot_tier6    <-  5232194578 

# actives_fillin_t6
# actives_fillin_nont6
# actives_fillin_allTiers


actives_fillin_nont6 %<>% 
	mutate(salary = salary * salary_tot_nontier6 / sum(salary * nactives))

actives_fillin_t6 %<>% 
	mutate(salary = salary * salary_tot_tier6 / sum(salary * nactives))


(actives_fillin_nont6$salary * actives_fillin_nont6$nactives) %>% sum
(actives_fillin_t6$salary    * actives_fillin_t6$nactives)    %>% sum


actives_fillin_allTiers %<>% 
	left_join(select(actives_fillin_t6,    age, yos, salary_t6    = salary, nactives_t6    = nactives), by = c("age", "yos")) %>% 
	left_join(select(actives_fillin_nont6, age, yos, salary_nont6 = salary, nactives_nont6 = nactives), by = c("age", "yos")) %>% 
	mutate(salary   = ifelse(salary_t6   !=0, salary_t6,   salary_nont6),
				 nactives = ifelse(nactives_t6 !=0, nactives_t6, nactives_nont6)
				 ) %>% 
	select(-salary_t6, -salary_nont6, -nactives_t6, -nactives_nont6)

# Check the result
#actives_fillin_allTiers$nactives %>% sum
#sum(with(actives_fillin_allTiers, nactives*salary))


# Constructing inputs for the model
init_actives_tiers <- 
	bind_rows(
		actives_fillin_allTiers,
		actives_fillin_t6,
		actives_fillin_nont6
	) %>% 
	rename(tier = planname)

init_actives_tiers

# Spread and examine the results
df_spread <- actives_fillin_t6 %>%
	select(ea, age, nactives) %>%
	spread(age, nactives)







#*******************************************************************************
#                    2.  Initial service retirees   ####
#*******************************************************************************

df_nservRets
agecuts_servRets
yoscuts_servRets

# Spreading age/yos cells
make_lservRets <- function(df, agecuts, yoscuts){
	lactives <- list(
		agecuts = agecuts,
		yoscuts = yoscuts,
		actives.yos = 
			df %>%
			select(
				planname = tier,
				age.cell,
				yos.cell,
				nactives = nservRets,
				salary   = benefit_servRet,
			) %>%
			mutate(age = age.cell, yos = yos.cell)
	)
}

fillin_servRets <- function(df){
	
	fillin.actives.spreadyos.splineage(df) %>% 
		ungroup %>%
		select(planname, age, yos, ea,
					 #age.cell, yos.cell,
					 nactives, 
					 salary = salary.final) %>% 
		mutate_at(vars(nactives, salary), funs(na2zero)) %>% 
		rename(nservRets = nactives,
					 benefit_servRet = salary)
}


lservRets <- make_lservRets(df_nservRets, agecuts_servRets, yoscuts_servRets)
lservRets

servRets_fillin_ageYOS <- fillin_servRets(lservRets)
servRets_fillin_ageYOS




# Turning age/yos cells into age groups

servRets_fillin <- 
	servRets_fillin_ageYOS %>% 
	group_by(age) %>% 
	summarise(planname  = unique(planname),
						benefit_servRet = sum(nservRets*benefit_servRet) / sum(nservRets),
						nservRets = sum(nservRets)
	)
servRets_fillin

# Check the data
# servRets_fillin_ageYOS$nservRets %>% sum
# servRets_fillin$nservRets %>% sum	
# with(servRets_fillin_ageYOS, sum(nservRets * benefit_servRet))
# with(servRets_fillin, sum(nservRets * benefit_servRet))


# Constructing inputs for the model
init_servRets_tiers <- 
	bind_rows(
		servRets_fillin,
		servRets_fillin %>% mutate(planname = "nontier6"),
		servRets_fillin %>% mutate(planname = "tier6", nservRets = 0, benefit_servRet = 0)
	) %>% 
	rename(tier = planname)
init_servRets_tiers
	


# Examine results
servRets_fillin_spread <- servRets_fillin_ageYOS %>% 
	select(ea, age, nservRets) %>% 
	spread(age, nservRets)

benefit_servRet_fillin_spread <- servRets_fillin_ageYOS %>% 
	select(ea, age, benefit_servRet) %>% 
	spread(age, benefit_servRet)

servRets_fillin_spread 
benefit_servRet_fillin_spread


#*******************************************************************************
#                    3.  Initial disability retirees   ####
#*******************************************************************************

df_ndisbRets
agecuts_disbRets
yoscuts_disbRets

# Spreading age/yos cells
make_ldisbRets <- function(df, agecuts, yoscuts){
	lactives <- list(
		agecuts = agecuts,
		yoscuts = yoscuts,
		actives.yos = 
			df %>%
			select(
				planname = tier,
				age.cell,
				yos.cell,
				nactives = ndisbRets,
				salary   = benefit_disbRet,
			) %>%
			mutate(age = age.cell, yos = yos.cell)
	)
}

fillin_disbRets <- function(df){
	
	fillin.actives.spreadyos.splineage(df) %>% 
		ungroup %>%
		select(planname, age, yos, ea,
					 #age.cell, yos.cell,
					 nactives, 
					 salary = salary.final) %>% 
		mutate_at(vars(nactives, salary), funs(na2zero)) %>% 
		rename(ndisbRets = nactives,
					 benefit_disbRet = salary)
}

ldisbRets <- make_ldisbRets(df_ndisbRets, agecuts_disbRets, yoscuts_disbRets)
ldisbRets

disbRets_fillin_ageYOS <- fillin_disbRets(ldisbRets)
disbRets_fillin_ageYOS


# Turning age/yos cells into age groups

disbRets_fillin <- 
	disbRets_fillin_ageYOS %>% 
	group_by(age) %>% 
	summarise(planname  = unique(planname),
						benefit_disbRet = sum(ndisbRets*benefit_disbRet) / sum(ndisbRets),
						ndisbRets = sum(ndisbRets)
	)
disbRets_fillin

# Check the data
# disbRets_fillin_ageYOS$ndisbRets %>% sum
# disbRets_fillin$ndisbRets %>% sum
# with(disbRets_fillin_ageYOS, sum(ndisbRets * benefit_disbRet))
# with(disbRets_fillin, sum(ndisbRets * benefit_disbRet))



# Constructing inputs for the model
init_disbRets_tiers <- 
	bind_rows(
		disbRets_fillin,
		disbRets_fillin %>% mutate(planname = "nontier6"),
		disbRets_fillin %>% mutate(planname = "tier6", ndisbRets = 0, benefit_disbRet = 0)
	) %>% 
	rename(tier = planname)
init_disbRets_tiers



# Examine results
disbRets_fillin_spread <- disbRets_fillin_ageYOS %>% 
	select(ea, age, ndisbRets) %>% 
	spread(age, ndisbRets)

benefit_disbRet_fillin_spread <- disbRets_fillin_ageYOS %>% 
	select(ea, age, benefit_disbRet) %>% 
	spread(age, benefit_disbRet)

disbRets_fillin_spread 
benefit_disbRet_fillin_spread







#*******************************************************************************
#                    6.  save results   ####
#*******************************************************************************

init_actives_tiers
init_servRets_tiers
init_disbRets_tiers
# init_terms_tiers



save(
	init_actives_tiers,
	init_servRets_tiers,
	#init_survivors,
	init_disbRets_tiers,
	# init_terms_tiers,
	file = paste0(dir_data, "Data_NYSERS_memberData_20180331_imputed.RData")
)






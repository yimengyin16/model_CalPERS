# Prepare model inputs for tier "misc_classic"



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_CalPERS_decrements_ES2017_imputed.RData
#'   - inputs/data_proc/Data_CalPERS_demographics_20180630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "misc_classic"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   
#'     - tier specific parameters


#*******************************************************************************
#                               Tier specification  ####
#*******************************************************************************


##' Members included 
#'  - misc classic tier 1&2
#'  - industrial classic tier 1&2

    
##' Service retirement 
#'  
#'  - Benefit rules
#'      - Use benefit rules based on Misc Tier 1 classic members with 2%@55 rules.
#'        (The 2%@55 rule is applied to all classic members who joined before 1/15/2011, 
#'        so this rule should over the majority of classic members)
#'  
#'  - Final compensation 
#'      - The plan policy: 
#'        - 12 month for members who joined before 1/15/2011
#'        - 36 month for members who joined on or after 1/15/2011)
#'      - Model:
#'        - 12 month (1-year in the model) for classic members.
#'        - 36 month (3-year in the model) for pepra members.  
#'        - Do not model salary cap and SS offset for now, do calibration instead.
#'  
#'  Eligibility: 
#'   - classic: age>=50 & yos>=5
#'   - pepra:   age>=52 & yos>=5
#'  
#'  Vesting: yos >=5 
#'  
#'  Benefit factor: 2% at age 55


# Deferred retirement  
#' - Plan policy: eligible to receive benefit at age 50 (classic) or 52 (PEPRA).
#  - Model: start receiving benefit at 59 
#' - Simplification: do not model refund upon separation but take into account its separation rates
# TODO: check if the offical valuation report also use the age 59 assumption. 


# Disability retirement
#  
#  - Based on regular disability retirement benefit
#  - Adjust benefit upward for industrial disability
#  
#  - Eligibility: yos > 5
#
#  - Benefit: 
#     - formula: 1.8% x service x Final compensation (may want to use higher benefit factor)
#     - service:
#      - YOS if YOS <10 or YOS > 18.518, else
#      - YOS + years would have worked until 60, 
#      - with max benefit 1/3 of final compensation
#   
#  - Simplification: does not compare with service retirement benefit


# Death benefit: pre-retirement
#  - sum of 
#    -  Member's accumulated contributions, with interest max(6%, prevailing discount rate)
#    -  6 months' salary if eligible for service retirement or Alternate Death Benefit




## Assumptions, needs to be revisited 

#' Notes on group weights
#'  - See "groupWgts" tab in run control file. 
#' Active members (AV2018 np15-19)
#    - misc total:0.937
#     - misc-classic	0.679
#     - misc-pepra	  0.258
#       or 
#     - misc-Tier1	  0.918
#     - misc-Tier2	  0.018
#    
#    - ind	0.063
#     - inds-classic	0.046
#     - inds-pepra	  0.017
#' 
#' For now, the weights of active members above are applied all types of decrements, 
#' including rates for retirees. 


# Notes on gender ratio:
# 40% male and 60% female for all calculations for misc members. 



## Assumptions on demographics
# 
# Classic and PEPRA mebmers:
#  Active members:
#   - Members who joined the plan on or after Jan 1, 2013 are PEPRA members
#   - In the model, we assume that a member attains 1 yos after a full year of employment. 
#   - The dempgrahpic data from AV2018 are up to 6/30/2018
#   - Therefore, members who joined on 6/30/2013 had just attained 5 YOS and 
#     members who joined between 7/1/2012 and 6/30/2013 have 5 yos in the membership data. 
#   - Assuming the entry of new members uniformly distributed during year, we can assume that 
#     all members with YOS <= 4 and half of new members with YOS = 5 are PEPRA members. 
#  
#  Serivice retirees
#   - According to CAFR2018-19 ep159, more than 99.9% of service retirees and 
#     beneficiaries are classic members. In the model we assume all service retirees 
#     and beneficiaires in the AV data are classic members
#
#  Disability retirees. 
#   - We have not found data that show the proportion of PEPRA members in disability retirees.
#   - Because the PEPRA tier is still new and members are generally younger, we assume
#     all disability retirees in the AV data are classic members. 
# 
#  Initial terminated members
#   - For now, we assume that for each tier the liability of initial terminated members(in or not in pay status) 
#     is a fixed percentage of the AL of retirees. 
#   - As we assume the PEPRA tier has no retirees in the model, there are no AL for initial terminated members 
#     under the current simplification method. The should not be an issue because the actual AL for termianted should be 
#     very small as the PEPRA tier is still new. 







#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data    <- "inputs/data_proc/"
dir_outputs <- "model/tiers/tierData/"



# Model settings
range_age <- 20:110
range_ea  <- 20:74  # max retirement age is assumed to be 75 (qxr = 1 at age 75 in AV tables) 



# Tier specific parameters
tier_name <- "misc_classic"
age_vben  <- 59 # assumed age of starting receiving deferred retirement benefits
v.year    <- 5
fasyears  <- 1  # based on policy before PEPRA
bfactor   <- 0.02
cola_assumed <- 0.02 # assumed cola rates for valuation  
EEC_rate <- 0.0735 # TODO:check/estimate EEC rate for classic and PEPRA members


# Other tier params to add
# cola

# EEC rate, need to think about EEC

# gender ratio

#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_CalPERS_decrements_ES2017_imputed.RData"))
load(paste0(dir_data, "Data_CalPERS_demographics_20180630_fillin.RData"))



#*******************************************************************************
#                      ## Decrements 1: combining groups ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "misc_classic|inds_classic")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc_classic","wgt"] <-  0.679
wgts[wgts$grp == "inds_classic","wgt"] <-  0.046


## calculate weighted average

df_qxr_tier <- 
  df_qxr_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age, yos) %>% 
  summarise(qxr = weighted.mean(qxr, wgt), .groups = "rowwise") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()


## Disability retirement rates

# groups included
grp_include <- df_qxd_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc_t1_female","wgt"] <-  0.679 * 0.918/(0.918+0.018) * 0.6
wgts[wgts$grp == "misc_t1_male",  "wgt"] <-  0.679 * 0.918/(0.918+0.018) * 0.4
wgts[wgts$grp == "misc_t2_female","wgt"] <-  0.679 * 0.018/(0.918+0.018) * 0.6
wgts[wgts$grp == "misc_t2_male",  "wgt"] <-  0.679 * 0.018/(0.918+0.018) * 0.4
wgts[wgts$grp == "inds",  "wgt"]         <-  0.046


## calculate weighted average
# Need to combine two types of disability rates: adding the two rates

df_qxd_tier <- 
  df_qxd_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxd.nonocc = weighted.mean(qxd.nonocc, wgt), 
            qxd.occ    = weighted.mean(qxd.occ, wgt),
            qxd        = qxd.nonocc + qxd.occ,
            .groups = "rowwise") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Termination with refund

# groups included
grp_include <- df_qxt.refund_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc_t1",  "wgt"] <-  0.679 * 0.918/(0.918+0.018)
wgts[wgts$grp == "misc_t2",  "wgt"] <-  0.679 * 0.018/(0.918+0.018)
wgts[wgts$grp == "inds",     "wgt"] <-  0.046


## calculate weighted average
df_qxt.refund_tier <- 
  df_qxt.refund_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(qxt.refund = weighted.mean(qxt.refund, wgt),
            .groups = "rowwise") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()




## Termination with vested benefits

# groups included
grp_include <- df_qxt.vest_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc_t1",  "wgt"] <-  0.679 * 0.918/(0.918+0.018)
wgts[wgts$grp == "misc_t2",  "wgt"] <-  0.679 * 0.018/(0.918+0.018)
wgts[wgts$grp == "inds",     "wgt"] <-  0.046


## calculate weighted average
df_qxt.vest_tier <- 
  df_qxt.vest_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(qxt.vest = weighted.mean(qxt.vest, wgt),
            .groups = "rowwise") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()


## combine two types of termination rates

df_qxt_tier <- 
  left_join(df_qxt.vest_tier,
            df_qxt.refund_tier,
            by = c("grp", "yos", "ea")
            ) %>% 
  mutate(qxt = qxt.refund + qxt.vest,
         age = ea + yos) %>% 
  relocate(grp, ea, age, yos, qxt)


## Pre-retirement mortality
df_qxm.pre_tier <-  
  df_qxm.pre_imputed %>% 
  mutate(qxm.pre.nonocc = 0.6 * qxm.pre.nonocc_female + 0.4 * qxm.pre.nonocc_male,
         qxm.pre.occ    = 0.1 * qxm.pre.occ_female    + 0.9 * qxm.pre.occ_male,
         qxm.pre = qxm.pre.nonocc + qxm.pre.occ,
         grp = tier_name
         ) %>% 
  select(grp, age, qxm.pre, qxm.pre.nonocc, qxm.pre.occ)




## Post-retirement mortality, without projection

# Need to combine two types of disability mortality rates: using weighted average
#  - assume x% of disability retirement is job-related


df_qxm.post_tier <-  
  df_qxm.post_imputed %>% 
  mutate(qxm.post         = 0.6 * qxm.post_female         + 0.4 * qxm.post_male,
         qxmd.post.nonocc = 0.6 * qxmd.post.nonocc_female + 0.4 * qxmd.post.nonocc_male,
         qxmd.post.occ    = 0.1 * qxmd.post.occ_female    + 0.9 * qxmd.post.occ_male,
         qxmd.post        = 0.8 * qxmd.post.nonocc        + 0.2 * qxmd.post.occ,
         grp = tier_name
  ) %>% 
  select(grp, age, 
         qxm.post, qxmd.post
         #qxmd.post.nonocc, qxmd.post.occ, 
         #qxm.post_female, qxm.post_male,
         #qxmd.post.nonocc_female, qxmd.post.nonocc_male,
         #qxmd.post.occ_female,    qxmd.post.occ_male
         )


## Post-retirement mortality, with projection
df_qxm.post_proj_tier <-  
  df_qxm.post_proj_imputed %>% 
  mutate(qxm.post_proj         = 0.6 * qxm.post_female_proj         + 0.4 * qxm.post_male_proj,
         qxmd.post.nonocc_proj = 0.6 * qxmd.post.nonocc_female_proj + 0.4 * qxmd.post.nonocc_male_proj,
         qxmd.post.occ_proj    = 0.1 * qxmd.post.occ_female_proj    + 0.9 * qxmd.post.occ_male_proj,
         qxmd.post_proj        = 0.8 * qxmd.post.nonocc_proj        + 0.2 * qxmd.post.occ_proj,
         grp = tier_name
  ) %>% 
  select(grp, age, 
         qxm.post_proj, qxmd.post_proj
         #qxmd.post.nonocc_proj, qxmd.post.occ_proj,
         #qxm.post_female_proj, qxm.post_male_proj,
         #qxmd.post.nonocc_female_proj, qxmd.post.nonocc_male_proj,
         #qxmd.post.occ_female_proj,    qxmd.post.occ_male_proj
         )


# df_qxr_tier
# df_qxd_tier
# df_qxt.refund_tier
# df_qxt.vest_tier
# df_qxm.pre_tier
# df_qxm.post_tier
# df_qxm.post_proj_tier



#*******************************************************************************
#        ## Decrements 2: Single decrement table ####
#*******************************************************************************

# df_qxr_tier
# df_qxd_tier
# df_qxt.refund_tier
# df_qxt.vest_tier
# df_qxm.pre_tier
# df_qxm.post_tier
# df_qxm.post_proj_tier

decrements_tier <- expand.grid(age = range_age, 
                               ea  = range_ea) %>% 
  mutate(yos = age - ea,
         grp = tier_name) %>% 
  filter(age >= ea) %>% 
  left_join(df_qxm.pre_tier,        by = c("grp", "age")) %>%         # pre-retirement mortality 
  left_join(df_qxm.post_tier,       by = c("grp", "age")) %>%         # post-retirement mortality with no projection
  # left_join(df_qxm.post_proj_tier,  by = c("grp", "age")) %>%         # post-retirement mortality with 15-year projection
  # left_join(df_qxt.vest_tier,       by = c("grp", "ea", "yos")) %>%   # termination with vested benefit
  # left_join(df_qxt.refund_tier,     by = c("grp", "ea", "yos")) %>%   # termination with refund
  left_join(df_qxt_tier,            by = c("grp", "ea", "age", "yos")) %>%   # termination with vested benefit
  left_join(df_qxr_tier,            by = c("grp", "age", "yos")) %>%  # service retirement
  left_join(df_qxd_tier,            by = c("grp", "age")) %>%         # disability

  select(grp, ea, age, yos, qxm.pre, qxm.post, qxmd.post, qxt, qxt.vest, qxt.refund, qxr, qxd, everything())%>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.)

# decrement_tier




#*******************************************************************************
#        ## Decrements 3: adding eligibility information ####
#*******************************************************************************

# Create 2 columns for each tier
 # elig_servRet_full:  number of year of being eligible for full or greater retirement benefits
 # elig_servRet_early: number of year of being eligible for early retirement benefits; 
 #             0 after being eligible for full retirement benefits

decrements_tier  %<>% 
  group_by(ea) %>% 
  mutate(
    # Eligibility for full (or greater) retirement benefit
    elig_servRet_full = ifelse( (age >= 55 & yos >= 5), 1, 0) %>% cumsum,
    
    # Eligibility for early retirement benefit
    elig_servRet_early = ifelse( (age >= 50 & yos >= 5), 1, 0) %>% cumsum,
    elig_servRet_early = ifelse( elig_servRet_full, 0, elig_servRet_early)
    ) %>% 

  ## Adjustments to decrement rates based on eligibility
  #   1. Only keep retirement rates when a member is eligible
  #   2. Coerce termination rates to 0 when eligible for early retirement or full retirement, or age >= age_vben 
  
  mutate(
    qxr        = ifelse(elig_servRet_early | elig_servRet_full, qxr, 0),
    qxt.refund = ifelse((elig_servRet_early == 0 & elig_servRet_full == 0) & age < age_vben, qxt.refund, 0),
    qxt.vest   = ifelse((elig_servRet_early == 0 & elig_servRet_full == 0) & age < age_vben, qxt.vest,   0),
    qxt        = ifelse((elig_servRet_early == 0 & elig_servRet_full == 0) & age < age_vben, qxt,   0)
  ) %>% 
  ungroup





#*******************************************************************************
#                      ## Decrements 4: Improvement table  ####
#*******************************************************************************

# improvement for post retirement mortality

decrements_improvement <- 
  expand_grid(year =  2017:(2017+14),
              age  =  range_age) %>% 
  left_join(
    bind_rows(
      # df_qxm.post_imputed %>% mutate(year = 2017),
      # df_qxm.post_proj_imputed %>% 
      #   rename_with( ~str_remove(.x, "_proj" )) %>% 
      #   mutate(year = 2017+14)
      
      df_qxm.post_tier %>% mutate(year = 2017),
      df_qxm.post_proj_tier %>%
        rename_with( ~str_remove(.x, "_proj" )) %>%
        mutate(year = 2017+14)
      ),
    by = c("year", "age")
    )

decrements_improvement %<>% 
  group_by(age) %>% 
  arrange(age, year) %>% 
  # filter(age == 90) %>% 
  mutate(across(!c(year, grp), ~ seq(first(.x), last(.x), length.out = n()))) %>% 
  mutate(across(!c(year, grp), ~ .x / .x[year == min(year)])) %>% 
  rename_with(~ paste0("impr_", .x), !c(year, age, grp)) %>% 
  mutate(grp = tier_name )

#*******************************************************************************
#                      ## Salary Scale  ####
#*******************************************************************************


# df_salScale.merit_imputed

# groups included
grp_include <- df_salScale.merit_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc", "wgt"] <-  0.679
wgts[wgts$grp == "inds", "wgt"] <-  0.046
wgts

## calculate weighted average
df_salScale_tier <- 
  df_salScale.merit_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(salScale.merit = weighted.mean(salScale.merit, wgt),
            .groups = "rowwise") %>% 
  mutate(grp = tier_name,
         salScale.infl = 0.0275,
         salScale = salScale.merit + salScale.infl) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()



#*******************************************************************************
#                      ## Initial demographics  ####
#*******************************************************************************

##  View the inputs
# df_nactives_fillin
# df_n_servRet_fillin
# df_n_disbRet_occ_fillin
# df_n_disbRet_nonocc_fillin
# df_n_beneficiaries_fillin


## groups included 
grp_include <- df_nactives_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]



## Active members

# all active members 
df_n_actives_tier <- 
  df_nactives_fillin %>% 
  filter(grp %in% grp_include) %>% 
  # left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(salary   = weighted.mean(salary, nactives, na.rm = TRUE) %>% na2zero(),
            nactives = sum(nactives, na.rm= TRUE) %>% na2zero,
            .groups = "rowwise") %>% 
  mutate(grp = tier_name,
         age = ea + yos) %>% 
  relocate(grp) %>% 
  arrange(ea, age) %>% 
  ungroup()

# CalPERS: Check total benefit againt the AV value: payroll
# sum(df_n_actives_tier$nactives * df_n_actives_tier$salary)
# model/target: 12951558687/12950836352 = 100.0056%


# Keep classic members only
#  assume 
#    - members with yos <= 4 are all pepra members
#    - 50% of members with yos == 5 are pepra members
#    - the rest are classic membrs 

df_n_actives_tier %<>% 
  mutate(nactives = case_when(
    yos <= 4 ~ 0,
    yos == 5 ~ nactives * 0.5,
    TRUE ~ nactives
  ))

# Potential issue:
#  - Currently we have not found data about the proportion of PEPRA members in
#    state PERF A members. 
#  - The closest is the number of classic and PEPRA members in 
#    the entire PERF A, including both state and non-state members. (CAFR2019-19 ep159)
#    It shows that 72.5% misc(and inds?) members are classic members. 
#  - Under the assumptions described above, classic members only accounts for 
#    61% of total state PERF A members. 
#  - This can be because 
#      1. We have overestimated the number of PEPRA members under our assumption. 
#      2. The proportion of PEPRA members differs for state and non-state PERF A members. 
#      3. A combination of the two. 
# 
# TODO: For now, we will just stick with the simple assumptions, and see if the 
#       simulation results are off the target too much. 
#  

# (df_n_actives_tier$nactives %>% sum)*0.725
# (df_n_actives_tier$nactives %>% sum)*0.275
# 
# df_n_actives_tier %>%
#   mutate(nactives_c = case_when(
#     yos <= 4 ~ 0,
#     yos == 5 ~ nactives * 0.5,
#     TRUE ~ nactives
#   )) %>%
#   summarise(nactives_c = sum(nactives_c),
#             nactives   = sum(nactives)) %>%
#   mutate(c_share = nactives_c / nactives)





## Service retirees
 # For now, combine service retirees and beneficiaries

# assume all service retirees are classic members

df_n_servRet_tier <- 
  full_join(df_n_servRet_fillin,
            df_n_beneficiaries_fillin,
            by = c("AV_date", "grp", "age", "age.cell")
            ) %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_servRet = weighted.mean(benefit_servRet, n_servRet, na.rm= TRUE),
            n_servRet       = sum(n_servRet, na.rm = TRUE),
            
            benefit_beneficiaries = weighted.mean(benefit_beneficiaries, n_beneficiaries, na.rm= TRUE),
            n_beneficiaries       = sum(n_beneficiaries, na.rm = TRUE),
            
            .groups = "drop") %>% 
  colwise(na2zero)(.) %>% 
  mutate(grp = tier_name,
         benefit_servRet = na2zero((benefit_servRet * n_servRet + benefit_beneficiaries * n_beneficiaries) / (n_servRet + n_beneficiaries)), 
         n_servRet       = n_servRet + n_beneficiaries
         ) %>% 
  select(grp, age, n_servRet, benefit_servRet) %>% 
  arrange(age) %>% 
  ungroup()

# CalPERS: Check total benefit againt the AV value (AV2018 ep141-145)
# Note payments for beneficiaries (death after retirement) are included
# (df_n_servRet_tier$n_servRet*df_n_servRet_tier$benefit_servRet) %>% sum
# model/target:5867048719/5880795001 = 99.77%



## Disability retirees
 # For now, combine industrial and non-industrial disability retirees

# Assume all disability retirees are classic members

df_n_disbRet_tier <- 
  left_join(df_n_disbRet_nonocc_fillin,
            df_n_disbRet_occ_fillin,
            by = c("AV_date", "grp", "age", "age.cell")
  ) %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_disbRet_nonocc = weighted.mean(benefit_disbRet_nonocc, n_disbRet_nonocc, na.rm= TRUE),
            n_disbRet_nonocc       = sum(n_disbRet_nonocc, na.rm = TRUE),
            
            benefit_disbRet_occ   = weighted.mean(benefit_disbRet_occ, n_disbRet_occ, na.rm= TRUE),
            n_disbRet_occ         = sum(n_disbRet_occ, na.rm = TRUE),
            
            .groups = "rowwise") %>% 
  mutate(grp = tier_name,
         benefit_disbRet = na2zero((benefit_disbRet_nonocc * n_disbRet_nonocc + benefit_disbRet_occ * n_disbRet_occ) / (n_disbRet_occ + n_disbRet_nonocc)),
         n_disbRet       = n_disbRet_occ + n_disbRet_nonocc
         ) %>% 
  select(grp, age, n_disbRet, benefit_disbRet) %>% 
  arrange(age) %>% 
  ungroup()

# CalPERS: Check total benefit againt the AV value
# (df_n_disbRet_tier$n_disbRet*df_n_disbRet_tier$benefit_disbRet) %>% sum
# model/target:225566624/225566841 = 99.9999%


## View the results
# df_n_actives_tier
# df_n_servRet_tier
# df_n_disbRet_tier


 
#*******************************************************************************
#                    ## Saving tier information in a list  ####
#*******************************************************************************

# collect tier-specific parameters in a list
tier_params <- 
  list(
    tier_name = tier_name,
    age_vben  = age_vben,
    v.year    = v.year,
    fasyears  = fasyears,  # based on policy before PEPRA
    cola_assumed = cola_assumed,
    
    bfactor = bfactor,
    EEC_rate = EEC_rate
  )


# Store all tier data in a list
assign(paste0("tierData_", tier_name), 
       
         list(
           tier_name = tier_name,
           
           decrements = decrements_tier,
           decrements_improvement = decrements_improvement,
           
           df_n_actives = df_n_actives_tier,
           df_n_servRet = df_n_servRet_tier,
           df_n_disbRet = df_n_disbRet_tier,
           
           df_salScale  = df_salScale_tier,
           
           tier_params = tier_params
         )
       )

# Save the list of tier data in a .rds (single object) file
saveRDS(get(paste0("tierData_", tier_name)), 
        file = paste0(dir_outputs, "tierData_", tier_name, ".rds"))


# tierData <- readRDS(paste(dir_outputs, "tierData_", tier_name, ".rds"))



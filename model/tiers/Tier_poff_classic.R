# Prepare model inputs for tier "sfty_classic"



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_CalPERS_decrements_ES2017_imputed.RData
#'   - inputs/data_proc/Data_CalPERS_demographics_20180630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "sftyAll"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   
#'     - tier specific parameters


#*******************************************************************************
#                               Tier specification  ####
#*******************************************************************************


##' Members included 
#'  - state safety classic
#'  - state safety PEPRA
#'  - state POFF classic
#'  - state POFF PEPRA
#'  - CHP classic
#'  - CHP PEPRA
    

##' Service retirement 
#'  
#'  - Benefit rules: Plan policy 
#'     - safety (37%): 
#'         -* 2.5%55 for Classic members before Jan 15, 2011)
#'         -  2.5%@60 or 2%@55 for Classic on or after Jan 15, 2011  (Only applied for two years?)
#'         -* 2%@57 for PEPRA members (after Jan 1 2013)
#'     - POFF (53%)   
#'         -* 3%@55 or 3%@50 for Classic before Jan 15, 2011
#'         -  2.5%@55 for Classic (except firefighters) on or after Jan 15, 2011  (Only applied for two years?)
#'         -  3%@55 for Classic on or after Oct 31, 2010
#'         -* 2.5%@57 or 2.7@57 for PEPRA members (after Jan 1 2013) 
#'     - CHP (10%)
#'         -* 3%@50 for Classic before Oct 31, 2010
#          -  3%@55 for Classic on or after Oct 31, 2010
#          -* 2.7%@57 for PEPRA members (after Jan 1 2013)  
#'
#'
#'  - Benefit rules: model 
#'         - Classic members: 3%@50 constant, may be need to adjust downward
#'         - PEPRA members:   2.5%@57, benefit reduction follows POFF ( -0.2/7 each year before 57)  
#'
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
#'  
#'  Eligibility: 
#'   - age>=50 & yos>=5
#'  
#'  Vesting: yos >=5 
#'  
#'  Benefit factor: constant 3%, adjust downward for calibration
#'  Final compensation: 12 month
#'
# Deferred retirement  
#' start receiving benefit at 59
#' - Simplification: do not model refund upon separation but take into account its separation rates


# Disability retirement
#  
#  - Based on industrial disability retirement benefit
#  - Adjust benefit downward for industrial disability
#  
#  - Eligibility: no restriction on YOS
#
#  - Benefit: 
#     - 50% of final compensation
#     - May choose Service Retirement benefit if larger 
#     - Simplification: does not compare with service retirement benefit


# Death benefit: pre-retirement
#  - sum of 
#    -  Member's accumulated contributions, with interest max(6%, prevailing discount rate)
#    -  6 months' salary if eligible for service retirement or Alternate Death Benefit



## Assumptions, needs to be revisited 

# Notes on aggregate EEC rate:
#  -  calculated based on covered payroll and EEC in AV2018 np17-19
#  -  Safety: 2316124913, 256385863
#  -  POFF:   3522647266, 426055155
#  -  CHP:    871895121,  96865133
# (96865133 + 426055155 + 256385863)/(2316124913+3522647266+871895121) = 0.1161
#'
#' Model:
#'   - use the same EEC rate of 11.61% for both Classic and PEPRA members.    
#'      



# Other tier params to add

#' Notes on group weights
#'  - weights are constructed based on member data provided in AV2018 and 
#'    CAFR2018-19 ep159 (classic vs pepra)
#' Active members (AV2018 np15-19)
#   - safety-classic	0.2605
#   - safety-pepra	  0.1078
#   - poff-classic	  0.3796
#   - poff-pepra	    0.1570
#   - chp-classic	    0.0672
#   - chp-pepra	      0.0278
#
# How group specific data are aggregated:
#  - Retirement rates are aggregated using classic/PEPRA specific weights.
#  - For other decrements:
#    - classic: Aggregated using the weights for classic members only
#    - PEPRA: Aggregated using total weight of classic and PEPRA for each type of members (safety/poff/chp)
#             Because the total weights may better reflect the weights in the long run when the PEPRA
#             tier becomes more mature. 



# Notes on gender ratio:
# ?90% male and 10% female for all calculations for safety members. 


# Need to combine two types of disability mortality rates: using weighted average
#  - assume 50% of disability retirement is job-related
#  -used in df_qxm.post_tier and df_qxm.post_proj_tier

# Assumed inflation in salary scale: salScale.infl = 0.0275




#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data    <- "inputs/data_proc/"
dir_outputs <- "model/tiers/tierData/"


# Model settings
range_age <- 20:110
range_ea  <- 20:74  # max retirement age is assumed to be 75 (qxr = 1 at age 75 in AV tables) 



# Tier specific parameters

tier_name <- "poff_classic"
age_vben  <- 59 # assumed age of starting receiving deferred retirement benefits
v.year    <- 5
fasyears  <- 1  # based on policy before PEPRA
bfactor   <- 0.03
cola_assumed <- 0.02 # assumed cola rates for valuation  
EEC_rate  <- 0.12




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
grp_include <- grp_include[str_detect(grp_include , "poff")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

#wgts[wgts$grp == "sfty_classic", "wgt"] <-  0.2605
#wgts[wgts$grp == "sfty_pepra",   "wgt"] <-  0.1078
wgts[wgts$grp == "poff_classic", "wgt"] <-  0.3796
#wgts[wgts$grp == "poff_pepra_1", "wgt"] <-  0.1570 * 0.5   # 2.5%@57
#wgts[wgts$grp == "poff_pepra_2", "wgt"] <-  0.1570 * 0.5   # 2.7%@57
#wgts[wgts$grp == "chp_classic",  "wgt"] <-  0.0672
#wgts[wgts$grp == "chp_pepra",    "wgt"] <-  0.0278
# wgts;sum(wgts$wgt)

## calculate weighted average
df_qxr_tier <- 
  df_qxr_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age, yos) %>% 
  summarise(qxr = weighted.mean(qxr, wgt), .groups = "drop" ) %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Disability retirement rates

# groups included
grp_include <- df_qxd_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "poff")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

#wgts[wgts$grp == "sfty","wgt"] <- 0.2605 # + 0.1078
wgts[wgts$grp == "poff","wgt"] <- 0.3796 # + 0.1570
#wgts[wgts$grp == "chp","wgt"]  <- 0.0672 # + 0.0278
# wgts

## calculate weighted average
# Need to combine the two types of disability rates: adding the two rates

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
grp_include <- grp_include[str_detect(grp_include, "poff")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

#wgts[wgts$grp == "sfty","wgt"] <- 0.2605 # + 0.1078
wgts[wgts$grp == "poff","wgt"] <- 0.3796 # + 0.1570
#wgts[wgts$grp == "chp","wgt"]  <- 0.0672 # + 0.0278
# wgts

## calculate weighted average
df_qxt.refund_tier <- 
  df_qxt.refund_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(qxt.refund = weighted.mean(qxt.refund, wgt), 
            .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()




## Termination with vested benefits

# groups included
grp_include <- df_qxt.vest_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "poff")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

#wgts[wgts$grp == "sfty","wgt"] <- 0.2605 # + 0.1078
wgts[wgts$grp == "poff","wgt"] <- 0.3796 # + 0.1570
#wgts[wgts$grp == "chp","wgt"]  <- 0.0672 # + 0.0278
# wgts

## calculate weighted average
df_qxt.vest_tier <- 
  df_qxt.vest_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(qxt.vest = weighted.mean(qxt.vest, wgt),
            .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()


## combine two types of termination rates: adding up the two types of rate
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
  mutate(qxm.pre.nonocc = 0.1 * qxm.pre.nonocc_female + 0.9 * qxm.pre.nonocc_male,
         qxm.pre.occ    = 0.1 * qxm.pre.occ_female    + 0.9 * qxm.pre.occ_male,
         qxm.pre = qxm.pre.nonocc + qxm.pre.occ,
         grp = tier_name
         ) %>% 
  select(grp, age, qxm.pre, qxm.pre.nonocc, qxm.pre.occ)




## Post-retirement mortality, without projection

# Need to combine two types of disability mortality rates: using weighted average
#  - assume 50% of disability retirement is job-related


df_qxm.post_tier <-  
  df_qxm.post_imputed %>% 
  mutate(qxm.post         = 0.1 * qxm.post_female         + 0.9 * qxm.post_male,
         qxmd.post.nonocc = 0.1 * qxmd.post.nonocc_female + 0.9 * qxmd.post.nonocc_male,
         qxmd.post.occ    = 0.1 * qxmd.post.occ_female    + 0.9 * qxmd.post.occ_male,
         qxmd.post        = 0.5 * qxmd.post.nonocc        + 0.5 * qxmd.post.occ, 
         grp = tier_name
  ) %>% 
  select(grp, age, 
         qxm.post, qxmd.post
         # qxmd.post.nonocc, qxmd.post.occ, 
         # qxm.post_female, qxm.post_male,
         # qxmd.post.nonocc_female, qxmd.post.nonocc_male,
         # qxmd.post.occ_female,    qxmd.post.occ_male
         )


## Post-retirement mortality, with projection
 # This will be used to construct improvment table

df_qxm.post_proj_tier <-  
  df_qxm.post_proj_imputed %>% 
  mutate(qxm.post_proj         = 0.1 * qxm.post_female_proj         + 0.9 * qxm.post_male_proj,
         qxmd.post.nonocc_proj = 0.1 * qxmd.post.nonocc_female_proj + 0.9 * qxmd.post.nonocc_male_proj,
         qxmd.post.occ_proj    = 0.1 * qxmd.post.occ_female_proj    + 0.9 * qxmd.post.occ_male_proj,
         qxmd.post_proj        = 0.5 * qxmd.post.nonocc_proj        + 0.5 * qxmd.post.occ_proj,
         grp = tier_name
  ) %>% 
  select(grp, age, 
         qxm.post_proj, qxmd.post_proj
         # qxmd.post.nonocc_proj, qxmd.post.occ_proj,
         # qxm.post_female_proj, qxm.post_male_proj,
         # qxmd.post.nonocc_female_proj, qxmd.post.nonocc_male_proj,
         # qxmd.post.occ_female_proj,    qxmd.post.occ_male_proj
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

#' Service retirement:
#'   Use 50/5  3%@50 (constant 3%) for now, may want to adjust the benefit factor downward
#'   to account for the lower benefit  
#' Vesting: 5 years



# Create 2 columns for each tier
 # elig_servRet_full:  number of year of being eligible for full or greater retirement benefits
 # elig_servRet_early: number of year of being eligible for early retirement benefits; 
 #             0 after being eligible for full retirement benefits

# For safety tier with 3%@50 benefit rule, may want to set full and early retirement age the same 


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

# Improvement for post retirement mortality

# For CalPERS (PERF A), we assume linear improvement from the current rates to
# the projected rates over 15 years. 

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

# Improvement factor, based on 2017 basis
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

# assumption: inflation component of salary scale: 2.75%


# groups included
grp_include <- df_salScale.merit_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "poff")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

#wgts[wgts$grp == "sfty","wgt"] <- 0.2605 # + 0.1078
wgts[wgts$grp == "poff","wgt"] <- 0.3796 # + 0.1570
#wgts[wgts$grp == "chp","wgt"]  <- 0.0672 # + 0.0278
# wgts

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
grp_include <- grp_include[str_detect(grp_include, "poff")]


## Active members
df_n_actives_tier <- 
  df_nactives_fillin %>% 
  filter(grp %in% grp_include) %>% 
  # left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(salary   = weighted.mean(salary, nactives, na.rm = TRUE) %>% na2zero(),
            nactives = sum(nactives, na.rm= TRUE) %>% na2zero,
            .groups = "drop") %>% 
  mutate(grp = tier_name,
         age = ea + yos) %>% 
  relocate(grp) %>% 
  arrange(ea, age) %>% 
  ungroup()

# CalPERS: Check total benefit againt the AV value: payroll (AV2018 ep20-22)
# sum(df_n_actives_tier$nactives * df_n_actives_tier$salary)
# model/target: 6710663988/(2316124913 + 3522647266 + 871895121) = 99.99%


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


## Service retirees
 # For now, combine service retirees and beneficiaries

grp_include <- df_n_servRet_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "poff")]

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
# model/target:2492264817/2492264816 = 100%





## Disability retirees
 # For now, combine industrial and non-industrial disability retirees

grp_include <- df_n_disbRet_nonocc_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "poff")]

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

# CalPERS: Check total benefit againt the AV value (AV2018 ep141-145)
# (df_n_disbRet_tier$n_disbRet*df_n_disbRet_tier$benefit_disbRet) %>% sum
# model/target:773929137/ 774100780 = 99.97783%

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



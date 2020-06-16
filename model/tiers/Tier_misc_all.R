# Prepare model inputs for tier "miscAll"



#*******************************************************************************
#                                Notes ####
#*******************************************************************************


#' Inputs:
#'   - inputs/data_proc/Data_CalPERS_decrements_ES2017_imputed.RData
#'   - inputs/data_proc/Data_CalPERS_demographics_20180630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "miscAll"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   




##' What this tier includes
#'  - misc classic tier 1
#'  - misc classic tier 2
#'  - misc PEPRA tier 1
#'  - misc PEPRA tier 2
#'  - industrial classic tier 1/2
#'  - industrial classic tier 1/2 


##' Policies applied to this tier





#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data  <- "inputs/data_proc/"
tier_name <- "miscAll"




#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_CalPERS_decrements_ES2017_imputed.RData"))
load(paste0(dir_data, "Data_CalPERS_demographics_20180630_fillin.RData"))



#*******************************************************************************
#                      ## Decrements ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc_classic","wgt"] <-  0.7
wgts[wgts$grp == "misc_pepra",  "wgt"] <-  0.1
wgts[wgts$grp == "inds_classic","wgt"] <-  0.1
wgts[wgts$grp == "inds_pepra",  "wgt"] <-  0.1

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

wgts[wgts$grp == "misc_t1_female","wgt"] <-  6
wgts[wgts$grp == "misc_t1_male",  "wgt"] <-  4
wgts[wgts$grp == "misc_t2_female","wgt"] <-  0.6
wgts[wgts$grp == "misc_t2_male",  "wgt"] <-  0.4
wgts[wgts$grp == "inds",  "wgt"]         <-  2



## calculate weighted average
df_qxd_tier <- 
  df_qxd_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxd.nonocc = weighted.mean(qxd.nonocc, wgt), 
            qxd.occ    = weighted.mean(qxd.occ, wgt),
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

wgts[wgts$grp == "misc_t1",  "wgt"] <-  10
wgts[wgts$grp == "misc_t2",  "wgt"] <-  0.5
wgts[wgts$grp == "inds",     "wgt"] <-  2


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

wgts[wgts$grp == "misc_t1",  "wgt"] <-  10
wgts[wgts$grp == "misc_t2",  "wgt"] <-  0.5
wgts[wgts$grp == "inds",     "wgt"] <-  2


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


## Pre-retirement mortality
df_qxm.pre_tier <-  
  df_qxm.pre_imputed %>% 
  mutate(qxm.pre.nonocc = 0.6 * qxm.pre.nonocc_female + 0.4 * qxm.pre.nonocc_male,
         qxm.pre.occ    = 0.9 * qxm.pre.occ_female    + 0.1 * qxm.pre.occ_male,
         qxm.pre = qxm.pre.nonocc + qxm.pre.occ,
         grp = tier_name
         ) %>% 
  select(grp, age, qxm.pre, qxm.pre.nonocc, qxm.pre.occ)




## Pre-retirement mortality, without projection
df_qxm.post_tier <-  
  df_qxm.post_imputed %>% 
  mutate(qxm.post         = 0.6 * qxm.post_female + 0.4 * qxm.post_male,
         qxmd.post.nonocc = 0.6 * qxmd.post.nonocc_female + 0.4 * qxmd.post.nonocc_male,
         qxmd.post.occ    = 0.9 * qxmd.post.occ_female    + 0.1 * qxmd.post.occ_male,
         grp = tier_name
  ) %>% 
  select(grp, age, qxm.post, qxmd.post.nonocc, qxmd.post.occ)


## Pre-retirement mortality, with projection
df_qxm.post_proj_tier <-  
  df_qxm.post_proj_imputed %>% 
  mutate(qxm.post         = 0.6 * qxm.post_female_proj + 0.4 * qxm.post_male_proj,
         qxmd.post.nonocc = 0.6 * qxmd.post.nonocc_female_proj + 0.4 * qxmd.post.nonocc_male_proj,
         qxmd.post.occ    = 0.9 * qxmd.post.occ_female_proj    + 0.1 * qxmd.post.occ_male_proj,
         grp = tier_name
  ) %>% 
  select(grp, age, qxm.post, qxmd.post.nonocc, qxmd.post.occ)




# df_qxr_tier
# df_qxd_tier
# df_qxt.refund_tier
# df_qxt.vest_tier
# df_qxm.post_tier
# df_qxm.post_proj_tier




#*******************************************************************************
#                      ## Salary Scale  ####
#*******************************************************************************
df_salScale.merit_imputed


# groups included
grp_include <- df_salScale.merit_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "misc",  "wgt"] <-  10
wgts[wgts$grp == "inds",     "wgt"] <-  0.8
wgts

## calculate weighted average
df_salScale.merit_tier <- 
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

df_nactives_fillin
df_n_servRet_fillin
df_n_disbRet_occ_fillin
df_n_disbRet_nonocc_fillin
df_n_beneficiaries_fillin


## groups included 
grp_include <- df_nactives_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "misc|inds")]



## Active members
df_nactives_tier <- 
  df_nactives_fillin %>% 
  filter(grp %in% grp_include) %>% 
  # left_join(wgts, by = "grp") %>% 
  group_by(yos, ea) %>% 
  summarise(salary   = weighted.mean(salary, nactives, na.rm = TRUE),
            nactives = sum(nactives, na.rm= TRUE),
            .groups = "rowwise") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(ea, yos) %>% 
  ungroup()


## Service retirees
 # For now, combine service retirees and beneficiaries

df_n_servRet_tier <- 
  left_join(df_n_servRet_fillin,
            df_n_beneficiaries_fillin,
            by = c("AV_date", "grp", "age", "age.cell")
            ) %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_tot_servRet = sum(benefit_tot_servRet, na.rm= TRUE),
            n_servRet           = sum(n_servRet, na.rm = TRUE),
            
            benefit_tot_beneficiaries = sum(benefit_tot_beneficiaries, na.rm= TRUE),
            n_beneficiaries           = sum(n_beneficiaries, na.rm = TRUE),
            
            .groups = "rowwise") %>% 
  mutate(grp = tier_name,
         n_servRet       = n_servRet + n_beneficiaries,
         benefit_servRet = na2zero((benefit_tot_servRet + benefit_tot_beneficiaries) / n_servRet)) %>% 
  select(grp, age, n_servRet, benefit_servRet) %>% 
  arrange(age) %>% 
  ungroup()


## Disability retirees
 # For now, combine industrial and non-industrial disability retirees

df_n_disbRet_tier <- 
  left_join(df_n_disbRet_nonocc_fillin,
            df_n_disbRet_occ_fillin,
            by = c("AV_date", "grp", "age", "age.cell")
  ) %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_tot_disbRet_nonocc = sum(benefit_tot_disbRet_nonocc, na.rm= TRUE),
            n_disbRet_nonocc           = sum(n_disbRet_nonocc, na.rm = TRUE),
            
            benefit_tot_disbRet_occ = sum(benefit_tot_disbRet_occ, na.rm= TRUE),
            n_disbRet_occ           = sum(n_disbRet_occ, na.rm = TRUE),
            
            .groups = "rowwise") %>% 
  mutate(grp = tier_name,
         n_disbRet       = n_disbRet_occ + n_disbRet_nonocc,
         benefit_disbRet = na2zero((benefit_tot_disbRet_nonocc + benefit_tot_disbRet_occ) / n_disbRet)) %>% 
  select(grp, age, n_disbRet, benefit_disbRet) %>% 
  arrange(age) %>% 
  ungroup()




df_nactives_tier
df_n_servRet_tier
df_n_disbRet_tier


 



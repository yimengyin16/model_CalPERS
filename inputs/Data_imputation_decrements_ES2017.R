## Imputation of decrement tables and salary scales for CalPERS


## Inputs
#   - inputs/data_raw/Data_CalPERS_decrements_ES2017_raw.RData"


## Outputs
 #  -  tables imputed across age/ea/yos ranges, all in "tidy" format
 


##' Notes
#'  Data processing in this script must be "model agnostics"
#'  Data processing can take into account 
#'    - the structure of demographic inputs (e.g. age, yos ranges)
#'    - relevant CalPERS rules and valuation methods 


## Data frames to be processed
# df_qxr_raw,
# df_qxd_raw,
# df_qxt.refund_raw,
# df_qxt.vest_raw,
# df_qxm.pre_raw,
# df_qxm.post_raw,
# df_qxm.post_raw_proj,
# 
# df_salScale.merit_raw,


## Imputation rules
#'  - Range of age: 20-110
#'  - Range of ea:  20 - ?? 
#'  - Range of yos: 0-??
#' 
#' Ranges of ea and yos should take into account the age-yos ranges of the data for active members
#'  - In active memeber data: max age is 69, max yos is 29 
#'  - 


# Steps
#' 1. Max retirement age: based on benefit rules and retirement rate assumptions
#'    - should be 75 (retirement rate assumptions)
#' 2. Max ea and max age for actives is (max retAge - 1)
#' 3. Max yos is (max age for actives - min ea)
#' 
#' If max age in active member data is greater than the max age determined above, 
#' then we should consider adjusting the member data(e.g. merged to lower age groups)




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_inputs  <- "inputs/data_proc/"
fn_inputs   <- "Data_CalPERS_decrements_ES2017_raw.RData" 
filePath_inputs <- paste0(dir_inputs, fn_inputs)

dir_outputs <- "Inputs/data_proc/"


load(filePath_inputs)



#*******************************************************************************
#                      ## Local tools ####
#*******************************************************************************

# ## spline smoothing 
# splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
#   # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
#   # or just 2 columns, with no nonfillvar
#   # last column ALWAYS must be the value var
#   valvar<-names(df)[length(names(df))]
#   nonfillvar<-setdiff(names(df),c(fillvar,valvar))
#   f<-function(x) {
#     if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
#     spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
#     dfout<-data.frame(x=spl$x, y=spl$y)
#     names(dfout)<-c(fillvar,valvar)
#     return(dfout)
#   }
#   if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
#   return(dfl2)
# }


#*******************************************************************************
#                      ## Importing service retirement rates ####
#*******************************************************************************

# Indices:
#   - age: 50-75, by 1y, NO imputation needed  
#   - yos: 5-35,  by 5y, imputation needed

# Imputation:
#   - across yos 0-35 within each grp-age group
#   - negative values to 0
#   - yos should be extended to 55 to allow for age=75 with ea = 20, rates at yos=35 
#     will be used for all yos>35 in each age group.  


df_qxr_imputed <- 
  df_qxr_raw %>% 
  unite("grp_age", grp, age, sep = "*") %>% 
  splong("yos", fitrange = 0:55) %>% 
  group_by(grp_age) %>% 
  mutate(qxr = ifelse(qxr<=0, 0, qxr),
         qxr = ifelse(yos > 35, qxr[yos == 35], qxr)
         ) %>% 
  separate(grp_age, into = c("grp", "age"), sep = "\\*", convert = TRUE) 



#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************

# Indices:
#   - age: 20-80, by 10y, imputation needed  

# Imputation:
#   - across age 20-80 within each grp
#   - all NA values are converted to 0

df_qxd_imputed <- 
  left_join(
  df_qxd_raw %>% 
    select(-qxd.occ) %>% 
    splong("age"),
  
  df_qxd_raw %>% 
    select(-qxd.nonocc) %>% 
    mutate(qxd.occ = na2zero(qxd.occ)) %>% 
    splong("age", method = "hyman"), # can avoid negative values, but requires monotone inputs
  
  Joining, by = c("grp", "age")
  )


# # checking negative values
# (df_qxd_imputed$qxd.nonocc<0) %>% sum
# (df_qxd_imputed$qxd.occ<0) %>% sum
# 
# # plot the rates
# df_qxd_imputed %>% 
#   gather(var, value, -grp, -age) %>% 
#   qplot(x = age, y = value, color = var, geom = "line", data = .) + facet_wrap(.~grp)


#*******************************************************************************
#                      ## Importing termination rates, refund  ####
#*******************************************************************************



# row indices
#  - groups
#   - misc_t1
#   - misc_t2
#   - inds
#   - sfty
#   - poff
#   - chp
#  - yos: 0~50 by y5,  imputation needed
#  - ea: (20, 30, 40), imputation needed

# Imputation: 
#  - convert NAs to 0 (yos = 50 in safety and POFF)
#  - first impute across yos from 0-55, using value at yos ==50 for yos > 50
#  - then impute  across ea from 20-75, using value at age=40/45 for age > 40/45


df_qxt.refund_imputed <- 
  
bind_rows(
  df_qxt.refund_raw %>% 
    filter(grp != "misc_t2") %>% 
    mutate(qxt.refund = na2zero(qxt.refund)) %>% 
    unite("grp_ea", grp, ea, sep = "+") %>% 
    splong("yos", fitrange = 0:55, method = "hyman") %>% 
    group_by(grp_ea) %>% 
    mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
           qxt.refund = ifelse(yos > 50, qxt.refund[yos == 50], qxt.refund)
           ), 
  
  df_qxt.refund_raw %>% 
    filter(grp == "misc_t2") %>% 
    mutate(qxt.refund = na2zero(qxt.refund)) %>% 
    unite("grp_ea", grp, ea, sep = "+") %>% 
    splong("yos", fitrange = 0:55) %>% 
    group_by(grp_ea) %>% 
    mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
           qxt.refund = ifelse(yos > 50, qxt.refund[yos == 50], qxt.refund)
    )
)


df_qxt.refund_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:75, method = "hyman") %>% # use hyman method wherever possible
  # filter(qxt.refund<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
         qxt.refund = ifelse(ea > 45, qxt.refund[ea == 45], qxt.refund)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)




#*******************************************************************************
#                      ## Importing termination rates, vested  ####
#*******************************************************************************

# Row indices
#  - groups
#   - misc_t1
#   - misc_t2
#   - inds
#   - sfty
#   - poff
#   - chp
#  - yos: 0~50 by y5,  imputation needed
#  - ea: (20, 30, 40), imputation needed

# Imputation: 
#  - convert NAs to 0 
#  - first impute across yos from 5-55, using value at yos ==50 for yos > 50, 0 if yos < 5
#  - then impute  across ea from 20-75, using value at age=40/45 for age > 40/45

# df_qxt.vest_raw

df_qxt.vest_imputed <- 
  bind_rows(
    df_qxt.vest_raw %>% 
      filter(grp != "misc_t2", yos >=5) %>% 
      mutate(qxt.vest = na2zero(qxt.vest)) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 5:55, method = "hyman") %>% 
      group_by(grp_ea) %>% 
      mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
             qxt.vest = ifelse(yos > 50, qxt.vest[yos == 50], qxt.vest)
      ), 
    
    df_qxt.vest_raw %>% 
      filter(grp == "misc_t2", yos >=10) %>% 
      mutate(qxt.vest = na2zero(qxt.vest)) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 10:55, method = "hyman") %>% 
      group_by(grp_ea) %>% 
      mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
             qxt.vest = ifelse(yos > 50, qxt.vest[yos == 50], qxt.vest)
      )
  )

df_qxt.vest_imputed <- 
  left_join(
    expand_grid(grp_ea = df_qxt.vest_imputed$grp_ea %>% unique, yos = 0:55),
    df_qxt.vest_imputed,
    by = c("grp_ea", "yos")
  ) %>% 
  mutate(qxt.vest = ifelse(yos < 5, 0, qxt.vest),
         qxt.vest = ifelse(yos < 10 & str_detect(grp_ea, "misc_t2"), 0, qxt.vest)
         )

# df_qxt.vest_imputed


df_qxt.vest_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:75, method = "hyman") %>% # use hyman method wherever possible
  # filter(qxt.vest<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
         qxt.vest = ifelse(ea > 45, qxt.vest[ea == 45], qxt.vest)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)




#*******************************************************************************
#                      ## Importing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-80 by 5, imputation needed
#' Labels in col names:
#'  - pre: pre-retirement
#'  - occ/nonocc: occupational and non-occupational 
#'                (industrial and non-industrial related in CalPERS terms)
#'  - female/male

#' Imputation
#'  - impute across age 20-80

df_qxm.pre_imputed <-  
  df_qxm.pre_raw %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)

# df_qxm.pre_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)


#*******************************************************************************
#                      ## Importing post-retirement mortality  ####
#*******************************************************************************

# Indices:
#   - age: 20-110, by 5y, imputation needed  


# Imputation:
#   - across yos 20-110 within each variable


df_qxm.post_imputed <- 
  df_qxm.post_raw %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)



df_qxm.post_proj_imputed <- 
  df_qxm.post_raw_proj %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)


# df_qxm.post_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)
# 
# df_qxm.post_proj_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)



#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************

# Row indices
#  - groups
#   - misc
#   - inds
#   - sfty
#   - poff
#   - chp
#  - yos: 0, 3, 5~50 by y5,  imputation needed
#  - ea: (20, 30, 40), imputation needed

# Imputation: 
#  - convert NAs to 0 
#  - first impute across yos from 5-55, using value at yos ==30 for yos > 30
#  - then impute  across ea from 20-75, using value at age=40/45 for age > 40/45


# df_salScale.merit_raw


df_salScale.merit_imputed <- 
  bind_rows(
    df_salScale.merit_raw %>% 
      filter(!grp %in% c("poff", "chp")) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 0:55, method = "hyman") %>% 
      group_by(grp_ea) %>% 
      mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
             salScale.merit = ifelse(yos > 30, salScale.merit[yos == 30], salScale.merit)
      ), 
    
    df_salScale.merit_raw %>% 
      filter(grp %in% c("poff", "chp")) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 0:55) %>% 
      group_by(grp_ea) %>% 
      mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
             salScale.merit = ifelse(yos > 30, salScale.merit[yos == 30], salScale.merit)
            )
)
  
# df_salScale.merit_imputed


df_salScale.merit_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:75) %>% # use hyman method wherever possible
  # filter(salScale.merit<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
         salScale.merit = ifelse(ea > 45, salScale.merit[ea == 45], salScale.merit)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)









#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************

# df_qxr_imputed
# df_qxd_imputed
# df_qxt.refund_imputed
# df_qxt.vest_imputedp
# df_qxm.pre_imputed
# df_qxm.post_imputed
# df_qxm.post_proj_imputed
# 
# df_salScale.merit_imputed


save(
  df_qxr_imputed,
  df_qxd_imputed,
  df_qxt.refund_imputed,
  df_qxt.vest_imputed,
  df_qxm.pre_imputed,
  df_qxm.post_imputed,
  df_qxm.post_proj_imputed,

  df_salScale.merit_imputed,

	file = paste0(dir_outputs, "Data_CalPERS_decrements_ES2017_imputed.RData")
)





## Loading decrement tables salary scales from the 2017 experience study for CalPERS


## Inputs
#   - inputs/data_raw/Data_CalPERS_decrements_ES2017.xlsx"


## Outputs
 #  - All tables in "tidy" format  
 







#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
# source("libraries.R")




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_dataRaw  <- "inputs/data_raw/"
fn_dataRaw   <- "Data_CalPERS_Decrements_ES2017.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


names_sheet <- excel_sheets(filePath_dataRaw)




#*******************************************************************************
#                      ## Importing service retirement rates ####
#*******************************************************************************

# Local helper function
get_servRetRates <- function(sheetName, fileName = filePath_dataRaw){
  
  ls <- read_excel_range(fileName, sheetName) 
  
  var_name <- ls$tblInfo$var_name # ls$tblInfo[ls$tblInfo$var == "var_name", "value"][[1]] 
  grp_name <- ls$tblInfo$grp_name # ls$tblInfo[ls$tblInfo$var == "grp_name", "value"][[1]] 
  
  
  ls$df %>% 
    gather(yos, value, -age) %>% 
    rename(!!var_name := value) %>% 
    mutate(grp = grp_name) %>% 
    select(grp, everything())
}



sheetNames_serRet <- names_sheet[str_detect(names_sheet, "servRet")]

df_qxr_raw <- map(sheetNames_serRet, get_servRetRates) %>% 
  bind_rows() %>% 
  mutate(yos = as.numeric(yos))

df_qxr_raw


#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************


df_qxd_raw <- 
left_join(
  read_excel_range(filePath_dataRaw, "disbRet_nonInds")$df %>%
    gather(age, qxd.nonocc, -grp),

  read_excel_range(filePath_dataRaw, "disbRet_inds")$df %>%
    gather(age, qxd.occ, -grp)
) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(grp, age)

df_qxd_raw 



#*******************************************************************************
#                      ## Importing termination rates ####
#*******************************************************************************

# row indices
#  - groups
#   - misc_t1
#   - misc_t2
#   - inds
#   - sfty
#   - poff
#   - chp
#  - yos: 0~50 by 5
#  - ea: 20, 30, 40



df_qxt.refund_raw <-
  
  bind_rows(
    read_excel_range(filePath_dataRaw, "defrRet_refund_misc_t1")$df %>%
      gather(ea, qxt.refund,-yos, convert = TRUE) %>%
      mutate(grp = "misc_t1") %>%
      relocate(grp),
    
    read_excel_range(filePath_dataRaw, "defrRet_refund_misc_t2")$df %>%
      gather(ea, qxt.refund,-yos, convert = TRUE) %>%
      mutate(grp = "misc_t2") %>%
      relocate(grp),
    
    left_join(
      expand_grid(
        grp = c("inds", "sfty", "poff", "chp"),
        ea  = c(20, 30, 40),
        yos = seq(0, 50, 5)
        ),
      
      read_excel_range(filePath_dataRaw, "defrRet_refund_other")$df %>%
        gather(grp, qxt.refund,-yos) %>%
        mutate(qxt.refund = as.numeric(qxt.refund)) %>%
        relocate(grp),
      
      by = c("grp", "yos")
    )
  )



df_qxt.vest_raw <-
  
  bind_rows(
    read_excel_range(filePath_dataRaw, "defrRet_vest_misc_t1")$df %>%
      gather(ea, qxt.vest,-yos, convert = TRUE) %>%
      mutate(grp = "misc_t1",
             qxt.vest = as.numeric(qxt.vest)) %>%
      relocate(grp),
    
    read_excel_range(filePath_dataRaw, "defrRet_vest_misc_t2")$df %>%
      gather(ea, qxt.vest,-yos, convert = TRUE) %>%
      mutate(grp = "misc_t2",
             qxt.vest = as.numeric(qxt.vest)) %>%
      relocate(grp),
    
    read_excel_range(filePath_dataRaw, "defrRet_vest_inds")$df %>%
      gather(ea, qxt.vest,-yos, convert = TRUE) %>%
      mutate(grp = "inds",
             qxt.vest = as.numeric(qxt.vest)) %>%
      relocate(grp),
    
    left_join(
      expand_grid(
        grp = c("sfty", "poff", "chp"),
        ea  = c(20, 30, 40),
        yos = seq(0, 50, 5)
      ),
      
      read_excel_range(filePath_dataRaw, "defrRet_vest_other")$df %>%
        gather(grp, qxt.vest,-yos) %>%
        mutate(qxt.vest = as.numeric(qxt.vest)) %>%
        relocate(grp),
      
      by = c("grp", "yos")
    )
  )





#*******************************************************************************
#                      ## Importing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-80 by 5
#' Labels in col names:
#'  - pre: pre-retirement
#'  - occ/nonocc: occupational and non-occupational 
#'                (industrial and non-industrial related in CalPERS terms)
#'  - female/male


df_qxm.pre_raw <-
  left_join(
    read_excel_range(filePath_dataRaw, "mortality_preRet_nonInds")$df,
    read_excel_range(filePath_dataRaw, "mortality_preRet_inds")$df,
    by = "age"
  )



#*******************************************************************************
#                      ## Importing post-retirement mortality  ####
#*******************************************************************************


df_qxm.post_raw_proj <- 
  read_excel_range(filePath_dataRaw, "mortality_postRet_15yProj")$df

df_qxm.post_raw <- 
  read_excel_range(filePath_dataRaw, "mortality_postRet_noProj")$df





#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************


df_salScale.merit_raw <-
  
  bind_rows(
    read_excel_range(filePath_dataRaw, "salScale_merit_misc")$df %>%
      gather(ea, salScale.merit, -yos, convert = TRUE) %>%
      mutate(grp = "misc") %>%
      relocate(grp),
    
    
    read_excel_range(filePath_dataRaw, "salScale_merit_inds")$df %>%
      gather(ea, salScale.merit, -yos, convert = TRUE) %>%
      mutate(grp = "inds") %>%
      relocate(grp),
    
    
    left_join(
      expand_grid(
        grp = c("sfty", "poff", "chp"),
        ea  = c(20, 30, 40),
        yos = c(0, 3, seq(5, 30, 5))
      ),
      
      read_excel_range(filePath_dataRaw, "salScale_merit_other")$df %>%
        gather(grp, salScale.merit, -yos) %>%
        relocate(grp),
      
      by = c("grp", "yos")
    )
  )

df_salScale.merit_raw


#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************



df_qxr_raw
df_qxd_raw
df_qxt.refund_raw
df_qxt.vest_raw
df_qxm.pre_raw
df_qxm.post_raw
df_qxm.post_raw_proj

df_salScale.merit_raw


save(
  df_qxr_raw,
  df_qxd_raw,
  df_qxt.refund_raw,
  df_qxt.vest_raw,
  df_qxm.pre_raw,
  df_qxm.post_raw,
  df_qxm.post_raw_proj,
  
  df_salScale.merit_raw,
		 
	file = paste0(dir_dataOut, "Data_CalPERS_decrements_ES2017_raw.RData")
)





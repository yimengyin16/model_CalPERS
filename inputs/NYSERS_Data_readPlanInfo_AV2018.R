# Load the following plan information of NYCTRS:
 # Schedule of amortization payments for existing UAALs 
 # Schedule of recognizing unrecognized investment gains/losses due to asset smoothing. 
 # (Tier specific information)





#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_dataRaw  <- "Inputs/Data_raw/"
fn_dataRaw   <- "Data_NYSERS_planInfo_AV2018.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/"



#*******************************************************************************
#                      ## Initial amortization payments ####
#*******************************************************************************
init_amort_raw <- read_xlsx(filePath_dataRaw, sheet = "Init_amort", range = "C7:M8")
init_amort_raw



#*******************************************************************************
#                      ## Unrecognized investment gains/losses  ####
#*******************************************************************************


init_unrecReturns.unadj <- read_xlsx(filePath_dataRaw, 
																		 sheet = "Init_unrecReturn", range = "C6:D10")
init_unrecReturns.unadj


#*******************************************************************************
#                      ## Save Data ####
#*******************************************************************************

save(init_amort_raw,
		 init_unrecReturns.unadj,
		 file = paste0(dir_dataOut, "Data_NYSERS_planInfo_AV2018.RData"))

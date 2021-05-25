# Notes:
## - For the branch "dev_gains" 



# 0. Packages and functions ----
source("libraries.R")
source("functions.R")


# 1. Preparing inputs ----

## 1.1 Loading raw data
source("inputs/Data_readDemographics_20180630.R")
source("inputs/Data_readDecrements_ES2017.R")
source("inputs/Data_readPlanInfo_AV2018.R")

## 1.2 Imputation and extrapolation
source("inputs/Data_imputation_demographics_20180630.R")
source("inputs/Data_imputation_decrements_ES2017.R")


# 2. Constructing tiers ----

## misc and industrial members
source("model/tiers/Tier_misc_classic.R")
source("model/tiers/Tier_misc_pepra.R")

## safety, POFF, and CHP members
source("model/tiers/Tier_sfty_classic.R")
source("model/tiers/Tier_sfty_pepra.R")

## POFF only (not essential, just for an example in the final report)
source("model/tiers/Tier_poff_classic.R")
source("model/tiers/Tier_poff_pepra.R")


# 3. Actuarial valuation ----
## Note:
##  - The lastest version is in the folder "/valuation2", which should replace "/valuation"
##    when finalizing the program. 
##  - You will see multiple versions for some files, only the latest versions are
##    are used. (e.g. version (3) for "model_val_indivLiab_flexbf")

## - You can control which valuations to run by changing the "include" column of in the 
##   "param_val" tab of "/model/RunControl.xlsx". It may take a long time to run all valuations (rows). 
##   For a text run, you can only run the valuations needed in the simulations you want to run. 

##   You can choose which simulations to run in the "param_sim" tab by changing the "include" column, 
##   and the valuation needed is specified in the "val_name" column. 

##   For example, if you only need to run the simulation "misc25_baseline", then
##   only the valuation "misc2t_bf100_cola2" is needed. 

source("model/valuation2/model_val_runControl.R")
# The result is saved in /model/valuation2/outputs_val


# 4. Simulation
## If change the simulation horizon (nyear in tab "GlobalPrams"), make sure the 
## values in the "period" column for each return scenario in the "return" tab add 
## up to the simulation horizon (currently 50 years). (I wanted to make this adjustment
## automatic but have not implemented yet.)

source("model/simulation/model_sim_runControl.R")
# The result is saved in /model/simulation/outputs_sim



# 5. Analysis

# "analysis/Results_PERFState_v2(v6).Rmd" produces an R markdown notebook that shows some summary results,
#  and produces tables for the final report. 

# I suggest running the rmd file chunk by chunk.

# We have made the notebook an webpage using "Github Pages" for easier sharing. 
# Here is the link: 
# https://yimengyin16.github.io/model_CalPERS/Results_PERFState.nb.html

 











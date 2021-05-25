# Prepare model inputs for tier "misc_classic"



#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
source("libraries.R")


#*******************************************************************************
#              ## tiers with both member types: classic and pepra ####
#*******************************************************************************
source("model/tiers/Tier_miscAll.R")
source("model/tiers/Tier_sftyAll.R")


#*******************************************************************************
#              ## tiers with only one member type  ####
#*******************************************************************************
source("model/tiers/Tier_misc_classic.R")
source("model/tiers/Tier_misc_pepra.R")

source("model/tiers/Tier_sfty_classic.R")
source("model/tiers/Tier_sfty_pepra.R")


source("model/tiers/Tier_poff_classic.R")
source("model/tiers/Tier_poff_pepra.R")

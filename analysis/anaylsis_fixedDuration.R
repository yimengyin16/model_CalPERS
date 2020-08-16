# Comparing AL under 7% and 5% discout rate
# 
source("libraries.R")


dir_vals <- "model/valuation/outputs_val/"

ls_valNames <- c(
  
  "miscAll_bf100_cola2",
  "miscAll_bf100_cola2_DR5",
  
  "sftyAll_bf100_cola2",
  "sftyAll_bf100_cola2_DR5",
  
  "miscAll_bf50_cola1",
  "miscAll_bf50_cola1_DR5",
  
  "sftyAll_bf50_cola1",
  "sftyAll_bf50_cola1_DR5"
)


fn <- function(valName){
  # valName <- ls_valNames[1]
  x <- readRDS(paste0(dir_vals, "val_", valName, ".rds"))
  
  x$aggLiab[[1]]$active %>% 
    as.data.frame %>% 
    mutate(AL_actives = ALx.servRet.laca + ALx.defrRet + ALx.death + ALx.disbRet) %>% 
    select(year, AL_actives) %>% 
    
    left_join(x$aggLiab[[1]]$servRet.la %>% 
                as.data.frame %>% 
                select(year, ALx.servRet.la)) %>% 
    
    left_join(x$aggLiab[[1]]$defrRet %>% 
                as.data.frame %>% 
                select(year, ALx.defrRet)) %>% 
    
    left_join(x$aggLiab[[1]]$disbRet %>% 
                as.data.frame %>% 
                select(year, ALx.disbRet)) %>% 
    
    left_join(x$aggLiab[[1]]$death %>% 
                as.data.frame %>% 
                select(year, ALx.death)) %>% 
    
    mutate(AL_tot = AL_actives + ALx.servRet.la + ALx.defrRet + ALx.disbRet + ALx.death,
           valName = valName
    ) %>% 
    select(valName, year, AL_tot)
}

df_vals <- 
  map(ls_valNames, ~ fn(.x)) %>% 
  bind_rows()


## miscAll, original policy
df_vals %>% 
  filter(valName %in% ls_valNames[1:2]) %>% 
  mutate(DR = ifelse(str_detect(valName, "DR5"), "dr5", "dr7"),
         valName = str_remove(valName, "_DR5")
         ) %>% 
  spread(DR, AL_tot) %>% 
  mutate(ratio5to7 = dr5/dr7) %>% 
  filter(year %in% c(2018, 2028, 2048))


## miscAll, low benefit
df_vals %>% 
  filter(valName %in% ls_valNames[5:6]) %>% 
  mutate(DR = ifelse(str_detect(valName, "DR5"), "dr5", "dr7"),
         valName = str_remove(valName, "_DR5")
  ) %>% 
  spread(DR, AL_tot) %>% 
  mutate(ratio5to7 = dr5/dr7) %>% 
  filter(year %in% c(2018, 2028, 2048))

  
## sftyAll, original policy
df_vals %>% 
  filter(valName %in% ls_valNames[3:4]) %>% 
  mutate(DR = ifelse(str_detect(valName, "DR5"), "dr5", "dr7"),
         valName = str_remove(valName, "_DR5")) %>% 
  spread(DR, AL_tot) %>% 
  mutate(ratio5to7 = dr5/dr7) %>% 
  filter(year %in% c(2018, 2028, 2048))


## sftyAll, low benefit
df_vals %>% 
  filter(valName %in% ls_valNames[7:8]) %>% 
  mutate(DR = ifelse(str_detect(valName, "DR5"), "dr5", "dr7"),
         valName = str_remove(valName, "_DR5")) %>% 
  spread(DR, AL_tot) %>% 
  mutate(ratio5to7 = dr5/dr7) %>% 
  filter(year %in% c(2018, 2028, 2048))


# Durations largely fall in the range of 1.20 ~ 1.27
# Safety plans have slightly higher duration
# Durations tend to decrease slighly over time

# suggested duration:
 # Misc:   1.22
 # Safety: 1.25


(1.243433 + 1.234818 + 1.216076 +1.201740)/4

(1.274248 +1.254451+  1.243549 + 1.221626)/4



















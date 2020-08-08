

dir_sims <- "model/simulation/outputs_sim/"

ls_simNames <- 
  c("twoTiers_baseline",
    "twoTiers_bf1_lowERC",
    "twoTiers_bf1_highERC",

    "twoTiers_cola_lowERC",
    "twoTiers_cola_highERC2",

    "twoTiers_bf1&cola_lowERC",
    "twoTiers_bf1&cola_highERC2"
    )

df_results <- 
  map(ls_simNames, ~ readRDS(paste0(dir_sims, "sim_", .x, ".rds"))$results) %>% 
  bind_rows() %>% 
  mutate(NC.ER_PR = 100*(NC - EEC)/PR ) %>% 
  mutate(sim_name = factor(sim_name, levels = ls_simNames))


vars_report <- c("sim_name","sim", "year", "AL", "UAAL", "FR_MA", "ERC", "ERC_PR", "NC_PR", "NC.ER_PR", "SC_PR")

## Deterministic run, baseline (assumed return achieved every year)
df_memo_det1 <- 
  df_results %>% 
  filter(sim == 0, year <= 2030) %>%   #%in% c(2018, 2025, 2030)) %>% 
  select(any_of(vars_report)) %>% 
  arrange(year, sim_name) %>% 
  #select(sim_name, year, AL,UAAL, FR_MA, ERC,  ERC_PR, NC_PR, NC.ER_PR, SC_PR ) %>% 
  mutate(across(!contains("_") & !year & !sim, ~ .x / 1e9))

filter(df_memo_det1, year == 2018) %>% kable(digits = 1)
filter(df_memo_det1, year == 2028) %>% kable(digits = 1)



## Deterministic run, asset shock  
df_memo_det2 <- 
  df_results %>% 
  filter(sim == -2, year<= 2030) %>%  # %in% c(2018, 2025, 2030)) %>% 
  select(any_of(vars_report)) %>% 
  arrange(year, sim_name) %>% 
  #select(sim_name, year, AL,UAAL, FR_MA, ERC,  ERC_PR, NC_PR, NC.ER_PR, SC_PR ) %>% 
  mutate(across(!contains("_") & !year & !sim, ~ .x / 1e9))

filter(df_memo_det2, year == 2018) %>% kable(digits = 1)
filter(df_memo_det2, year == 2028) %>% kable(digits = 1)






## stochastic runs

# order geometric return up to 2028, show variables at various percentiles 

nsim <-  500

df_order <- 
df_results %>% 
  filter(sim > 0, year <= 2030, sim_name == "twoTiers_baseline") %>% 
  group_by(sim) %>% 
  summarise(geoReturn = get_geoReturn(i.r)) %>% 
  arrange(geoReturn) %>% 
  mutate(order_geoReturn = 100*(1:n())/nsim )

  
df_stch <- 
  df_results %>% 
  filter(sim > 0)%>% 
  left_join(df_order, by = "sim")


df_memo_stch <- 
  df_stch %>% 
  filter(
         # year %in% c(2018, 2025, 2030), 
         order_geoReturn %in% c(90, 75, 50, 25, 10)) %>% 
  arrange(year, desc(order_geoReturn), sim_name) %>% 
  select(sim_name, sim, year, order_geoReturn, AL, UAAL, FR_MA, ERC, ERC_PR) %>% 
  mutate(across(!contains("_") & !year &!sim, ~ .x / 1e9)) %>% 
  relocate(order_geoReturn, .after = year)
  #kable(digits = 1)
df_memo_stch

df_memo_stch %>% filter(order_geoReturn == 75, year == 2030) %>% kable(digits = 1)
df_memo_stch %>% filter(order_geoReturn == 50, year == 2030) %>% kable(digits = 1)
df_memo_stch %>% filter(order_geoReturn == 25, year == 2030) %>% kable(digits = 1)
df_memo_stch %>% filter(order_geoReturn == 10, year == 2030) %>% kable(digits = 1)



save(df_memo_det1, df_memo_det2, df_memo_stch, 
     file = "analysis/outputs_analysis/dfs_memo2.RData")


xlsx::write.xlsx2(df_memo_det1 %>% filter(year %in% c(2018, 2025, 2028, 2030)),
                  file = "analysis/outputs_analysis/PERFA_memo2.xlsx", 
                  sheetName = "detRun_returnAchieved" 
                  )

xlsx::write.xlsx2(df_memo_det2 %>% filter(year %in% c(2018, 2025, 2028, 2030)),
                  file = "analysis/outputs_analysis/PERFA_memo2.xlsx", 
                  sheetName = "detRun_assetShock",
                  append = TRUE
                  )

xlsx::write.xlsx2(df_memo_stch %>% filter(year %in% c(2025)),
                  file = "analysis/outputs_analysis/PERFA_memo2.xlsx", 
                  sheetName = "stchRun_2025",
                  append = TRUE
                  )


xlsx::write.xlsx2(df_memo_stch %>% filter(year %in% c(2028)),
                  file = "analysis/outputs_analysis/PERFA_memo2.xlsx", 
                  sheetName = "stchRun_2028",
                  append = TRUE
)

xlsx::write.xlsx2(df_memo_stch %>% filter(year %in% c(2030)),
                  file = "analysis/outputs_analysis/PERFA_memo2.xlsx", 
                  sheetName = "stchRun_2030",
                  append = TRUE
                  )













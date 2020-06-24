

dir_sims <- "model/simulation/outputs_sim/"

ls_simNames <- 
  c("twoTiers_baseline",
    "twoTiers_bf1_fast",
    "twoTiers_bf1_slow"

    # "twoTiers_cola_fast",
    # "twoTiers_cola_slow",
    # 
    # "twoTiers_bf1&cola_fast",
    # "twoTiers_bf1&cola_slow"
    )


map(ls_simNames, ~ readRDS(paste0(dir_sims, "sim_", .x, ".rds"))$results)


# df_results <- 
# bind_rows(
# readRDS(paste0(dir_sims, "sim_Dev_bf2.rds"))$results,
# readRDS(paste0(dir_sims, "sim_Dev_bf1.rds"))$results,
# readRDS(paste0(dir_sims, "sim_Dev_cola.rds"))$results
# )



## Deterministic run, baseline (assumed return achieved every year)

# year 1
df_results %>% 
  filter(sim == 0, year %in% c(2018)) %>% 
  select(sim_name, AL, MA, FR_MA, UAAL, ERC, ERC_PR, NC_PR, AL.active, AL.nonactive,SC, AL.servRet)


# year 10
df_results %>% 
  filter(sim == 0, year %in% c(2027)) %>% 
  select(sim_name, AL, FR_MA, UAAL, ERC, ERC_PR, NC_PR)



## Deterministic run, asset shock
# year 1
df_results %>% 
  filter(sim == -2, year %in% c(2018)) %>% 
  select(sim_name, AL, FR_MA, UAAL, ERC, ERC_PR, NC_PR, AL.active)


# year 10
df_results %>% 
  filter(sim == -2, year %in% c(2027)) %>% 
  select(sim_name, AL, FR_MA, UAAL, ERC, ERC_PR, NC_PR)







source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "02_load_data.R"))




ord_mod <- brm(as.integer(rating) ~ kind*member + (1 | prolific_id) + 
                 (1 | spec),
               data = rating_data,
               family = cumulative(),
               cores = 4)

ord_mod %>% 
  write_rds(here("models", "ord_mod.rds"))



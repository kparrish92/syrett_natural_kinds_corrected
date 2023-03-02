source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "02_load_data.R"))


b2 <- brm(response_corrected ~ kind + (1 | prolific_id) + (1 | spec), 
          data=three_choice_data,
          family="categorical")

b2 %>% 
  write_rds(here("models", "multinom_mod.rds"))
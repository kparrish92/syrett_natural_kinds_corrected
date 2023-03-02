source(here::here("scripts", "00_libs.R"))


data_4_senses_raw = read.csv(here("data", "4_faultless disagreement_data.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) 


colnames(data_4_senses_raw)[1] <- "Q"  

data_4_senses = data_4_senses_raw %>% 
  pivot_longer(c(3:52), names_to = "prolific_id", values_to = "response") %>% 
  rename(kind = 1, spec = 2) %>% 
  filter(!is.na(response_corrected)) %>% 
  mutate(source = "four_choice")

data_4_senses %>% 
  write.csv(here("data", "tidy", "four_choice_data.csv"))

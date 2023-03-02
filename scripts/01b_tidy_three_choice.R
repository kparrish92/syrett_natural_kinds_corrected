source(here::here("scripts", "00_libs.R"))


data_3_senses_raw = read.csv(here("data", "3_senses_data.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) 


data_5_senses_raw = read.csv(here("data", 
                                  "5_faultless_disagreement_senses_data.csv"), 
                             header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) 

colnames(data_3_senses_raw)[1] <- "Q"  

colnames(data_5_senses_raw)[1] <- "Q"  

data_3_senses = data_3_senses_raw %>% 
  pivot_longer(c(3:50), names_to = "prolific_id", values_to = "response") %>% 
  rename(kind = 1, spec = 2) %>% 
  mutate(response_corrected = case_when(
    response == "Is not" ~ "is_not",
    response == "Is" ~ "is",
    response == "Both" ~ "both",
    response == "Both" ~ "Both "
  )) %>% 
  filter(!is.na(response_corrected)) %>% 
  mutate(source = "senses")


data_5_senses = data_5_senses_raw %>% 
  pivot_longer(c(3:53), names_to = "prolific_id", values_to = "response") %>% 
  rename(kind = 1, spec = 2) %>% 
  mutate(response_corrected = case_when(
    response == "Is not" ~ "is_not",
    response == "Is" ~ "is",
    response == "Both" ~ "both",
    response == "Both" ~ "Both "
  )) %>% 
  filter(!is.na(response_corrected)) %>% 
  mutate(source = "faultless")


three_choice_data = rbind(data_3_senses, data_5_senses) %>% 
  write.csv(here("data", "tidy", "three_choice_data.csv"))

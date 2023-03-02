source(here::here("scripts", "00_libs.R"))

rating_data_mem = read.csv(here("data", "1_baseline_member_rating_data.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rename(kind = 1, spec = 2) %>% 
  pivot_longer(c(3:52), names_to = "prolific_id", values_to = "rating") %>% 
  mutate(prompt_type = "member")

rating_data_non_mem = read.csv(here("data", "2_baseline_nonmember_rating_data.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rename(kind = 1, spec = 2) %>% 
  pivot_longer(c(3:49), names_to = "prolific_id", values_to = "rating") %>% 
  mutate(prompt_type = "non_member")

rating_data = rbind(rating_data_mem, rating_data_non_mem) %>% 
  write.csv(here("data", "tidy", "rating_data_tidy.csv"))


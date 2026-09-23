## Include cross-sectional data only
library(here)
library(tidyverse)

# Load files
Non_Target_Responses <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>%
  filter(Task == "Production",
         Session %in% c("C", "1"),
         !is.na(Non_Target_Response_Type)) %>%
  filter(Months_at_Start >= 84) %>% 
  group_by(Speaker_Group) %>% 
  count(Non_Target_Response_Type, sort = TRUE) %>% 
  mutate(Percentage = round(n / sum(n) * 100, digits = 1)) %>% 
  bind_rows(summarise(., 
                      Non_Target_Response_Type = "Total", 
                      n = sum(n), 
                      Percentage = 100)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Speaker_Group, values_from = c(n, Percentage), names_vary = "slowest") %>% 
  write_csv(here("Manuscripts", "DOM Manuscript (Applied Psycholinguistics)", "Data Files", "Non-Target Response Counts.csv"))


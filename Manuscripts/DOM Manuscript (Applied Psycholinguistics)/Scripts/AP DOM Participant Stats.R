library(tidyverse)
library(here)


# Load data
## Master file
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  filter(Item == "Prod-01",
         Session %in% c("C", "1"),
         Months_at_Start >= 84) %>% 
  distinct(across(-Non_Target_Response_Type), .keep_all = TRUE) %>% 
  mutate(Years_at_Testing = Months_at_Testing/12)


## Count L2L
L2L <- Master %>% 
  filter(Speaker_Group == "L2L")

unique(L2L$Part_ID)


## Count HS
HS <- Master %>% 
  filter(Speaker_Group == "HS")

unique(HS$Part_ID)


## Count MLs
ML <- Master %>% 
  filter(Speaker_Group == "ML")

unique(ML$Part_ID)


# Calculate participant age
Age <- Master %>% 
  group_by(Speaker_Group) %>% 
  summarize(Mean = mean(Years_at_Testing, na.rm = TRUE),
            SD = sd(Years_at_Testing, na.rm = TRUE),
            Max = max(Years_at_Testing, na.rm = TRUE),
            Min = min(Years_at_Testing, na.rm = TRUE))

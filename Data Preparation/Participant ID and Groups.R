library(tidyverse)
library(here)


# Load dataframes
L2_2_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Second Clitic Production.csv"))
HS_2_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Second Clitic Production.csv"))
L2_45_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic Production.csv"))
HS_45_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic Production.csv"))
L2_78_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic Production.csv"))
HS_78_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic Production.csv"))


# Observe lists
unique(L2_2_Prod$Part_ID)
unique(L2_45_Prod$Part_ID)
unique(L2_78_Prod$Part_ID)
unique(HS_2_Prod$Part_ID)
unique(HS_45_Prod$Part_ID)
unique(HS_78_Prod$Part_ID)


# Master list
Master <- rbind(L2_2_Prod, HS_2_Prod, L2_45_Prod, HS_45_Prod, L2_78_Prod, HS_78_Prod)

unique(Master$Part_ID)


# Generate participant age statistics
Age_Summary <- Master %>% 
  filter(!is.na(Months)) %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Months), SD = sd(Months))


# Generate participant Spanish percentages
Ratio_Summary <- Master %>% 
  filter(!is.na(SPA_Pct)) %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(SPA_Pct), SD = sd(SPA_Pct))


library(tidyverse)
library(here)

options(scipen = 99)


# Load data
## DOM
HS_2_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Second DOM Production.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "HS")

HS_45_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_2_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Second DOM Production.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "L2L")

L2_45_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


## Subjunctive
HS_45_Subj <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_Subj <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_45_Subj <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_Subj <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


## Gender
HS_2_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Second Clitic Production.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "HS")

HS_45_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_2_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Second Clitic Production.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "L2L")

L2_45_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic Production.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic Production.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


# Join dataframes
## DOM
DOM <- rbind(HS_2_DOM, HS_45_DOM, HS_78_DOM, L2_2_DOM, L2_45_DOM, L2_78_DOM) %>% 
  mutate(Structure = "DOM",
         Verb_Freq_Std = "Placeholder",
         Interface = "Yes",
         Timing = "Early",
         Months_at_Start_Std = (Months_at_Start - mean(Months_at_Start))/sd(Months_at_Start)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined DOM Production Data.csv"))

Subjunctive <- rbind(HS_45_Subj, HS_78_Subj, L2_45_Subj, L2_78_Subj) %>% 
  mutate(Structure = "Subjunctive",
         Interface = "No",
         Timing = "Late",
         Months_at_Start_Std = (Months_at_Start - mean(Months_at_Start))/sd(Months_at_Start)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Subjunctive Production Data.csv"))

Clitics <- rbind(HS_2_Gender, HS_45_Gender, HS_78_Gender, L2_2_Gender, L2_45_Gender, L2_78_Gender) %>% 
  mutate(Accuracy = Accuracy_Clit_Gen,
         Structure = "Clitics",
         Verb_Freq_Std = "Placeholder",
         Interface = "Yes",
         Timing = "Late",
         Months_at_Start_Std = (Months_at_Start - mean(Months_at_Start))/sd(Months_at_Start)) %>% 
  filter(!is.na(Accuracy_Clit_Gen)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Clitics Production Data.csv"))

Articles <- rbind(HS_2_Gender, HS_45_Gender, HS_78_Gender, L2_2_Gender, L2_45_Gender, L2_78_Gender) %>% 
  mutate(Accuracy = Accuracy_DP_Gen,
         Structure = "Articles",
         Verb_Freq_Std = "Placeholder",
         Interface = "No",
         Timing = "Early",
         Months_at_Start_Std = (Months_at_Start - mean(Months_at_Start))/sd(Months_at_Start))  %>% 
  filter(!is.na(Accuracy_DP_Gen)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Articles Production Data.csv"))


# Create master dataframe
Master <- rbind(DOM, Subjunctive, Clitics, Articles) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Aggregated Production Data.csv"))

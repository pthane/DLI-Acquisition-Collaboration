library(tidyverse)
library(here)

options(scipen = 99)


# Load data
## DOM
HS_2_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Second DOM FCT.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "HS")

HS_45_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_DOM <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_2_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Second DOM FCT.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "L2L")

L2_45_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_DOM <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


## Subjunctive
HS_45_Subj <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_Subj <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_45_Subj <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_Subj <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


## Gender
HS_2_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Second Clitic FCT.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "HS")

HS_45_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "HS")

HS_78_Gender <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "HS")

L2_2_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Second Clitic FCT.csv")) %>% 
  mutate(Age_Group = "2nd grade",
         Lang_Group = "L2L")

L2_45_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Age_Group = "4th/5th grade",
         Lang_Group = "L2L")

L2_78_Gender <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Age_Group = "7th/8th grade",
         Lang_Group = "L2L")


# Join dataframes
## DOM
DOM <- rbind(HS_2_DOM, HS_45_DOM, HS_78_DOM, L2_2_DOM, L2_45_Gender, L2_78_DOM) %>% 
  mutate(Structure = "DOM",
         Verb_Freq_Std = "Placeholder",
         Interface = "Yes",
         Timing = "Early",
         Months_Std = (Months - mean(Months))/sd(Months)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined DOM FCT Data.csv"))

Subjunctive <- rbind(HS_45_Subj, HS_78_Subj, L2_45_Subj, L2_78_Subj) %>% 
  mutate(Structure = "Subjunctive",
         Interface = "No",
         Timing = "Late",
         Months_Std = (Months - mean(Months))/sd(Months)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Subjunctive FCT Data.csv"))

Clitics <- rbind(HS_2_Gender, HS_45_Gender, HS_78_Gender, L2_2_Gender, L2_45_Gender, L2_78_Gender) %>% 
  mutate(Structure = "Clitics",
         Verb_Freq_Std = "Placeholder",
         Interface = "Yes",
         Timing = "Late",
         Months_Std = (Months - mean(Months))/sd(Months)) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Clitics FCT Data.csv"))


# Create master dataframe
Master <- rbind(DOM, Subjunctive, Clitics) %>% 
  write_csv(here("CSV Files", "CSVs Rejoined by Structure", "Aggregated FCT Data.csv"))

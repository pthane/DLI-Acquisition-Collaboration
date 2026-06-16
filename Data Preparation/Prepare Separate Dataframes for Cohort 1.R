library(tidyverse)
library(here)


# Load data
## Read CSV files
Production <- read_csv(here("CSV Files", "Master Files", "Production Data.csv"))
FCT <- read_csv(here("CSV Files", "Master Files", "FCT Data.csv"))


## Check alignment
Compare_Cols <- data.frame(Prod_Col = colnames(Production),
                           FCT_Col = colnames(FCT))


# Generate L2 production datasets
## Second grade
Production_L2_2 <- Production %>%
  filter(Cohort == 1,
         Session == 1) %>% 
  filter(Group == "L2") %>% 
  filter(Grade < 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 2nd",
         Speaker_Group = "L2L",
         Age_Group = "2nd",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_L2_2_Clitics <- Production_L2_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>% 
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Second Clitic Production.csv")

Production_L2_2_DOM <- Production_L2_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>% 
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Second DOM Production.csv")


## Fourth/fifth grade
Production_L2_45 <- Production %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "L2") %>% 
  filter(Grade < 6) %>% 
  filter(Grade > 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 4th/5th",
         Speaker_Group = "L2L",
         Age_Group = "4th/5th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_L2_45_Clitics <- Production_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic Production.csv")

Production_L2_45_Subjunctive <- Production_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv")

Production_L2_45_DOM <- Production_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth DOM Production.csv")


## Seventh and eighth group
Production_L2_78 <- Production %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "L2") %>% 
  filter(Grade > 6) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 7th/8th",
         Speaker_Group = "L2L",
         Age_Group = "7th/8th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_L2_78_Clitics <- Production_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic Production.csv")

Production_L2_78_Subjunctive <- Production_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv")

Production_L2_78_DOM <- Production_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>%
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth DOM Production.csv")


# Generate L2 FCT datasets
## Second grade
FCT_L2_2 <- FCT %>%
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "L2") %>% 
  filter(Grade < 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 2nd",
         Speaker_Group = "L2L",
         Age_Group = "2nd",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_L2_2_Clitics <- FCT_L2_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Second Clitic FCT.csv")

FCT_L2_2_DOM <- FCT_L2_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Second DOM FCT.csv")


## Fourth/fifth grade
FCT_L2_45 <- FCT %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "L2") %>% 
  filter(Grade < 6) %>% 
  filter(Grade > 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 4th/5th",
         Speaker_Group = "L2L",
         Age_Group = "4th/5th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_L2_45_Clitics <- FCT_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic FCT.csv")

FCT_L2_45_Subjunctive <- FCT_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv")

FCT_L2_45_DOM <- FCT_L2_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth DOM FCT.csv")


## Seventh and eighth group
FCT_L2_78 <- FCT %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "L2") %>% 
  filter(Grade > 6) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "L2L 7th/8th",
         Speaker_Group = "L2L",
         Age_Group = "7th/8th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_L2_78_Clitics <- FCT_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic FCT.csv")

FCT_L2_78_Subjunctive <- FCT_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>%
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv")

FCT_L2_78_DOM <- FCT_L2_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth DOM FCT.csv")


# Generate heritage production datasets
## Second grade
Production_HS_2 <- Production %>%
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade < 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 2nd",
         Speaker_Group = "HS",
         Age_Group = "2nd",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_HS_2_Clitics <- Production_HS_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Second Clitic Production.csv")

Production_HS_2_DOM <- Production_HS_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Second DOM Production.csv")


## Fourth/fifth grade
Production_HS_45 <- Production %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade < 6) %>% 
  filter(Grade > 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 4th/5th",
         Speaker_Group = "HS",
         Age_Group = "4th/5th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_HS_45_Clitics <- Production_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic Production.csv")

Production_HS_45_Subjunctive <- Production_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv")

Production_HS_45_DOM <- Production_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth DOM Production.csv")


## Seventh and eighth group
Production_HS_78 <- Production %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade > 6) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 7th/8th",
         Speaker_Group = "HS",
         Age_Group = "7th/8th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

Production_HS_78_Clitics <- Production_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic Production.csv")

Production_HS_78_Subjunctive <- Production_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv")

Production_HS_78_DOM <- Production_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth DOM Production.csv")


# Generate HS FCT datasets
## Second grade
FCT_HS_2 <- FCT %>%
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade < 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 2nd",
         Speaker_Group = "HS",
         Age_Group = "2nd",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_HS_2_Clitics <- FCT_HS_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Second Clitic FCT.csv")

FCT_HS_2_DOM <- FCT_HS_2 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Second DOM FCT.csv")


## Fourth/fifth grade
FCT_HS_45 <- FCT %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade < 6) %>% 
  filter(Grade > 3) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 4th/5th",
         Speaker_Group = "HS",
         Age_Group = "4th/5th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_HS_45_Clitics <- FCT_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic FCT.csv")

FCT_HS_45_Subjunctive <- FCT_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>%
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv")

FCT_HS_45_DOM <- FCT_HS_45 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Fourth and Fifth DOM FCT.csv")


## Seventh and eighth group
FCT_HS_78 <- FCT %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Group == "Heritage") %>% 
  filter(Grade > 6) %>% 
  mutate(BESA_Std = (BESA_Score - mean(BESA_Score))/sd(BESA_Score),
         Group = "HS 7th/8th",
         Speaker_Group = "HS",
         Age_Group = "7th/8th",
         Contexts = (ENG_Contexts + SPA_Contexts)) %>% 
  mutate(SPA_Pct = (SPA_Contexts/Contexts)) %>% 
  select(!starts_with("..."))

FCT_HS_78_Clitics <- FCT_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Clitic") %>% 
  select(!starts_with("...")) %>%
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic FCT.csv")

FCT_HS_78_Subjunctive <- FCT_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "Subjunctive") %>% 
  mutate(Verb_Freq_Std = (Verb_Freq - mean(Verb_Freq))/sd(Verb_Freq)) %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv")

FCT_HS_78_DOM <- FCT_HS_78 %>% 
  filter(Cohort == 1,
         Session == 1) %>%
  filter(Structure == "DOM") %>% 
  select(!starts_with("...")) %>% 
  write_csv("./CSV Files/Heritage/Heritage Seventh and Eighth DOM FCT.csv")


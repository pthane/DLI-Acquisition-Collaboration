## Incorporates all data

library(here)
library(tidyverse)


# Load CSV files
Production <- read_csv(here("CSV Files", "Master Files", "Production Data.csv")) %>% 
  mutate(Task = "Production")

FCT <- read_csv(here("CSV Files", "Master Files", "FCT Data.csv")) %>% 
  mutate(Task = "Selection")


# Merge CSV files
Master <- rbind(Production, FCT) %>% 
  mutate(Speaker_Group = case_when(Group == "L2"       ~ "L2L",
                                   Group == "Heritage" ~ "HS"),
         Age_Group = case_when(Grade < 3           ~ "2nd",
                               Grade > 3 & Grade < 6 ~ "4th/5th",
                               Grade > 6           ~ "7th/8th"),
    Group = case_when(Group == "L2"       & Grade < 3           ~ "L2L 2nd",
                      Group == "L2"       & Grade > 3 & Grade < 6 ~ "L2L 4th/5th",
                      Group == "L2"       & Grade > 6           ~ "L2L 7th/8th",
                      Group == "Heritage" & Grade < 3           ~ "HS 2nd",
                      Group == "Heritage" & Grade > 3 & Grade < 6 ~ "HS 4th/5th",
                      Group == "Heritage" & Grade > 6           ~ "HS 7th/8th")) %>%
  mutate(Months_at_Start = (Months_at_Start - mean(Months_at_Start, na.rm = TRUE)) / sd(Months_at_Start, na.rm = TRUE)) %>% 
  mutate(Structure = case_when(Task == "Production" & Structure == "Clitic" & !is.na(Accuracy_DP_Gen) ~ "Article",
                               TRUE ~ Structure)) %>% 
  mutate(Accuracy_Combined = case_when(Task == "Selection"                               ~ Accuracy,
                                       Task == "Production" & Structure == "DOM"         ~ Accuracy,
                                       Task == "Production" & Structure == "Subjunctive" ~ Accuracy,
                                       Task == "Production" & Structure == "Clitic"      ~ Accuracy_Clit_Gen,
                                       Task == "Production" & Structure == "Article"     ~ Accuracy_DP_Gen)) %>% 
  mutate(Timing = case_when(Structure == "Article"     ~ "Early",
                            Structure == "DOM"         ~ "Early",
                            Structure == "Clitic"      ~ "Late",
                            Structure == "Subjunctive" ~ "Late"),
         Interface = case_when(Structure == "DOM"         ~ "Yes",
                               Structure == "Clitic"      ~ "Yes",
                               Structure == "Article"     ~ "No",
                               Structure == "Subjunctive" ~ "No")) %>% 
  mutate(Contexts = ENG_Contexts + SPA_Contexts,
         Study_Age = case_when(Session == 1 ~ 0,
                               Session == 2 ~ 36),
         Months_at_Testing = case_when(Session == 1 ~ Months_at_Start,
                                       Session == 2 ~ Months_at_Start + Study_Age)) %>% 
  mutate(SPA_Pct = SPA_Contexts / Contexts,
         Verb_Freq_Std = case_when(Structure == "Subjunctive" ~ (Verb_Freq - mean(Verb_Freq, na.rm = TRUE)) / sd(Verb_Freq, na.rm = TRUE)),
         BESA_Std = (BESA_Score - mean(BESA_Score)) / sd(BESA_Score),
         Months_at_Start_Std = (Months_at_Start - mean(Months_at_Start, na.rm = TRUE)) / sd(Months_at_Start, na.rm = TRUE)) %>% 
  select(!starts_with("...")) %>% 
  write_csv(here("CSV Files", "Tidy Data", "Tidy Data All Structures.csv"))


# Partitioned by structure
Articles <- Master %>% 
  filter(Structure == "Article") %>% 
  mutate(Structure = "Articles") %>% 
  write_csv(here("CSV Files", "Tidy Data", "Tidy Data Articles.csv"))

Clitics <- Master %>% 
  filter(Structure == "Clitic") %>% 
  mutate(Structure = "Clitics") %>% 
  write_csv(here("CSV Files", "Tidy Data", "Tidy Data Clitics.csv"))

DOM <- Master %>% 
  filter(Structure == "DOM") %>% 
  write_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv"))

Subjunctive <- Master %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("CSV Files", "Tidy Data", "Tidy Data Subjunctive.csv"))


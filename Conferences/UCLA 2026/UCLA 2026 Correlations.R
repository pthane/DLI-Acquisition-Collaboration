## Includes cohort 1 data only. No subjunctive data from second grade.

library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(emmeans)

options(scipen = 99)


# Prepare data
## Load dataframes
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data All Structures.csv")) %>%
  filter(Task == "Production",
         Cohort == 1,
         Session == 1,
         Speaker_Group == "HS",
         !is.na(Accuracy_Combined)) %>% 
  mutate(Abbreviated_Structure = (case_when(Structure == "DOM" ~ "DOM",
                                            Structure == "Subjunctive" ~ "VS",
                                            Structure == "Clitics" ~ "GA-DOC",
                                            Structure == "Articles" ~ "GA-DN")))


# Structure by structure model
## Main GLMM
Structure_Model <- glmer(Accuracy_Combined ~ Structure +
                           (1 | Part_ID) + (1 | Item),
                         data = Master,
                         family = "binomial")

summary(Structure_Model)


## Post-hoc comparisons
Structure_Pairwise <- emmeans(Structure_Model, spec = "Structure")
Structure_Tukey <- contrast(Structure_Pairwise, method = "pairwise")

summary(Structure_Tukey)
confint(Structure_Tukey)


# Main model
Main_Model <- glmer(Accuracy_Combined ~ 1 + Timing + Interface + Age_Group +
                      (1 | Part_ID) + (1 | Item),
                    data = Master,
                    family = "binomial")

summary(Main_Model)

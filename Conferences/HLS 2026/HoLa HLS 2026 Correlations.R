## Includes cross-sectional data only

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
         Session == 1,
         !is.na(Accuracy_Combined))


# Group by structure model
## Model specification
Structure_Model <- glmer(Accuracy_Combined ~ 1 + Speaker_Group + Structure + Months_at_Testing_Ctd + Speaker_Group:Structure +
                      (1 | Part_ID) + (1 | Item),
                    data = Master,
                    family = "binomial",
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 10000)))

summary(Structure_Model)


## Post-hoc comparisons
Pairwise <- emmeans(Structure_Model, spec = "Structure")
Tukey <- contrast(Pairwise, method = "pairwise")

summary(Tukey)
confint(Tukey)


# Participant counts as it stands
## Separate groups
HS <- Master %>% 
  filter(Speaker_Group == "HS")

L2L <- Master %>% 
  filter(Speaker_Group == "L2L")


## Get participant counts
unique(HS$Part_ID)
unique(L2L$Part_ID)

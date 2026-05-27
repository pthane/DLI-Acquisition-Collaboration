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

# Two-variable model
Binary_Model <- glmer(Accuracy_Combined ~ 1 + Speaker_Group + Months_at_Start + Timing + Interface +
                      (1 + Timing + Interface | Part_ID) + (1 | Item),
                    data = Master,
                    family = "binomial",
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 10000)))

summary(Binary_Model)


# Group by structure model
## Model specification
Structure_Model <- glmer(Accuracy_Combined ~ 1 + Speaker_Group + Structure + Months_at_Start + Speaker_Group:Structure +
                      (1 + Structure | Part_ID) + (1 | Item),
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

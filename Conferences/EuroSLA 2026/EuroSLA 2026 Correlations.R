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
         !is.na(Accuracy_Combined))


# Prepare models
## Timing of acquisition vs. Interface
Timing_Interface_Model <- glmer(Accuracy_Combined ~ 1 + Timing + Interface + Age_Group + Speaker_Group +
                      (1 + Timing + Interface | Part_ID) + (1 | Item),
                    data = Master,
                    family = "binomial",
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 10000)))

summary(Timing_Interface_Model)


## Structure-by-structure
Structure_Model <- glmer(Accuracy_Combined ~ 1 + Structure +
                                  (1 + Structure | Part_ID) + (1 | Item),
                                data = Master,
                                family = "binomial",
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 10000)))

summary(Structure_Model)


## Post-hoc comparisons for structure
Pairwise <- emmeans(Structure_Model, spec = "Structure")
Tukey <- contrast(Pairwise, method = "pairwise")

summary(Tukey)
confint(Tukey)


## Rinke et al. (2003) would predict the following hierarchy: articles > clitics > subjunctive and DOM
## Our results: Articles > clitics > DOM > subjunctive (mostly aligned)
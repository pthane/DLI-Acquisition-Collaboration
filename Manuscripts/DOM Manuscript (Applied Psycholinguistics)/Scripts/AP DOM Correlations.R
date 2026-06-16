## Includes cross-sectional data only
## When data are inputted, CHANGE Grade to Months_at_Start.

library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  filter(Session == 1) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine"))) %>% 
  drop_na(Accuracy)


## Create task-specific sheets
Production <- Master %>% 
  filter(Task == "Production")

Selection <- Master %>% 
  filter(Task == "Selection")


# Generate correlations
## Fully specified random effects
GLMM_Combined_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Testing_Ctd + Task + Gender +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

summary(GLMM_Combined_Full)
VarCorr(GLMM_Combined_Full)


## Reduced random effects not reported in manuscript
GLMM_Combined_Reduced <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Testing_Ctd + Task + Gender +
                              (1 | Part_ID),
                            data = Master,
                            family = "binomial",
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=10000)))

summary(GLMM_Combined_Reduced)


# Generate production GLMM
## Full model
GLMM_Production_Full <- glmer(Accuracy ~ Speaker_Group + Months_at_Testing_Ctd + Gender +
                           (1 + Gender | Part_ID) + (1 | Item),
                         data = Production,
                         family = "binomial",
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Production_Full)
VarCorr(GLMM_Production_Full)


## Reduced random effects not reported in manuscript
GLMM_Production_Reduced <- glmer(Accuracy ~ Speaker_Group + Months_at_Testing_Ctd + Gender +
                                (1 + Gender | Part_ID),
                              data = Production,
                              family = "binomial",
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Production_Reduced)


# Generate selection GLMM
## Full model
GLMM_Selection_Full <- glmer(Accuracy ~ Speaker_Group + Months_at_Testing_Ctd + Gender +
                               (1 + Gender | Part_ID) + (1 | Item),
                             data = Selection,
                             family = "binomial",
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Selection_Full)
VarCorr(GLMM_Selection_Full)


## Reduced random effects not reported in manuscript
GLMM_Selection_Reduced <- glmer(Accuracy ~ Speaker_Group + Months_at_Testing_Ctd + Gender +
                                  (1 | Part_ID),
                                data = Selection,
                                family = "binomial",
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Selection_Reduced)

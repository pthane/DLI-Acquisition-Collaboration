## Includes cross-sectional data only


library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine"))) %>% 
  drop_na(Accuracy, Speaker_Group, Months_at_Start, Task, Gender, Part_ID, Item)


## Create task-specific sheets
Production <- Master %>% 
  filter(Task == "Production")

Selection <- Master %>% 
  filter(Task == "Selection")


# Generate correlations
## Nested model comparisons
GLMM_Null <- glmer(Accuracy ~ 1 +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

GLMM_Speaker_Group <- glmer(Accuracy ~ 1 + Speaker_Group +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

GLMM_Months <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Start +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

GLMM_Task <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Start_Std + Task +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

GLMM_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Start_Std + Task + Gender +
                    (1 + Gender | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial",
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=10000)))

GLMM_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Months_at_Start_Std + Task + Gender + Speaker_Group:Months_at_Start_Std +
                      (1 + Gender | Part_ID) + (1 | Item),
                    data = Master,
                   family = "binomial",
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=10000)))

anova(GLMM_Null, GLMM_Speaker_Group, GLMM_Months, GLMM_Task, GLMM_Gender, GLMM_Full, test = "chisq")


## Final correlation
summary(GLMM_Gender)

plot_model(GLMM_Gender, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of Final GLMM Model", y = "Estimates") +
  scale_x_discrete(labels = c("Feminine gender", "Preference task", "7th/8th grade age group", "4th/5th age group", "L2L speaker group", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Generate production GLMM
GLMM_Production <- glmer(Accuracy ~ Speaker_Group + Months_at_Start_Std + Gender + Speaker_Group:Months_at_Start_Std +
                           (1 + Gender | Part_ID) + (1 | Item),
                         data = Production,
                         family = "binomial",
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Production)


# Generate selection GLMM
GLMM_Selection <- glmer(Accuracy ~ Speaker_Group + Months_at_Start_Std + Gender + Speaker_Group:Months_at_Start_Std +
                           (1 + Gender | Part_ID) + (1 | Item),
                         data = Selection,
                         family = "binomial",
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

summary(GLMM_Selection)

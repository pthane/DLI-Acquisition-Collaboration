library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
L2_2_Prod <- read.csv(here("CSV Files", "L2 Learners", "L2 Second DOM Production.csv"))
L2_2_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Second DOM FCT.csv"))

L2_45_Prod <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM Production.csv"))
L2_45_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM FCT.csv"))

L2_78_Prod <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM Production.csv"))
L2_78_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM FCT.csv"))

HS_2_Prod <- read.csv(here("CSV Files", "Heritage", "Heritage Second DOM Production.csv"))
HS_2_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Second DOM FCT.csv"))

HS_45_Prod <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM Production.csv"))
HS_45_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM FCT.csv"))

HS_78_Prod <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM Production.csv"))
HS_78_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM FCT.csv"))


## Create combined data
Production <- rbind(L2_2_Prod, L2_45_Prod, L2_78_Prod, HS_2_Prod, HS_45_Prod, HS_78_Prod)
Production$Gender <- factor(Production$Gender, levels = c("Masculine", "Feminine"))

FCT <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT, HS_45_FCT, HS_2_FCT, HS_78_FCT)
FCT$Gender <- factor(FCT$Gender, levels = c("Masculine", "Feminine"))

Master <- rbind(Production, FCT)
Master$Task <- factor(Master$Task, levels = c("Production", "Forced Choice"))
Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))


# Generate correlations
## Nested model comparisons
GLMM_Null <- glmer(Accuracy ~ 1 +
                    (1 | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial")

GLMM_Speaker_Group <- glmer(Accuracy ~ 1 + Speaker_Group +
                    (1 | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial")

GLMM_Age_Group <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group +
                    (1 | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial")

GLMM_Task <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task +
                    (1 | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial")

GLMM_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Gender +
                    (1 | Part_ID) + (1 | Item),
                  data = Master,
                  family = "binomial")

GLMM_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Gender + Speaker_Group:Age_Group +
                      (1 | Part_ID) + (1 | Item),
                    data = Master,
                   family = "binomial")

anova(GLMM_Null, GLMM_Speaker_Group, GLMM_Age_Group, GLMM_Task, GLMM_Gender, GLMM_Full, test = "chisq")


## Final correlation
summary(GLMM_Gender)

plot_model(GLMM_Gender, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of Final GLMM Model", y = "Estimates") +
  scale_x_discrete(labels = c("Feminine gender", "Preference task", "7th/8th grade age group", "4th/5th age group", "L2L speaker group", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Generate production GLMM
GLMM_Production <- glmer(Accuracy ~ Speaker_Group * Age_Group + Gender +
                           (1 | Part_ID) + (1 | Item),
                         data = Production,
                         family = "binomial")

summary(GLMM_Production)


# Generate production GLMM
GLMM_FCT <- glmer(Accuracy ~ Speaker_Group * Age_Group + Gender +
                           (1 | Part_ID) + (1 | Item),
                         data = FCT,
                         family = "binomial")

summary(GLMM_FCT)

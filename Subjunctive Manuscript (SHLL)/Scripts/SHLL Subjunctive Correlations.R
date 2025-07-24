library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
L2_45_Prod <- read.csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv"))
L2_45_FCT <- read.csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv"))

L2_78_Prod <- read.csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv"))
L2_78_FCT <- read.csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv"))

HS_45_Prod <- read.csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv"))
HS_45_FCT <- read.csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv"))

HS_78_Prod <- read.csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv"))
HS_78_FCT <- read.csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv"))


## Create combined data
Production <- rbind(L2_45_Prod, L2_78_Prod, HS_45_Prod, HS_78_Prod)
FCT <- rbind(L2_45_FCT, L2_78_FCT, HS_45_FCT, HS_78_FCT)

Master <- rbind(Production, FCT)
Master$Task <- factor(Master$Task, levels = c("Production", "Forced Choice"))


## Create L2 data for post-hoc analysis
L2_Only <- Master %>% 
  filter(Speaker_Group == "L2L")


# Nested model comparisons
## Overall correlation
Correlation_Null <- glmer(Accuracy ~ 1 +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Speaker_Group <- glmer(Accuracy ~ 1 + Speaker_Group +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Age_Group <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Task <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Verb_Type <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Verb_Type +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Verb_Freq <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Verb_Type + Verb_Freq_Std +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Speaker_Age <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Verb_Type + Verb_Freq_Std + Speaker_Group:Age_Group +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Verb_Type + Verb_Freq_Std + Speaker_Group:Age_Group + Speaker_Group : Verb_Type +
                                         (1 | Part_ID) + (1 | Item),
                                       data = Master,
                                       family = "binomial")

anova(Correlation_Null, Correlation_Speaker_Group, Correlation_Age_Group, Correlation_Task, Correlation_Verb_Type, Correlation_Verb_Freq, Correlation_Speaker_Age, Correlation_Full, test = "Chisq")


# Full GLMMs
## Overall correlation
summary(Correlation_Verb_Type)


plot_model(Correlation_Verb_Type, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model", y = "Estimates") +
  scale_y_continuous(breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  scale_x_discrete(labels = c("Regular verbs", "Forced choice task", "7th/8th grade", "L2 learners", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Post-hoc L2 correlation
Correlation_L2 <- glmer(Accuracy ~ 1 + Age_Group +
                          (1 | Part_ID) + (1 | Item),
                        data = L2_Only,
                        family = "binomial")

summary(Correlation_L2)


## Post-hoc regularity by task interaction
Correlation_Interaction <- glmer(Accuracy ~ 1 + Task * Verb_Type +
                                 (1 | Part_ID) + (1 | Item),
                                 data = Master,
                                 family = "binomial")

summary(Correlation_Interaction)

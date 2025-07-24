library(tidyverse)
library(here)


# Prepare dataframes
## Load data
L2_45_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

HS_45_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

L2_78_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

HS_78_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")


## Production average
Production <- rbind(L2_45_Prod, HS_45_Prod, L2_78_Prod, HS_78_Prod) %>% 
  group_by(Part_ID, Age_Group, Speaker_Group) %>%
  summarize(Total_Subj = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100))

Selection <- rbind(L2_45_FCT, HS_45_FCT, L2_78_FCT, HS_78_FCT) %>% 
  group_by(Part_ID, Age_Group, Speaker_Group) %>%
  summarize(Total_Subj = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Forced Choice",
         Ratio = (Ratio*100))

Master <- rbind(Production, Selection)
Master$Task <- factor(Master$Task, levels = c("Production", "Forced Choice"))


# Aggregate model
Overall_Boxplot <- Master %>% 
  ggplot(aes(x = Age_Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  facet_grid(vars(Speaker_Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Age group", y = "Percentage of subjunctive responses", title = "Distribution of Subjunctive Use by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Overall_Boxplot

ggsave(filename = here("Subjunctive Manuscript (SHLL)", "Graphs", "SHLL Figure 3.pdf"),
       plot = Overall_Boxplot,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")

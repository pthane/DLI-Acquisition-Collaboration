library(tidyverse)
library(here)


# Load Data
L2_2_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM Production.csv"))
L2_2_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM FCT.csv"))

HS_2_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM Production.csv"))
HS_2_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM FCT.csv"))


## Fourth/fifth
L2_45_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM Production.csv"))
L2_45_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM FCT.csv"))

HS_45_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM Production.csv"))
HS_45_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM FCT.csv"))


## Seventh/eighth
L2_78_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM Production.csv"))
L2_78_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM FCT.csv"))

HS_78_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM Production.csv"))
HS_78_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM FCT.csv"))


## Group
Production <- rbind(L2_2_Prod, L2_45_Prod, L2_78_Prod, HS_2_Prod, HS_45_Prod, HS_78_Prod)
FCT <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT, HS_2_FCT, HS_45_FCT, HS_78_FCT) %>% 
  mutate(Task = "Preference")

Combined <- rbind(Production, FCT)
Combined$Task <- factor(Combined$Task, levels = c("Production", "Preference"))


# Generate percentages
## Bar graph
Master_Bar <- Combined %>% 
  group_by(Group, Task, Gender) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Boxplot
Master_Box <- Combined %>% 
  group_by(Part_ID, Group, Task, Gender) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Bar graph ratios
Master_Bar_Summary <- Master_Box %>%
  group_by(Group, Task, Gender) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE)) %>% 
  left_join(Master_Bar, Master_Bar_Summary, by = c("Group", "Task", "Gender"))


# Generate bar graph
Gender_Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Gender, y = Ratio, fill = Task)) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  facet_wrap(facets = vars(Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
              position = position_dodge(width = .9),
              vjust = 0.5,
              size = 2.75,
              fontface = "bold") +
  labs(x = "Gender of direct object", y = "Percentage of DOM responses", fill = "Task", title = "Differential Object Marking by Group, Task, and Gender") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Gender_Bar_Graph

ggsave(filename = here("DOM Manuscript (Applied Psycholinguistics)", "Graphs", "AP DOM Figure 4.pdf"),
       plot = Gender_Bar_Graph,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")

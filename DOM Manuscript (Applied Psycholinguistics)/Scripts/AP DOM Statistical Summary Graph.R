library(tidyverse)
library(here)
library(patchwork)


# Load Data
## Second
L2_2_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "2nd")

L2_2_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "2nd",
         Task = "Preference")

HS_2_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "2nd")

HS_2_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "2nd",
         Task = "Preference")


## Fourth/fifth
L2_45_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "4th/5th")

L2_45_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "4th/5th",
         Task = "Preference")

HS_45_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "4th/5th")

HS_45_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "4th/5th",
         Task = "Preference")


## Seventh/eighth
L2_78_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "7th/8th")

L2_78_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Group = "7th/8th",
         Task = "Preference")

HS_78_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "7th/8th")

HS_78_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Group = "7th/8th",
         Task = "Preference")


## Group
Production <- rbind(L2_2_Prod, L2_45_Prod, L2_78_Prod, HS_2_Prod, HS_45_Prod, HS_78_Prod)
FCT <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT, HS_2_FCT, HS_45_FCT, HS_78_FCT)

Combined <- rbind(Production, FCT)
Combined$Task <- factor(Combined$Task, levels = c("Production", "Preference"))


# Generate percentages
## Bar graph
Master_Bar <- Combined %>% 
  group_by(Group, SpeakerType, Task) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Boxplot
Master_Box <- Combined %>% 
  group_by(Part_ID, Group, SpeakerType, Task) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Bar graph ratios
Master_Bar_Summary <- Master_Box %>%
  group_by(Group, SpeakerType, Task) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE)) %>% 
  left_join(Master_Bar, Master_Bar_Summary, by = c("Group", "SpeakerType", "Task"))


# Generate graphs
## Bar graph
Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_grid(rows = vars(SpeakerType)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  labs(x = "Average (SD)", y = "Percentage of DOM responses", fill = "Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Bar_Graph


## Boxplot
Boxplot <- Master_Box %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  facet_grid(rows = vars(SpeakerType)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Distribution") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Boxplot


## Create joint plot
Group_Plot <- (Bar_Graph + Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Age, Speaker Type, and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("DOM Manuscript (Applied Psycholinguistics)", "Graphs", "AP DOM Figure 3.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")

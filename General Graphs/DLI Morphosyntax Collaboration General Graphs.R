## Includes cohort 1 data so far, but this will shift over time


library(tidyverse)
library(here)
library(patchwork)


# Load CSVs
## Production
Subj_Prod <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Subjunctive Production Data.csv")) %>%
  mutate(Structure = "Subjunctive",
         Task = "PROD")

DOM_Prod <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined DOM Production Data.csv")) %>%
  mutate(Structure = "DOM",
         Task = "PROD")

Clitics_Prod <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Clitics Production Data.csv")) %>%
  mutate(Structure = "Clitic gender",
         Task = "PROD")

## FCT
Subj_FCT <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Subjunctive FCT Data.csv")) %>%
  mutate(Structure = "Subjunctive",
         Task = "SEL")

DOM_FCT <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined DOM FCT Data.csv")) %>%
  mutate(Structure = "DOM",
         Task = "SEL")

Clitics_FCT <- read_csv(here("CSV Files", "CSVs Rejoined by Structure", "Rejoined Clitics FCT Data.csv")) %>%
  mutate(Structure = "Clitic gender",
         Task = "SEL")


## Create joint dataframes
Subj <- rbind(Subj_Prod, Subj_FCT)
DOM <- rbind(DOM_Prod, DOM_FCT)
Clitics <- rbind(Clitics_Prod, Clitics_FCT)


# Create summaries
## Subjunctive
Subj_Bar  <- Subj %>% 
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Total_Structure = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Subj_Box <- Subj %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Age_Group, Lang_Group, Task) %>%
  summarize(Total_Accuracy  = sum(Accuracy, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")

Subj_Bar_Summary <- Subj_Box %>%
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(Subj_Box, Subj_Bar_Summary, by = c("Age_Group", "Lang_Group", "Task"))


## DOM
DOM_Bar  <- DOM %>% 
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Total_Structure = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

DOM_Box <- DOM %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Age_Group, Lang_Group, Task) %>%
  summarize(Total_Accuracy  = sum(Accuracy, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")

DOM_Bar_Summary <- DOM_Box %>%
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(DOM_Box, DOM_Bar_Summary, by = c("Age_Group", "Lang_Group", "Task"))


## Clitics
Clitics_Bar  <- Clitics %>% 
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Total_Structure = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Clitics_Box <- Clitics %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Age_Group, Lang_Group, Task) %>%
  summarize(Total_Accuracy  = sum(Accuracy, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")

Clitics_Bar_Summary <- Clitics_Box %>%
  group_by(Age_Group, Lang_Group, Task) %>%
  summarize(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(Clitics_Box, Clitics_Bar_Summary, by = c("Age_Group", "Lang_Group", "Task"))


# Create axis modification
Subj_Abbreviations <- c("4th/5th", "7th/8th")
Other_Abbreviations <- c("2nd", "4th/5th", "7th/8th")


# Create subjunctive graphs
## Subjunctive bar graph
Subj_Bar_Graph <- Subj_Bar_Summary %>% 
  ggplot(aes(x = Age_Group, y = Average, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_x_discrete(labels = Subj_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Average (SD)", y = "Percentage of DOM responses") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Subj_Bar_Graph


## Subjunctive boxplot
Subj_Boxplot <- Subj_Box %>% 
  ggplot(aes(x = Age_Group, y = Ratio, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_boxplot() +
  scale_x_discrete(labels = Subj_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("PROD", "SEL")) +
  labs(x = "Distribution") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Subj_Boxplot

## Subjunctive joint graph
Subj_Final <- (Subj_Bar_Graph + Subj_Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Subj_Final

ggsave(filename = here("General Graphs", "DLI Morphosyntax Collaboration Subjunctive Graph.pdf"),
       plot = Subj_Final,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


# Create DOM graphs
## DOM bar graph
DOM_Bar_Graph <- DOM_Bar_Summary %>% 
  ggplot(aes(x = Age_Group, y = Average, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_x_discrete(labels = Other_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Average (SD)", y = "Percentage of DOM responses") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

DOM_Bar_Graph


## DOM boxplot
DOM_Boxplot <- DOM_Box %>% 
  ggplot(aes(x = Age_Group, y = Ratio, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_boxplot() +
  scale_x_discrete(labels = Other_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("PROD", "SEL")) +
  labs(x = "Distribution") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

DOM_Boxplot


## DOM joint graph
DOM_Final <- (DOM_Bar_Graph + DOM_Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

DOM_Final

ggsave(filename = here("General Graphs", "DLI Morphosyntax Collaboration DOM Graph.pdf"),
       plot = DOM_Final,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


# Create clitics graphs
## Clitics bar graph
Clitics_Bar_Graph <- Clitics_Bar_Summary %>% 
  ggplot(aes(x = Age_Group, y = Average, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_x_discrete(labels = Other_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Average (SD)", y = "Percentage of clitic responses") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Clitics_Bar_Graph


## Clitics boxplot
Clitics_Boxplot <- Clitics_Box %>% 
  ggplot(aes(x = Age_Group, y = Ratio, fill = Task)) +
  facet_grid(rows = vars(Lang_Group)) +
  geom_boxplot() +
  scale_x_discrete(labels = Other_Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("PROD", "SEL")) +
  labs(x = "Distribution") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Clitics_Boxplot


## Clitics joint graph
Clitics_Final <- (Clitics_Bar_Graph + Clitics_Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Clitics_Final

ggsave(filename = here("General Graphs", "DLI Morphosyntax Collaboration Clitics Graph.pdf"),
       plot = Clitics_Final,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


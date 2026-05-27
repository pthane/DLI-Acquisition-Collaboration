## Includes cohort 1 data only. No subjunctive data from second grade.

library(tidyverse)
library(here)
library(patchwork)


# Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data All Structures.csv")) %>%
  filter(Task == "Production",
         Cohort == 1,
         Session == 1,
         Speaker_Group == "HS",
         !is.na(Accuracy_Combined))


# Create averages
## Bar graph
Bar <- Master %>% 
  group_by(Age_Group, Structure, Interface, Timing) %>%
  summarize(Total_Structure = sum(Accuracy_Combined, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy_Combined)),
            Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Boxplot
Boxplot <- Master %>% 
  group_by(Part_ID, Age_Group, Structure, Interface, Timing) %>%
  summarize(Total_Structure = sum(Accuracy_Combined, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy_Combined)),
            Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Join bar and box data
Bar_Summary <- Boxplot %>%
  group_by(Age_Group, Structure) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE)) %>% 
  left_join(Bar, Bar_Summary, by = c("Age_Group", "Structure"))


# Create bar graph
Bar_Graph <- Bar_Summary %>% 
  ggplot(aes(x = Structure, y = Ratio, fill = Age_Group)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = paste0(round(Ratio), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.25,
            fontface = "bold") +
  labs(x = "Average (SD) by group",
       y = "Percentage of target responses",
       fill = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "bold"))


Bar_Graph


# Create boxplot
Boxplot_Graph <- Boxplot %>% 
  ggplot(aes(x = Structure, y = Ratio, fill = Age_Group)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  labs(x = "Distribution by group", fill = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Boxplot_Graph


# Join graphs
# Merge plots
Combined <- (Bar_Graph + Boxplot_Graph) + 
  plot_annotation(title = "Responses by Age, Group, and Structure") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Combined

ggsave(filename = here("Conferences", "UCLA 2026", "UCLA 2026 Summary.pdf"),
       plot = Combined,
       device = "pdf",
       width = 6.5,
       height = 4,
       units = "in")

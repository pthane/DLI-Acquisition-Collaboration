## Include cross-sectional data only

library(tidyverse)
library(here)
library(patchwork)


# Prepare data
## Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine")),
         Grade = factor(Grade, levels = c(2, 3, 4, 5, 6, 7, 8)),
         Task = case_when(Task == "Production" ~ "PROD",
                   Task == "Selection" ~ "SEL"))


# Generate percentages
## Bar graph
Master_Bar <- Master %>% 
  group_by(Speaker_Group, Grade, Task) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Violin
Master_Box <- Master %>% 
  group_by(Part_ID, Speaker_Group, Grade, Task) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Bar graph ratios
Master_Bar_Summary <- Master_Box %>%
  group_by(Speaker_Group, Grade, Task) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE)) %>% 
  left_join(Master_Bar, Master_Bar_Summary, by = c("Speaker_Group", "Grade", "Task"))


# Generate graphs
## Bar graph
Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Grade, y = Ratio, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_grid(rows = vars(Speaker_Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#CC3333", "#7A7E82")) +
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

## Violin
Violin <- Master_Box %>% 
  ggplot(aes(x = Grade, y = Ratio, fill = Task)) + 
  geom_violin() +
  facet_grid(rows = vars(Speaker_Group)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#CC0033", "#5F6A72")) +
  labs(x = "Distribution") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Violin


## Create joint plot
Group_Plot <- (Bar_Graph + Violin) + 
  plot_annotation(title = "Statistical Summary of Responses by Age, Speaker Type, and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("DOM Manuscript (Applied Psycholinguistics)", "Graphs", "AP DOM Figure 3.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 8,
       height = 4,
       units = "in")

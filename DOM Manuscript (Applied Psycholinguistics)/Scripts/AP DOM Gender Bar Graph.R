## Include cross-sectional data only

library(tidyverse)
library(here)


# Prepare data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine")),
         Grade = factor(Grade, levels = c(2, 3, 4, 5, 6, 7, 8)))


# Generate percentages
## Bar graph
Master_Bar <- Master %>% 
  group_by(Group, Task, Gender) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Boxplot
Master_Box <- Master %>% 
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
  scale_fill_manual(values = c("#CC0033", "#5F6A72")) +
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

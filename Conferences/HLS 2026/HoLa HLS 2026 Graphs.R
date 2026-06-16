# Cross-sectional data from session 1 only

library(tidyverse)
library(here)


# Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data All Structures.csv")) %>% 
  filter(Task == "Production",
         Session == 1) %>% 
  group_by(Part_ID, Speaker_Group, Task, Structure, Months_at_Testing) %>%
  summarize(Total_Accuracy    = sum(Accuracy_Combined, na.rm = TRUE),
            Total_Responses   = sum(!is.na(Accuracy_Combined)),
            Total_Rows        = n(),
            .groups = "drop") %>%
  mutate(Ratio = case_when(Task == "Production" ~ Total_Accuracy / Total_Responses,
                           Task == "Selection"  ~ Total_Accuracy / Total_Rows),
         Ratio = Ratio * 100,
         Structure = case_when(Structure == "DOM"         ~ "DOM",
                               Structure == "Subjunctive" ~ "Subjunctive",
                               Structure == "Clitic"      ~ "Clitics",
                               Structure == "Article"     ~ "Articles"))


# Graph
# Create plot
Plot <- Growth_Plot <- ggplot(Master, aes(x = Months_at_Testing, y = Ratio,
                                          color = Speaker_Group)) +
  geom_point(alpha = 0.85, size = 1.8) +
  geom_smooth(method = "glm", method.args = list(family = gaussian()),
              se = TRUE, linewidth = 0.9) +
  facet_grid(Structure ~ .) +
  scale_x_continuous(breaks = seq(84, 168, 12),
                     limits = c(77, 175)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(-2, 102)) +
  labs(x     = "Age (months)",
       y     = "Percentage of target-like responses",
       color = "Group",
       title = "Accuracy in Production by Age, Structure, and Group") +
  theme(axis.title      = element_text(face = "bold"),
        plot.title      = element_text(hjust = 0.5, face = "bold"),
        legend.title    = element_text(face = "bold"),
        strip.text      = element_text(face = "bold"),
        legend.position = "right")

Plot

ggsave(filename = here("Conferences", "HLS 2026", "HLS 2026 Abstract Summary.pdf"),
       plot = Plot,
       device = "pdf",
       width = 6.5,
       height = 4,
       units = "in")

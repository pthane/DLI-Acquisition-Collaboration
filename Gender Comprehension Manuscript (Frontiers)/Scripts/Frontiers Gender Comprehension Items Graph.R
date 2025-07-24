library(tidyverse)
library(here)


# Load data
HS_2_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

L2_2_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")


# Separate by gender
## Masculine
HS_2_FCT_Masculine <- HS_2_FCT %>% 
  filter(Gender == "Masculine")

HS_45_FCT_Masculine <- HS_45_FCT %>% 
  filter(Gender == "Masculine")

HS_78_FCT_Masculine <- HS_78_FCT %>% 
  filter(Gender == "Masculine")

L2_2_FCT_Masculine <- L2_2_FCT %>% 
  filter(Gender == "Masculine")

L2_45_FCT_Masculine <- L2_45_FCT %>% 
  filter(Gender == "Masculine")

L2_78_FCT_Masculine <- L2_78_FCT %>% 
  filter(Gender == "Masculine")


## Feminine
HS_2_FCT_Feminine <- HS_2_FCT %>% 
  filter(Gender == "Feminine")

HS_45_FCT_Feminine <- HS_45_FCT %>% 
  filter(Gender == "Feminine")

HS_78_FCT_Feminine <- HS_78_FCT %>% 
  filter(Gender == "Feminine")

L2_2_FCT_Feminine <- L2_2_FCT %>% 
  filter(Gender == "Feminine")

L2_45_FCT_Feminine <- L2_45_FCT %>% 
  filter(Gender == "Feminine")

L2_78_FCT_Feminine <- L2_78_FCT %>% 
  filter(Gender == "Feminine")


## Join by speaker group
HS <- rbind(HS_2_FCT, HS_45_FCT, HS_78_FCT)
L2L <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT)


# Separate by lexical items, HS
## HS, masculine
HS_Instrumentos <- HS %>% 
  filter(Item == "FCT-07") %>% 
  mutate(Item = "instrumentos")

HS_Lapicero <- HS %>% 
  filter(Item == "FCT-19") %>% 
  mutate(Item = "lapicero")

HS_Libro <- HS %>% 
  filter(Item == "FCT-03") %>% 
  mutate(Item = "libro")

HS_Libros <- HS %>% 
  filter(Item == "FCT-15") %>% 
  mutate(Item = "libros")

HS_Muñeco <- HS %>% 
  filter(Item == "FCT-11") %>% 
  mutate(Item = "muñeco")

HS_Números <- HS %>% 
  filter(Item == "FCT-23") %>% 
  mutate(Item = "números")


## HS, feminine
HS_Cartas <- HS %>% 
  filter(Item == "FCT-09") %>% 
  mutate(Item = "cartas")

HS_Guitarra <- HS %>% 
  filter(Item == "FCT-13") %>% 
  mutate(Item = "guitarra")

HS_Letras <- HS %>% 
  filter(Item == "FCT-17") %>% 
  mutate(Item = "letras")

HS_Película <- HS %>% 
  filter(Item == "FCT-05") %>% 
  mutate(Item = "película")

HS_Piñata <- HS %>% 
  filter(Item == "FCT-21") %>% 
  mutate(Item = "piñata")

HS_Uñas <- HS %>% 
  filter(Item == "FCT-05") %>% 
  mutate(Item = "uñas")


# Separate by lexical items, L2L
## L2L, masculine
L2L_Instrumentos <- L2L %>% 
  filter(Item == "FCT-07") %>% 
  mutate(Item = "instrumentos")

L2L_Lapicero <- L2L %>% 
  filter(Item == "FCT-19") %>% 
  mutate(Item = "lapicero")

L2L_Libro <- L2L %>% 
  filter(Item == "FCT-03") %>% 
  mutate(Item = "libro")

L2L_Libros <- L2L %>% 
  filter(Item == "FCT-15") %>% 
  mutate(Item = "libros")

L2L_Muñeco <- L2L %>% 
  filter(Item == "FCT-11") %>% 
  mutate(Item = "muñeco")

L2L_Números <- L2L %>% 
  filter(Item == "FCT-23") %>% 
  mutate(Item = "números")


## L2L, feminine
L2L_Cartas <- L2L %>% 
  filter(Item == "FCT-09") %>% 
  mutate(Item = "cartas")

L2L_Guitarra <- L2L %>% 
  filter(Item == "FCT-13") %>% 
  mutate(Item = "guitarra")

L2L_Letras <- L2L %>% 
  filter(Item == "FCT-17") %>% 
  mutate(Item = "letras")

L2L_Película <- L2L %>% 
  filter(Item == "FCT-05") %>% 
  mutate(Item = "película")

L2L_Piñata <- L2L %>% 
  filter(Item == "FCT-21") %>% 
  mutate(Item = "piñata")

L2L_Uñas <- L2L %>% 
  filter(Item == "FCT-05") %>% 
  mutate(Item = "uñas")


# Generate participant averages
## HS, masculine
HS_Instrumentos_Average <- aggregate(HS_Instrumentos$Accuracy, list(HS_Instrumentos$Structure), FUN = mean, na.rm = TRUE)
HS_Instrumentos_Average <- HS_Instrumentos_Average %>% rename(Structure_Avg = x)
HS_Instrumentos_Average <- left_join(HS_Instrumentos, HS_Instrumentos_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Lapicero_Average <- aggregate(HS_Lapicero$Accuracy, list(HS_Lapicero$Structure), FUN = mean, na.rm = TRUE)
HS_Lapicero_Average <- HS_Lapicero_Average %>% rename(Structure_Avg = x)
HS_Lapicero_Average <- left_join(HS_Lapicero, HS_Lapicero_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Libro_Average <- aggregate(HS_Libro$Accuracy, list(HS_Libro$Structure), FUN = mean, na.rm = TRUE)
HS_Libro_Average <- HS_Libro_Average %>% rename(Structure_Avg = x)
HS_Libro_Average <- left_join(HS_Libro, HS_Libro_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Libros_Average <- aggregate(HS_Libros$Accuracy, list(HS_Libros$Structure), FUN = mean, na.rm = TRUE)
HS_Libros_Average <- HS_Libros_Average %>% rename(Structure_Avg = x)
HS_Libros_Average <- left_join(HS_Libros, HS_Libros_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Muñeco_Average <- aggregate(HS_Muñeco$Accuracy, list(HS_Muñeco$Structure), FUN = mean, na.rm = TRUE)
HS_Muñeco_Average <- HS_Muñeco_Average %>% rename(Structure_Avg = x)
HS_Muñeco_Average <- left_join(HS_Muñeco, HS_Muñeco_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Números_Average <- aggregate(HS_Números$Accuracy, list(HS_Números$Structure), FUN = mean, na.rm = TRUE)
HS_Números_Average <- HS_Números_Average %>% rename(Structure_Avg = x)
HS_Números_Average <- left_join(HS_Números, HS_Números_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## HS, feminine
HS_Cartas_Average <- aggregate(HS_Cartas$Accuracy, list(HS_Cartas$Structure), FUN = mean, na.rm = TRUE)
HS_Cartas_Average <- HS_Cartas_Average %>% rename(Structure_Avg = x)
HS_Cartas_Average <- left_join(HS_Cartas, HS_Cartas_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Guitarra_Average <- aggregate(HS_Guitarra$Accuracy, list(HS_Guitarra$Structure), FUN = mean, na.rm = TRUE)
HS_Guitarra_Average <- HS_Guitarra_Average %>% rename(Structure_Avg = x)
HS_Guitarra_Average <- left_join(HS_Guitarra, HS_Guitarra_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Letras_Average <- aggregate(HS_Letras$Accuracy, list(HS_Letras$Structure), FUN = mean, na.rm = TRUE)
HS_Letras_Average <- HS_Letras_Average %>% rename(Structure_Avg = x)
HS_Letras_Average <- left_join(HS_Letras, HS_Letras_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Película_Average <- aggregate(HS_Película$Accuracy, list(HS_Película$Structure), FUN = mean, na.rm = TRUE)
HS_Película_Average <- HS_Película_Average %>% rename(Structure_Avg = x)
HS_Película_Average <- left_join(HS_Película, HS_Película_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Piñata_Average <- aggregate(HS_Piñata$Accuracy, list(HS_Piñata$Structure), FUN = mean, na.rm = TRUE)
HS_Piñata_Average <- HS_Piñata_Average %>% rename(Structure_Avg = x)
HS_Piñata_Average <- left_join(HS_Piñata, HS_Piñata_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_Uñas_Average <- aggregate(HS_Uñas$Accuracy, list(HS_Uñas$Structure), FUN = mean, na.rm = TRUE)
HS_Uñas_Average <- HS_Uñas_Average %>% rename(Structure_Avg = x)
HS_Uñas_Average <- left_join(HS_Uñas, HS_Uñas_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2L, masculine
L2L_Instrumentos_Average <- aggregate(L2L_Instrumentos$Accuracy, list(L2L_Instrumentos$Structure), FUN = mean, na.rm = TRUE)
L2L_Instrumentos_Average <- L2L_Instrumentos_Average %>% rename(Structure_Avg = x)
L2L_Instrumentos_Average <- left_join(L2L_Instrumentos, L2L_Instrumentos_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Lapicero_Average <- aggregate(L2L_Lapicero$Accuracy, list(L2L_Lapicero$Structure), FUN = mean, na.rm = TRUE)
L2L_Lapicero_Average <- L2L_Lapicero_Average %>% rename(Structure_Avg = x)
L2L_Lapicero_Average <- left_join(L2L_Lapicero, L2L_Lapicero_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Libro_Average <- aggregate(L2L_Libro$Accuracy, list(L2L_Libro$Structure), FUN = mean, na.rm = TRUE)
L2L_Libro_Average <- L2L_Libro_Average %>% rename(Structure_Avg = x)
L2L_Libro_Average <- left_join(L2L_Libro, L2L_Libro_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Libros_Average <- aggregate(L2L_Libros$Accuracy, list(L2L_Libros$Structure), FUN = mean, na.rm = TRUE)
L2L_Libros_Average <- L2L_Libros_Average %>% rename(Structure_Avg = x)
L2L_Libros_Average <- left_join(L2L_Libros, L2L_Libros_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Muñeco_Average <- aggregate(L2L_Muñeco$Accuracy, list(L2L_Muñeco$Structure), FUN = mean, na.rm = TRUE)
L2L_Muñeco_Average <- L2L_Muñeco_Average %>% rename(Structure_Avg = x)
L2L_Muñeco_Average <- left_join(L2L_Muñeco, L2L_Muñeco_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Números_Average <- aggregate(L2L_Números$Accuracy, list(L2L_Números$Structure), FUN = mean, na.rm = TRUE)
L2L_Números_Average <- L2L_Números_Average %>% rename(Structure_Avg = x)
L2L_Números_Average <- left_join(L2L_Números, L2L_Números_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2L, feminine
L2L_Cartas_Average <- aggregate(L2L_Cartas$Accuracy, list(L2L_Cartas$Structure), FUN = mean, na.rm = TRUE)
L2L_Cartas_Average <- L2L_Cartas_Average %>% rename(Structure_Avg = x)
L2L_Cartas_Average <- left_join(L2L_Cartas, L2L_Cartas_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Guitarra_Average <- aggregate(L2L_Guitarra$Accuracy, list(L2L_Guitarra$Structure), FUN = mean, na.rm = TRUE)
L2L_Guitarra_Average <- L2L_Guitarra_Average %>% rename(Structure_Avg = x)
L2L_Guitarra_Average <- left_join(L2L_Guitarra, L2L_Guitarra_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Letras_Average <- aggregate(L2L_Letras$Accuracy, list(L2L_Letras$Structure), FUN = mean, na.rm = TRUE)
L2L_Letras_Average <- L2L_Letras_Average %>% rename(Structure_Avg = x)
L2L_Letras_Average <- left_join(L2L_Letras, L2L_Letras_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Película_Average <- aggregate(L2L_Película$Accuracy, list(L2L_Película$Structure), FUN = mean, na.rm = TRUE)
L2L_Película_Average <- L2L_Película_Average %>% rename(Structure_Avg = x)
L2L_Película_Average <- left_join(L2L_Película, L2L_Película_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Piñata_Average <- aggregate(L2L_Piñata$Accuracy, list(L2L_Piñata$Structure), FUN = mean, na.rm = TRUE)
L2L_Piñata_Average <- L2L_Piñata_Average %>% rename(Structure_Avg = x)
L2L_Piñata_Average <- left_join(L2L_Piñata, L2L_Piñata_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_Uñas_Average <- aggregate(L2L_Uñas$Accuracy, list(L2L_Uñas$Structure), FUN = mean, na.rm = TRUE)
L2L_Uñas_Average <- L2L_Uñas_Average %>% rename(Structure_Avg = x)
L2L_Uñas_Average <- left_join(L2L_Uñas, L2L_Uñas_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Rejoin and prepare for graph
## HS data
HS_Masculine <- rbind(HS_Instrumentos_Average, HS_Lapicero_Average, HS_Libro_Average, HS_Libros_Average, HS_Muñeco_Average, HS_Números_Average) %>% 
  mutate(Group = "Heritage speakers")

HS_Feminine <- rbind(HS_Cartas_Average, HS_Guitarra_Average, HS_Letras_Average, HS_Película_Average, HS_Piñata_Average, HS_Uñas_Average) %>% 
  mutate(Group = "Heritage speakers")


## L2L data
L2L_Masculine <- rbind(L2L_Instrumentos_Average, L2L_Lapicero_Average, L2L_Libro_Average, L2L_Libros_Average, L2L_Muñeco_Average, L2L_Números_Average) %>% 
  mutate(Group = "L2 learners")

L2L_Feminine <- rbind(L2L_Cartas_Average, L2L_Guitarra_Average, L2L_Letras_Average, L2L_Película_Average, L2L_Piñata_Average, L2L_Uñas_Average) %>% 
  mutate(Group = "L2 learners")


## Aggregated data
Masculine <- rbind(HS_Masculine, L2L_Masculine)
Feminine <- rbind(HS_Feminine, L2L_Feminine)

Master <- rbind(Masculine, Feminine)
Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))


# Generate graphs
## Masculine
Masculine_Graph <- Masculine %>% 
  ggplot(aes(x = Item, y = Structure_Percentage, fill = Group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  labs(x = "Noun", y = "Percentage of expected gender morphology", title = "Clitic Gender Selection by Item (Masculine Nouns)") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Masculine_Graph

ggsave(filename = here("Gender Comprehension Manuscript (Frontiers)", "Graphs", "Frontiers Gender Figure 3.pdf"),
       plot = Masculine_Graph,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


## Feminine
Feminine_Graph <- Feminine %>% 
  ggplot(aes(x = Item, y = Structure_Percentage, fill = Group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  labs(x = "Noun", y = "Percentage of expected gender morphology", title = "Clitic Gender Selection by Item (Feminine Nouns)") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Feminine_Graph

ggsave(filename = here("Gender Comprehension Manuscript (Frontiers)", "Graphs", "Frontiers Gender Figure 4.pdf"),
       plot = Feminine_Graph,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")

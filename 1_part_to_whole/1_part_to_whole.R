library(hrbrthemes)
library(waffle)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(showtext)


data <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayChartChallenge2024/main/1_part_to_whole/1_part_to_whole.csv", dec = ",")

data$answer <- factor(data$answer,
                      levels = c("Mucha frecuencia", "Bastante frecuencia",
                                 "Poca frecuencia","Nunca"))

data$sex <- factor(data$sex,
                      levels = c("Mujeres", "Hombres"))


data$gen_order = factor(data$gen, levels=c("Gen Z", "Millenial", "Gen X", "Boomers-Silenciosa"),
                  labels=c('Gen Z\n(nacidos entre\n 1997 y 2013)',
                           'Millenial\n(nacidos entre\n 1981 y 1996)',
                           'Gen X\n(nacidos entre\n 1965 y 1981)',
                           'Boomers-Silenciosa\n(nacidos entre\n 1928 y 1964)'))


#Fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


chart <- data |>
  ggplot(aes(fill = answer, values = pct)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL,
       title="Miedo de volver a casa sola/o de noche",
       subtitle = "Respuestas a la pregunta “¿Con qué frecuencia tienes miedo al volver sola/o\n a casa de noche?” por género y generación",
       caption = "Fuente: Encuesta 40db. Barómetro de marzo de 2024 / Laura Navarro.") +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  geom_waffle(
    color = "white", size = 0.33,
    make_proportional = TRUE, n_rows = 10,
    radius = unit(2, "pt")
  ) +
  facet_grid(gen_order~sex) +
  theme(legend.position = "top",
        text = element_text(family = "tawa"),
        plot.title = element_text(hjust = 0.5, family = "abril", size =24),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 12, color ="darkgrey",
                                     family = "tawa"),
        plot.caption = element_text(color = "grey", family = "tawa", hjust = 0.5),
        strip.text.x = element_text(hjust = 0.5, family = "tawa", face ="bold"),
        strip.text.y = element_text(angle = 0, family = "tawa"),
        plot.margin = margin(1,1,1.5,1.2, "cm")
        ) +
  scale_fill_manual(
    values = c("#FF9B50", "#F4DFB6","#8ECDDD", "#22668D" ))



chart

# save plot


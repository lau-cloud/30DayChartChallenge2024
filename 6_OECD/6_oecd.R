library(tidyverse)
library(scales)
library(ggiraph)


#Fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


data <- read.csv("6_oecd.csv")
paises <- read.csv("6_country_codes.csv")


data_clean <- data |> 
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) |>
  right_join(paises, by = "REF_AREA") |> 
  mutate(
    data_id = paste0(nombre_pais, TIME_PERIOD),
    tooltip = paste0(
      "<b>", as.character(nombre_pais),"</b>","<br>",
      "<b>", round(OBS_VALUE * 100, 1),"</b>", "%<br>"
    ))

p1 <- ggplot() + 
  geom_line_interactive(data = data_clean, aes( x = TIME_PERIOD, y = OBS_VALUE, group = REF_AREA),
            color = "#5755FE", size = 0.4, alpha=0.2) +
  
  geom_line_interactive(data = subset(data_clean, REF_AREA %in% c("ESP")), 
            aes( x = TIME_PERIOD, y = OBS_VALUE, group = REF_AREA),
            color = "#5755FE", size = 0.8) +
  
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022)) +
  theme_minimal() +
  labs(fill = NULL, colour = NULL,
       title="Antes de ir al médico, consultamos en Google",
       subtitle = "Porcentaje de la población que utiliza Internet para buscar información\n de salud (en los últimos 3 meses), por páis de la OCDE",
       caption = "Fuente: OCDE. / Laura Navarro") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "tawa"),
    plot.title = element_text(hjust = 0.5, family = "abril", size =22),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 12, color ="darkgrey",
                                 family = "tawa",
                                 margin = margin(0,0,10,0)),
    plot.caption = element_text(color = "grey", family = "tawa", hjust = 0.5, size=12,
                                margin = margin(20,0,0,0)),
    plot.margin = margin(1,1,1.5,1.2, "cm"),
    axis.text.x = element_text(size=12, color="darkgrey"),
    axis.text.y = element_text(size=12, color="darkgrey", face="bold")) +
    geom_vline(xintercept=2020, color="black", lty = 2) +
  
    
    #annotations  
    annotate("text", x = c(2023), y = c(72), 
               label = c("España") , color="#5755FE", 
               size=4.3, family="tawa", hjust = 1, fontface="bold") +
  annotate("text", x = c(2020.2), y = c(15), 
           label = c("Covid-19") , color="black", 
           size=4.3, family="tawa", hjust = 0)

p1


# save
ggsave("6_oecd.pdf",
       p1,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)  

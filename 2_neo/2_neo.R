library(tidyverse)
library(showtext)
library(cowplot)

#Fonts
font_add_google("DM Serif Display", "abril")
#font_add_google("Courier Prime", "courier")
font_add_google("Tajawal", "tawa")
showtext_auto()

#data
data <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayChartChallenge2024/main/2_neo/2_neo.csv", dec = ",")

#clean
data_clean <- data |> 
  pivot_longer(cols =starts_with("X"), 
               names_to = "year",
               values_to = "muertes") |> 
  mutate(year = gsub("X", "", year)) |> 
  filter(bound=="Median") |> 
  filter(!region_name %in% c("Europe and Northern America",
                             "Eastern and South-Eastern Asia",
                             "Northern Africa and Western Asia",
                             "Central and Southern Asia",
                             "Least developed countries",
                             "Oceania",
                             "South-Eastern Asia")) |> 
  arrange(muertes, year) |>
  mutate(region_name = factor(region_name, unique(region_name),
                              labels=c("Australia y Nueva Zelanda",
                                       "Europa",
                                       "Asia oriental",
                                       "Norteamérica",
                                       "América Latina y\n Caribe",
                                       "Asia central",
                                       "Asia occidental",
                                       "Norte de África",
                                       "Oceanía (excluyendo\n Australia y Nueva Zelanda)",
                                       "Estados insulares en\n vías de desarrollo",
                                       "Asia meridional",
                                       "Países en vías\n de desarrollo",
                                       "África Sub-sahariana",
                                       "Mundo")))

data_clean$year <- as.numeric(data_clean$year)




#plot World
world <- data_clean |> 
  filter(region_name=="Mundo") |> 
  ggplot(aes(x = year, y = muertes)) +
  geom_area(fill = "#F4CE14") +
  geom_line(color = "black", size=1) +
  ylim(0,500) +
  labs(y="", x="",
       title= "Mundo") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color= "darkgrey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "tawa", size = 16, hjust = 0.5, face="bold",
                              margin = margin(0,0,20,0)),
    plot.margin = margin(0, 250, 0, 250)
  )

world






#plot regions
regions <- data_clean |> 
  filter(region_name !="Mundo") |> 
  ggplot(aes(x = year, y = muertes)) +
  geom_area(fill = "#F4CE14") +
  geom_line(color = "black", size=1) +
  facet_wrap(.~region_name) +
  ylim(0,600) +
  labs(y="", x="", caption = "Fuente: Banco Mundial | Laura Navarro.") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color= "darkgrey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "tawa", size = 20, hjust = 0.5,
                              margin = margin(0,0,20,0)),
    plot.margin = margin(0, 20, 10, 20),
    strip.text.x = element_text(hjust = 0.5, family = "tawa",
                                size= 10),
    plot.caption = element_text(color = "grey", family = "tawa", hjust = 0.5,
                                size = 14)
  )

regions

#title
title <- ggdraw() + 
  draw_label("Cosas que mejoran: la mortalidad neonatal\n más baja de la historia",
             fontfamily = "abril", size = 24, hjust = 0.5, fontface = "bold") +
  draw_label("Tasa de mortalidad neonatal (por cada 1.000 nacimientos)",
             fontfamily = "tawa", size = 15, color = "darkgrey",
             y = -0.05, hjust = 0.5) +
  theme(plot.margin = margin(0, 0, 35, 7))



#merge together
plot_row <- plot_grid(world, regions
                      ,ncol = 1,
                     rel_heights = c(0.4, 1)
                    )


plot_grid <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)
plot_grid

# save
ggsave("2_neo.pdf",
       plot_grid,
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)

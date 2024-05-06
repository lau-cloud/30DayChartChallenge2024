#Code based on Milos Popovic tutorial (https://milospopovic.net/6-ways-to-map-population-with-r.r/)

# libraries we need
library(tidyverse)
library(sf)
library(classInt)
library(cartogram)
library(showtext)


# Loading fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


# Loading geojson of Barcelona's neighborhoods
barris <- st_read("0301040100_Barris_UNITATS_ADM.json",
                  stringsAsFactors = FALSE, 
                  as_tibble = TRUE)

# Loading geojson of Barcelona's boundary
perfil <- st_read("0301040100_TermeMunicipal_UNITATS_ADM.json",
                  stringsAsFactors = FALSE, 
                  as_tibble = TRUE)


# Data from Barcelona's population census

pop_barris <- read.csv("10_physical.csv")

names(barris)[29] <- "barri"

# merge shp and data.frame
df <- barris |>
  left_join(pop_barris, by = "barri")




############################## DOT DENSITY MAP

#divide number by 100 (each dot is 100 people)
df_100 <- df |> 
  mutate(total_100 = total/100)
  
# Apply Milos function
get_dot_density <- function() {
  num_dots <- ceiling(dplyr::select(as.data.frame(df_100), total_100))
  deu_dots <- map_df(
    names(num_dots),
    ~ sf::st_sample(df_100, size = num_dots[, .x], type = "random") |>
      sf::st_cast("POINT") |>
      sf::st_coordinates() |>
      as_tibble() |>
      setNames(c("long", "lat"))
  )
  return(deu_dots)
}

deu_dots <- get_dot_density()


##plot density map
map_plot <-
    ggplot(deu_dots) +
    geom_point(
      data = deu_dots, aes(x = long, y = lat),
      color = "#A0153E", size = .7, alpha = .3
    ) +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
  geom_sf(
    data = barris, fill = "transparent",
    color = "white", linewidth = .6) +
  theme_minimal() +
  geom_sf(data = perfil, fill="transparent", color="black", linewidth=0.6) +
  labs(fill = NULL, colour = NULL,
       title="¿En qué barrios de Barcelona hay\n más densidad de población?",
       subtitle = "Personas por km2. Cada punto representa a 100 personas",
       caption = "Fuente: OMD. / Laura Navarro") +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "abril", size =22,
                              lineheight = 1.1,
                              margin = margin(10,0,10,0)),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 12, color ="darkgrey"),
    plot.caption = element_text(color = "grey", hjust = 0.7, size=12)
  ) +
  annotate("text", x = c(2.083), y = c(41.3728), 
           label = c("Badal") , color="black", 
           size=5, family="tawa", hjust = 0.5, fontface="bold") +
  annotate(geom = "curve", x = 2.085, y = 41.37, xend = 2.123, yend = 41.373, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )

map_plot


# save
ggsave("10_physical.pdf",
       map_plot,
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)

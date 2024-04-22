library(tidyverse)
library(climaemet)
library(sf)
library(mapSpain) # para los límites de las provincias
library(terra)
library(showtext) # para las tipos


#Fonts
font_add_google("DM Serif Display", "abril")
#font_add_google("Courier Prime", "courier")
font_add_google("Tajawal", "tawa")
showtext_auto()

#llamada a la API de aemet para extraer los municipios
munis <- aemet_munic

# Filtramos la tabla "munis" para quedarnos con los municipios de cataluña
cat <- aemet_munic %>%
  filter(codauto_nombre == "Cataluña")  |>   
  pull(municipio)

#hacemos la llamada a la API con "aemet_forecast_daily(), 1:30 horas de llamada 
#daily <- aemet_forecast_daily(cat)

# comprobamos las variables disponibles
aemet_forecast_vars_available(daily)

# Seleccionamos variable de precipitacion y desanidamos
prec_diaria <- aemet_forecast_tidy(daily, "probPrecipitacion")

#### Cargamos GEOJSON
my_sf <- read_sf("recintos_municipales.json")

#Filtramos solo Cataluña y extraemos los últimos 5 dígitos del código
sf_cataluna <- my_sf |> 
  filter(CODNUT2 == "ES51") |> 
  mutate(
    municipio = str_sub(INSPIREID, -5)
  )

#Cargamos provincias del paquete "mapspain" y filtramos solo catalanas
cat_prov <- esp_get_prov(moveCAN = FALSE) |> 
  filter(nuts2.code == "ES51")

# Hacemos el merge y filtramos las fechas
cat_merged2 <- sf_cataluna |> 
  left_join(prec_diaria, by = c("municipio")) |> 
  filter(fecha > "2024-04-21" & fecha < "2024-04-25")

#Cambiamos el texto de los grids (los strip text)
supp.labs <- c("Avui, dilluns 22", "Demà, dimarts 23\nSant Jordi", "Demà passat, dimecres 24")
names(supp.labs) <- c("2024-04-22","2024-04-23","2024-04-24")

###Visualizamos
p_precipitacion <- ggplot(cat_merged2) +
  geom_sf(aes(fill = probPrecipitacion), color = NA, linewidth = 0.01) +
  geom_sf(data = cat_prov, fill = NA, color = "white", size = 0.5) +
  facet_wrap(~fecha, ncol=3,
             labeller = labeller(fecha = supp.labs)) +
  theme_void() +
  scale_fill_gradient2(
    low = "#EEF5FF",
    mid = "#B4D4FF",
    high = "#176B87",
    midpoint = 50,
    na.value = "grey",
    name="% de probabilitat",
   # breaks=c(0,25, 50, 75,100),
    guide = guide_legend(keyheight = unit(1.5, units = "mm"), 
                         keywidth=unit(12, units = "mm"), 
                         label.position = "bottom", 
                         title.position = 'top', nrow=1)) +
  labs(
    title = "On plourà per Sant Jordi?",
    subtitle = "Probabilitat de precipitació del 22 al 24 d'abril, per municipi de Catalunya",
    caption = "Dades: Aemet Open Data / Laura Navarro"
  ) +
  theme(
    text = element_text(family = "tawa"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, family = "abril", size =24, lineheight=.7),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 12, color ="darkgrey",
                                 margin = margin(10,0,15,0)),
    plot.caption = element_text(color = "grey", hjust = 0.5, size=12,
                                margin = margin(30,0,30,0)),
    strip.text.x = element_text(
      margin = margin(20,0,0,0),
      size=12
    ),
   # plot.margin = margin(1,1,1.5,1.2, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) + 
  annotate("text", label = "Barcelona", size = 3, x = 2.20, y = 41.25,
           color = "#C3C3C3", hjust = 0) +
  annotate("point", size = 1.2, x = 2.1, y = 41.3,
           color = "#929292", shape = 21, fill = "white", stroke =0.8) +
  annotate("text", label = "Girona", size = 3, x = 2.89, y = 41.85,
           color = "#C3C3C3", hjust = 0) +
  annotate("point", size = 1.2, x = 2.8, y = 41.9,
           color = "#929292", shape = 21, fill = "white", stroke =0.8) +
  annotate("text", label = "Lleida", size = 3, x = 0.69, y = 41.68,
           color = "#C3C3C3", hjust = 0) +
  annotate("point", size = 1.2, x = 0.6, y = 41.6,
           color = "#929292", shape = 21, fill = "white", stroke =0.8) +
  annotate("text", label = "Tarragona", size = 3, x = 1.27, y = 41.03,
           color = "#C3C3C3", hjust = 0)+
  annotate("point", size = 1.2, x = 1.2, y = 41.1,
           color = "#929292", shape = 21, fill = "white", stroke =0.8)
  
  

p_precipitacion

#guardamos
ggsave("16_weather.pdf",
       p_precipitacion,
       width = 10,
       height = 7,
       units = "in",
       dpi = 300)

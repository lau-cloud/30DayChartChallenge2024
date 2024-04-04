library(tidyverse)
library(showtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridtext)


#data
data <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayChartChallenge2024/main/3_makeover/3_makeover.csv")


##coast line
world <- ne_coastline(scale = "medium", returnclass = "sf")


#Fonts
font_add_google("DM Serif Display", "abril")
#font_add_google("Courier Prime", "courier")
font_add_google("Tajawal", "tawa")
showtext_auto()

data_clean <- data |> 
  select(Incident.Year, Month, Number.of.Dead, Minimum.Estimated.Number.of.Missing, lat, long) |> 
  rename(dead = "Number.of.Dead",
         missing = "Minimum.Estimated.Number.of.Missing",
         year = "Incident.Year") |> 
  pivot_longer(dead:missing, names_to = "tipo", values_to = "number")

data_2024 <- data_clean |> 
  filter(year==2024)



# Left chart
plot <- ggplot() +
  geom_sf(data=world, aes(), color="#AFAFAF") +
  geom_point(data=data_clean, aes(x=long, y=lat, size=number), alpha=0.6, color = "#E74646") +
  geom_point(data=data_2024, aes(x=long, y =lat, size=number), color="black") +
  xlim(-12,37) + 
  ylim(29,43) +
  coord_sf() +
  scale_color_manual(values = c("#E74646")) +
  labs(title="29.313",
       subtitle="Migrantes fallecidos o desaparecidos en el Mediterráneo desde 2014. Los datos\nrepresentan estimaciones mínimas y las ubicaciones son aproximadas.\nEn negro, el año 2024",
       caption = "Fuente: Missing Migrants Project | Laura Navarro",
       size = "Muertos y desaparecidos\nen cada incidente:") +
  scale_size(range=c(0.3,4),
             breaks=c(0,200,400,600)) +
  theme_void() + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF7FC", color="#FFF7FC"),
    plot.title = element_text(family = "abril", size = 21, hjust = 0.5,
                              margin = margin(20,0,10,0)),
    plot.subtitle = element_text(family="tawa", hjust=0.5, margin = margin(0,0,30,0),
                                 size=12),
    plot.margin = margin(0, 20, 10, 20),
    plot.caption = element_text(color = "darkgrey", family = "tawa", hjust = 0.5,
                                size = 12,
                                margin = margin(30,0,0,0)),
    legend.title = element_text(),
    legend.box="horizontal"
  ) +
  guides(color=guide_legend(title="Muertos y desaparecidos")) +

  annotate("text", x = c(-4), y = c(40.5), 
            label = c("E S P A Ñ A") , color="#757575", 
            size=4, family="tawa") +
  annotate("text", x = c(2), y = c(35), 
           label = c("A R G E L I A") , color="#757575", 
           size=4, family="tawa") +
  annotate("text", x = c(32), y = c(39), 
           label = c("T U R Q U Í A") , color="#757575", 
           size=4, family="tawa") +
  annotate("text", x = c(21), y = c(37), 
           label = c("G R E C I A") , color="#757575", 
           size=4, family="tawa") +
  annotate("text", x = c(9), y = c(36), 
           label = c("T Ú N E Z") , color="#757575", 
           size=4, family="tawa") +
  annotate("text", x = c(12), y = c(31), 
           label = c("L I B I A") , color="#757575", 
           size=4, family="tawa")
plot

# save
ggsave("3_makeover.pdf",
       plot,
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)  

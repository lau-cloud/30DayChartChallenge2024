library(tidyverse)
library(climaemet)
library(scales)
library(RColorBrewer)
library(showtext)
library(ggpubr)


#fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


#browseURL("https://opendata.aemet.es/centrodedescargas/obtencionAPIKey")


#get stations id
stations <- aemet_stations()
head(stations)

#read historical data for a particular station
station <- "0076" # BCN Observatori Fabra
bcn_daily <-
  aemet_daily_period(station, start = 1980, end = 2024) #años que nos interesan



#Create column with year and colum with day of the year
bcn_daily$year <- format(as.Date(bcn_daily$fecha, format="%Y-%m-%d"),"%Y")
bcn_daily$dia_year <- yday(bcn_daily$fecha)
bcn_daily$year <- as.numeric(bcn_daily$year)

#if tropical night, add 1, if not, a 0
bcn_daily$ntrop <- ifelse(bcn_daily$tmin >= 20, 1, 0)


#Analysis: how many tropical nights by year

#bcn_tropicales <- bcn_daily |> 
#  filter(ntrop == "1") |> 
#  group_by(year) |> 
#  summarize(tropicales = sum(ntrop))


#Tropical nights to factor to plot it
bcn_daily$ntrop <- as.factor(bcn_daily$ntrop)

#If torrida night, add 1, if not, a 0
bcn_daily$ntorr <- ifelse(bcn_daily$tmin >= 25, 1, 0)
bcn_daily$ntorr <- as.factor(bcn_daily$ntorr)

##filter only tropical nights to plot them
bcn_tropicales <- bcn_daily |> 
  filter(ntrop == "1")

#and filter torridas too
bcn_torridas <- bcn_daily |> 
  filter(ntorr == "1")


#plot
plot <- ggplot()+
  geom_tile(data = subset(bcn_daily, ntrop != "1"), aes(dia_year, year, fill= tmin)) + #color gradient only for days without tropical (and torrida) nights
  geom_tile(data=bcn_tropicales, aes(dia_year, year), fill = "#FF204E") + #plot only tropicals with a specific color
  geom_tile(data=bcn_torridas, aes(dia_year, year), fill = "#3C0753") + #plot the torridas at the top, with a specific color
  scale_x_continuous(position = "top",
                     limits = c(0, 365),
                     breaks = c(0,214,365),
                     labels = c("1 de enero", "1 de agosto", "31 de diciembre")) +
  theme_minimal() +
  labs(fill = NULL, colour = NULL,
       title="Noches tropicales y tórridas\n cada vez más comunes",
       subtitle = "Días con mínimas que no bajan de los 20ºC (tropicales) o 25ºC (tórridas)\nen la estación de Barcelona-El Prat",
       caption = "Fuente: AEMET open Data. / Laura Navarro") +
  scale_fill_gradient2(
    low = "white",
    high = "#ff7f8c",
    midpoint=8,
    na.value = "white",
    guide = "colourbar",
    breaks = c(0, 19)
  )  +
  theme(
  text = element_text(family = "tawa"),
legend.position = "top",
plot.title = element_text(hjust = 0.5, family = "abril", size =24, lineheight=.7),
plot.subtitle = element_text(hjust = 0.5,
                             size = 12, color ="darkgrey"),
plot.caption = element_text(color = "grey", hjust = 0.5, size=12,
                            margin = margin(20,0,0,0)),
strip.text.x = element_text(hjust = 0.5, face ="bold"),
strip.text.y = element_text(angle = 0),
plot.margin = margin(1,1,1.5,1.2, "cm"),
panel.grid.major.y = element_blank(),
panel.grid.minor.y = element_blank(),
axis.text.x = element_text(size=12),
axis.title.x = element_blank(),
axis.title.y = element_blank()) +
  geom_vline(xintercept=214, linetype="dotted", color = "black") +   #agosto
  annotate()
plot


# guardar
ggsave("14_heatmap2.pdf",
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)

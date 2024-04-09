#The code is based on this blog post from Duc-Quang Nguyen: https://dqn.website/post/interactive-mekko-charts-in-r/

#libraries
library(tidyverse)
library(showtext)
library(ggiraph)


#fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()


#read the data
fedes <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayChartChallenge2024/main/5_diverging/5_diverging.csv", encoding = "UTF-8")


#calculate percentages and put data in a long format
fedes_clean <- fedes |> 
  mutate(diferencia = ((mujeres/total)*100)-((hombres/total)*100)) |>  #this is for arranging later
  pivot_longer(cols=c('hombres', 'mujeres'), names_to = "sexo", values_to = "federados") |> 
  mutate(proporcion = federados / total)


##Uppercase first letter
fedes_clean$deporte <- str_to_sentence(fedes_clean$deporte, locale = "es")


#create the rectangles for the mekko
fedes_mosaic <- fedes_clean |>
  group_by(deporte) |> 
  mutate(
    prop_federados = federados / sum(federados),
    tot_group = sum(federados)) |>
  ungroup() |> 
  arrange(diferencia)

fedes_mosaic2 <- fedes_mosaic  |> 
  group_by(sexo)  |> 
  arrange(desc(diferencia))  |> 
  mutate(
    ymax = cumsum(tot_group) / sum(tot_group),
    ymin = (ymax - (tot_group/sum(tot_group)))
  )  |>  ungroup()  |> 
  group_by(deporte)  |> 
  arrange(desc(sexo))  |> 
  mutate(xmax = cumsum(prop_federados), xmin = xmax - prop_federados) |> 
  ungroup()  |> 
  arrange() |> 
  mutate(
    data_id = paste0(deporte, sexo), #for interactivity, we need a data_id and a tooltip
    tooltip = paste0(
      "<b>", as.character(deporte),"</b>","<br>",
      sexo, ": ", "<b>", round(proporcion * 100, 1),"</b>", "%<br>",
      "Total federados/as: ",federados,"<br>"
    )
  )

#plot it and save it to a variable
p1 <- ggplot(fedes_mosaic2) +
  geom_rect_interactive(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = sexo,
                            data_id = data_id, tooltip = tooltip),
            colour = "white", size = 0.1)+
  labs(fill = NULL, colour = NULL,
       title="Deporte federado en España",
       subtitle = "Licencias federadas según sexo. El ancho de la barra\n indica el peso de cada deporte sobre el total",
       caption = "Fuente: Consejo Superior de Deportes, año 2022. / Laura Navarro") +
  theme_minimal() +
  scale_fill_manual(values=c("#D6DAC8", "#824D74")) +
  scale_x_continuous(labels=c("0.00" = "0%", "0.25" = "25%", "0,50" = "50%", "0.75"="75%",
                            "1.00" = "100%")) +
  theme(
    text = element_text(family = "tawa"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, family = "abril", size =24),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 12, color ="darkgrey"),
    plot.caption = element_text(color = "grey", hjust = 0.5, size=12,
                                margin = margin(20,0,0,0)),
    strip.text.x = element_text(hjust = 0.5, face ="bold"),
    strip.text.y = element_text(angle = 0),
    plot.margin = margin(1,1,1.5,1.2, "cm"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  annotate("text", x = c(0.50), y = c(0.75), 
           label = c("Fútbol") , color="black", 
           size=5, family="tawa", hjust = 0.5)+
  annotate("text", x = c(0.50), y = c(0.96), 
           label = c("Caza") , color="black", 
           size=5, family="tawa", hjust = 0.5) +
  annotate("text", x = c(0.50), y = c(0.45), 
           label = c("Golf") , color="black", 
           size=5, family="tawa", hjust = 0.5) +
  annotate("text", x = c(0.50), y = c(0.29), 
           label = c("Baloncesto") , color="black", 
           size=5, family="tawa", hjust = 0.5) +
  annotate("text", x = c(0.53), y = c(0.20), 
           label = c("Montaña y escalada") , color="black", 
           size=5, family="tawa", hjust = 0.5) +
  annotate("text", x = c(0.85), y = c(0.29), 
           label = c("Gimnasia") , color="black", fontface="bold", 
           size=5, family="tawa", hjust = 0.5) +
  annotate(geom = "curve", x = 0.85, y = 0.25, xend = 0.85, yend = 0.03, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )


#interactivity
plot <- girafe(ggobj=p1,
       fonts = list(sans = "Helvetica"),
       options = list(
         opts_hover(css = "stroke-width:1.5;stroke:black;"),
         opts_selection(only_shiny = FALSE, type = "single", css = "stroke:black;"),
         opts_tooltip(offx = 20, offy = -10,
                      use_fill = FALSE, use_stroke = TRUE,
                      delay_mouseout = 1000,
                      css = "background:white;font-family: Helvetica;font-size:12px;padding:3pt;border-radius:5px"),
         opts_sizing(rescale = FALSE)),
       width_svg = 7.5,
       height_svg = 9)
plot

#save it as html widget
htmltools::save_html(plot, "plot.html")


# Load necessary libraries
library(ggplot2)
library(showtext)

#Fonts
font_add_google("DM Serif Display", "abril")
#font_add_google("Courier Prime", "courier")
font_add_google("Tajawal", "tawa")
showtext_auto()


data <- read.csv("https://raw.githubusercontent.com/lau-cloud/30DayChartChallenge2024/main/26_ai/fig_2_1_16.csv")

# Data
ai_performance_data <- data |> 
  rename(Performance = "Perfomance.relative.to.the.human.baseline") |> 
  mutate(Task1 = case_when(
    Task == 'Image classification' ~ 'Clasificación de imágenes',
    Task == "Competition-level mathematics" ~"Matemáticas nivel competición",
    Task == "Multitask language understanding" ~ "Comprensión del lenguaje multitarea",
    Task == "Basic-level reading comprehension"~"Comprensión lectora nivel básico",
    Task == "Medium-level reading comprehension"~"Comprensión lectora de nivel medio",
    Task == "English language understanding"~"Comprensión del inglés",
    Task == "Visual commonsense reasoning"~"Razonamiento visual de sentido común",
    Task == "Visual reasoning" ~"Razonamiento visual",
    Task == "Natural language inference"~"Inferencia del lenguaje natural")
  )
  


# Create the line chart
p <- ggplot(ai_performance_data, aes(x = Year, y = Performance, color = Task1)) +
  geom_line(size = 0.9) +
  geom_point(size=1) +
  scale_x_continuous(limits = c(2012, 2024), breaks = seq(2012, 2025, by = 2)) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.20)) +
  labs(title = "Capacidades de la IA comparadas\n con un ser humano",
       subtitle = "Desempeño técnico de la IA tomando como referencia las capacidades humanas (índice 1).\n
       Cuando se supera la línea negra, la IA saca mejor puntuación que un humano",
       caption = "Datos: Universidad de Stanford / Laura Navarro") +
  theme_minimal() +
  theme(
    text = element_text(family = "tawa", size = 12), # Set font for axis labels and legend
    plot.title = element_text(family = "abril", size = 24, hjust = 0.5, lineheight = 0.8), # Set font for title to serif and center the title
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 14, color ="darkgrey",
                                 margin = margin(8,0,20,0),
                                 lineheight = 0.4),
    panel.grid.major.y = element_line(color = "#dcdcdc"), # Set major gridlines color
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.title = element_blank(), # Remove axis titles
    axis.text = element_text(color = "grey"), # Set axis text color
    legend.position = "top", # Position legend at the bottom
    legend.title = element_blank(),
    plot.caption = element_text(color = "grey", hjust = 0.5, size=12,
                                margin = margin(20,0,0,0),)
  ) +
  scale_color_manual(values = c("#ff2500","#ffbeb3", "#01a08a","#9AE3D9", "#f2ad00", "#fade99",
                                "#f98400","#B6F0FF", "#5bbcd6")) +
  guides(color=guide_legend(ncol=3))

p
# Add dotted line at 100%
p + geom_hline(yintercept = 1,size=0.5) +
  annotate("text", x = 2012, y = 1, label = "Ser humano", vjust = -0.7, hjust = 0.15,
           family = "tawa", fontface = "bold") +
  annotate("text", x = 2021, y = 0.07, label = "GPT-2 (1.5B)", hjust = -0.2, vjust= 0.4,
           family = "tawa", color= "#f98400") +
  annotate("text", x = 2022, y = 0.57, label = "GPT-4 model", hjust = -0.2,
           family = "tawa", color= "#f98400", lineheight = 0.8) +
  annotate("text", x = 2023, y = 0.9, label = "GPT-4-code\n model", hjust = -0.13,
           family = "tawa", color= "#f98400", lineheight = 0.8) +
  annotate("text", x = 2018.8, y = 0.31, label = "GPT-2 (1.5B)\n afinado", hjust = 1,
           family = "tawa", color= "#01a08a", lineheight = 0.8) +
  annotate("text", x = 2023, y = 1.0, label = "Gemini Ultra", hjust = 0.5, vjust= -1.2,
           family = "tawa", color= "#01a08a", lineheight = 0.8)



# guardar
ggsave("26_ai.pdf",
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)

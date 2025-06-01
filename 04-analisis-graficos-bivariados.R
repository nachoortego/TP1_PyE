# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos)

###############################################################
# Boxplot comparativo cant de menores y uso de instalaciones #
##############################################################

datos %>%
  mutate(
    uso_espacios_verdes = factor(uso_espacios_verdes, levels = c("No hago uso","Al menos una vez por semana", "Diario"))
  ) %>%
  group_by(uso_espacios_verdes, cant_menores) %>%
  summarise(cantidad = n()) %>%
  ggplot() +
    aes(x = uso_espacios_verdes, y = cant_menores) +
    geom_boxplot(show.legend = F, fill = "tomato") +
    labs(title = "Distribución de uso de espacios verdes por cantidad de menores por vivienda",
         x = "Espacios Verdes", 
         y = "Cantidad de menores por vivienda") +
    coord_flip() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 40),
      panel.grid.major = element_line(color = "gray", size = 1),
      panel.grid.minor = element_line(color = "lightgray", size = 1),
      legend.position = "none"
    )+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(breaks = seq(0, max(cant_menores, na.rm = TRUE), by = 1))+
    theme_light()


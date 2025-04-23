# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos)

######################################################################
# GRÁFICO DE BARRA: Proporción promedio de mayores/menores por hogar # 
######################################################################
promedio_menores <- round(cant_menores/cant_integrantes * 100, 2) %>% mean()

data.frame(
  categoria = c("Integrantes del hogar", "Menores"),
  porcentaje = c(100 - promedio_menores, promedio_menores)
) %>%
  ggplot() +
    aes(x = 1, y = porcentaje, fill = categoria) +
    geom_bar(stat = "identity", width = 1) +
    coord_flip() +
    labs(
      title = "Distribución de integrantes mayores y menores a 18 años por vivienda",
      x = "",
      y = "Porcentaje",
      fill = "Categoría"
    ) +
    theme_minimal() +
    scale_fill_brewer(
      palette = "Set2",
      labels = c("Mayores de 18", "Menores de 18")
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.3),
      panel.grid.minor = element_line(color = "lightgray", size = 0.3),
      legend.position = "right",  # Mostramos la leyenda
      aspect.ratio = 1/3
    )

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


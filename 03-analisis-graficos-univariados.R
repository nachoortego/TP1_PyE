# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos)

###############################################################
# BOXPLOT: Tiempo de residencia en la vivienda actual en años # 
###############################################################
datos %>%
ggplot() +
  aes(x = tiempo_residencia, y = "a") +
  geom_boxplot(
    fill = "#69b3a2", 
    color = "#1f3d2e", 
    outlier.shape = 16, 
    outlier.colour = "#D55E00"
  ) +
  labs(
    title = "Tiempo de residencia en la vivienda actual en años", 
    x = "Tiempo de Residencia"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, max(datos$tiempo_residencia, na.rm = TRUE), by = 10)
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 24, color = "#4C4C4C"),
    axis.title.x = element_text(size = 18, color = "#4C4C4C"),
    axis.text.x = element_text(size = 14, color = "#4C4C4C"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_line(color = "lightgray", size = 0.3),
    panel.border = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

####################################################################################
# GRÁFICO DE TORTA: Proporción de los que tienen acceso a espacios de ejercitación # 
####################################################################################
table(acceso_espacios_pc) %>%
  as.data.frame() %>%
  mutate(
    porcentaje = round(100 * Freq / sum(Freq), 1),
    label = paste0(acceso_espacios_pc, "\n", porcentaje, "%")
  ) %>%
  ggplot() +
    aes(x = "", y = Freq, fill = acceso_espacios_pc) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = label), 
      position = position_stack(vjust = 0.5), 
      color = "white", 
      size = 5
    ) +
    labs(title = "Acceso a espacios de prácticas corporales") +
    theme_void() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = -20)),
      legend.position = "none"
    )

###########################################################################
# GRÁFICO DE TORTA: Proporción de los que tienen acceso a espacios verdes # 
###########################################################################
table(acceso_espacios_verdes) %>% 
  as.data.frame() %>%
  mutate(
    porcentaje = round(100 * Freq / sum(Freq), 1),
    label = paste0(acceso_espacios_verdes, "\n", porcentaje, "%")
  ) %>%
  ggplot() +
    aes(x = "", y = Freq, fill = acceso_espacios_verdes) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = label), 
      position = position_stack(vjust = 0.5), 
      color = "white", 
      size = 5
    ) +
    labs(title = "\nAcceso a espacios verdes") +
    theme_void() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = -20)),
      legend.position = "none"
    )
  
####################################################################################
# GRÁFICO DE BARRAS: Cantidad de personas con cada espacio de practicas corporales # 
####################################################################################
frecuencias_pc <- unlist(espacios_pc) [
  unlist(espacios_pc) != "" &
    !is.na(unlist(espacios_pc)) &
    unlist(espacios_pc) != "No existen tales espacios"
] %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  as.data.frame() 

colnames(frecuencias_pc) <- c("espacio", "cantidad")

frecuencias_pc %>%
  ggplot() +
    aes(x = espacio, y = cantidad, fill = espacio) +
    geom_bar(stat = "identity") +
    labs(
      title = "Espacios de prácticas corporales a menos de 500m de la vivienda",
      x = "Espacio",
      y = "Cantidad de personas"
    ) +
    coord_flip() +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#4C4C4C"),
      axis.title.x = element_text(size = 12, color = "#4C4C4C"),
      axis.title.y = element_text(size = 12, color = "#4C4C4C"),
      axis.text.x = element_text(size = 10, color = "#4C4C4C"),
      axis.text.y = element_text(size = 10, color = "#4C4C4C"),
      panel.grid.major = element_line(color = "gray", size = 0.3),
      panel.grid.minor = element_line(color = "lightgray", size = 0.3),
      panel.border = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )

##################################################################
# GRÁFICO DE BARRAS: Cantidad de personas con cada espacio verde # 
##################################################################
frecuencias_espacios_verdes <- unlist(espacios_verdes) [
  unlist(espacios_verdes) != "" &
    !is.na(unlist(espacios_verdes)) &
    unlist(espacios_verdes) != "No existen tales espacios"
] %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  as.data.frame()

colnames(frecuencias_espacios_verdes) <- c("espacio", "cantidad")

frecuencias_espacios_verdes %>%
  ggplot() +
    aes(x = espacio, y = cantidad, fill = espacio) +
    geom_bar(stat = "identity") +
    labs(
      title = "Cantidad de personas con espacios verdes a menos de 500m de la vivienda",
      x = "Tipo de espacio",
      y = "Cantidad de personas"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.3),
      panel.grid.minor = element_line(color = "lightgray", size = 0.3),
      legend.position = "none"
    ) + 
    coord_flip()

##################################################
# GRÁFICO DE TORTA: Acceso a bicicletas publicas # 
##################################################
datos %>%
  group_by(acceso_bp) %>%
  summarise(cantidad = n()) %>%
ggplot() +
  aes(x = "", y = cantidad, fill = acceso_bp) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "¿Tiene acceso al uso de bicicletas públicas?", fill = "Respuesta") +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) ,
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ) 

####################################################
# GRÁFICO DE BARRA: Frecuencias transporte publico # 
####################################################
datos %>%
  mutate(
    frec_tp = factor(frec_tp, 
        levels = 
          c(
            "Menos de 30 minutos entre cada colectivo",
            "1 colectivo cada 30 minutos", 
            "1 colectivo por hora", 
            "1 colectivo cada dos horas"
          )
    )
  ) %>%
  group_by(frec_tp) %>%
  summarise(cantidad = n()) %>%
  ggplot() +
    aes(x = frec_tp, y = cantidad, fill = frec_tp) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "Frecuencia de colectivos por cantidad de personas",
      x = "Frecuencia del transporte",
      y = "Cantidad de personas") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)) +
    coord_flip()


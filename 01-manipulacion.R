# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
colnames(datos)[4] <- "tiempo_residencia"
colnames(datos)[5] <- "cant_integrantes"
colnames(datos)[10] <- "cant_menores"
colnames(datos)[95:103] <- paste0("espacio_pc_", 1:9) #espacios de practicas corporales y esparcimiento
colnames(datos)[104] <- "uso_espacios_pc"
colnames(datos)[105:108] <- paste0("espacio_verde_", 1:4)
colnames(datos)[109] <- "uso_espacios_verdes"
colnames(datos)[110] <- "frec_tp" #frecuencia del transporte publico
colnames(datos)[112] <- "acceso_bp" #tiene acceso al uso de bicicletas publicas

espacios_pc <- datos[, paste0("espacio_pc_", 1:9)]
espacios_verdes <- datos[, paste0("espacio_verde_", 1:4)]



ggplot(frecuencias_espacios_verdes, aes(x = espacio, y = cantidad, fill = espacio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Acceso a espacios de prácticas corporales",
    x = "Espacio",
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
  )
  
################################################################################################
# BOXPLOT:  # 
################################################################################################
ggplot(datos, aes(x = tiempo_residencia, y = "a")) + 
  geom_boxplot(fill = "#69b3a2", color = "#1f3d2e", outlier.shape = 16, outlier.colour = "#D55E00") +
  labs(title = "Distribución del Tiempo de Residencia", x = "Tiempo de Residencia") +
  theme_minimal() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#4C4C4C"),
    axis.title.x = element_text(size = 12, color = "#4C4C4C"),
    axis.text.x = element_text(size = 10, color = "#4C4C4C"),
    panel.grid.major = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_line(color = "lightgray", size = 0.3),
    panel.border = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )







#######################
# BARRAS AGRUPADAS:  # 
######################
espacios_pc_con_info <- cbind(espacios_pc, datos[, c("frec_tp", "acceso_bp")])
espacios_pc_vector <- unlist(espacios_pc)

sin_espacios_pc_v <- espacios_pc_vector[espacios_pc_vector == "No existen tales espacios"]
tabla_sin_espacios_pc <- espacios_pc_con_info %>%
  filter(espacio_pc_8 == "No existen tales espacios") %>%
  group_by(frec_tp, acceso_bp) %>%
  summarise(cantidad = n(), .groups = "drop")

ggplot(tabla_sin_espacios_pc, aes(x = frec_tp, y = cantidad, fill = acceso_bp)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_bar(stat = "identity") +
  labs(
    title = "Uso de transporte público y acceso a bicicleta\n(personas sin acceso a espacios de prácticas corporales)",
    x = "Frecuencia de uso del transporte público",
    y = "Cantidad de personas",
    fill = "¿Tiene bicicleta?"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
  )



################################################################################################
# GRÁFICO DE BARRAS: Frecuencia de uso de espacios verdes en relacion a la cantidad de menores # 
################################################################################################
tabla_frecuencia_menores <- datos %>%
  mutate(
    uso_espacios_verdes = factor(uso_espacios_verdes, levels = c("No hago uso","Al menos una vez por semana", "Diario"))
  ) %>%
  group_by(uso_espacios_verdes) %>%
  summarise(promedio_menores = mean(cant_menores, na.rm = TRUE))

ggplot(tabla_frecuencia_menores, aes(x = uso_espacios_verdes, y = promedio_menores, fill = uso_espacios_verdes)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de Menores por Frecuencia de Uso de Espacios Verdes",
       x = "Frecuencia de Uso de Espacios Verdes",
       y = "Promedio de Menores") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    panel.grid.major = element_line(color = "gray", size = 1),
    panel.grid.minor = element_line(color = "lightgray", size = 1),
    legend.position = "none"
  )+ coord_flip()




###############################################################
# Boxplot comparativo cant de menores y uso de instalaciones #
##############################################################

tabla_frecuencia_menores <- datos %>%
  mutate(
    uso_espacios_verdes = factor(uso_espacios_verdes, levels = c("No hago uso","Al menos una vez por semana", "Diario"))
  ) %>%
  group_by(uso_espacios_verdes, cant_menores) %>%
  summarise(cantidad = n())



ggplot(tabla_frecuencia_menores, aes(x = uso_espacios_verdes, y = cant_menores) ) +
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
  scale_y_continuous(breaks = seq(0, max(tabla_frecuencia_menores$cant_menores, na.rm = TRUE), by = 1))+
theme_light()








##################################################
# GRÁFICO DE TORTA: Acceso a bicicletas publicas # 
##################################################
tabla_acceso_bicis = datos %>%
  group_by(acceso_bp) %>%
  summarise(cantidad = n())
ggplot(
  tabla_acceso_bicis,
  aes(x = "", y = cantidad, fill = acceso_bp)
) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribución del acceso a bicicletas publicas") +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)  
  ) 




####################################################
# GRÁFICO DE BARRA: Frecuencias transporte publico # 
####################################################
tabla_frec_tp <- datos %>%
  mutate(
    frec_tp = factor(frec_tp, levels = c("Menos de 30 minutos entre cada colectivo","1 colectivo cada 30 minutos", "1 colectivo por hora", "1 colectivo cada dos horas"))
  ) %>%
  group_by(frec_tp) %>%
  summarise(cantidad = n())

ggplot(
  tabla_frec_tp, 
  aes(x = frec_tp, y = cantidad, fill = frec_tp)
) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Frecuencia de uso del transporte público",
       x = "Frecuencia del transporte",
       y = "Cantidad de personas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)) 





###########################################################
# GRÁFICO DE BARRA: Frecuencias de uso de espacios verdes # 
###########################################################
tabla_uso_espacios_verdes <- datos %>%
  mutate(
    uso_espacios_verdes = factor(uso_espacios_verdes, levels = c("No hago uso","Al menos una vez por semana", "Diario"))
  ) %>%
  group_by(uso_espacios_verdes) %>%
  summarise(cantidad = n())

ggplot(
  tabla_uso_espacios_verdes,
  aes(x = uso_espacios_verdes, y = cantidad, fill = uso_espacios_verdes)
) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Frecuencia de uso de los espacios verdes",
       x = "Frecuencia",
       y = "Cantidad de personas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "gray", size = 0.3),  # Cuadrícula mayor
    panel.grid.minor = element_line(color = "lightgray", size = 0.3)  # Cuadrícula menor
  )

####################################################################
# GRÁFICO DE BARRA: Frecuencias de uso de espacios de ejercitación # 
####################################################################
tabla_uso_espacios_pc <- datos %>%
  mutate(
    uso_espacios_pc = factor(uso_espacios_pc, levels = c("No hago uso","Al menos una vez por semana", "Diario"))
  ) %>%
  group_by(uso_espacios_pc) %>%
  summarise(cantidad = n())

ggplot(
  tabla_uso_espacios_pc,
  aes(x = uso_espacios_pc, y = cantidad, fill = uso_espacios_pc)
) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Frecuencia de uso de los espacios de ejercitación",
       x = "Frecuencia",
       y = "Cantidad de personas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "gray", size = 0.3),  # Cuadrícula mayor
    panel.grid.minor = element_line(color = "lightgray", size = 0.3)  # Cuadrícula menor
  )
  
ee <- table(uso_espacios_pc)
barplot(ee)

######################################################################
# GRÁFICO DE BARRA: Proporción promedio de mayores/menores por hogar # 
######################################################################
porcentaje_menores_por_hogar <- round(cant_menores/cant_integrantes * 100, 2)
promedio_menores <- mean(porcentaje_menores_por_hogar, na.rm = TRUE)

df_grafico <- data.frame(
  categoria = c("Integrantes del hogar", "Menores"),
  porcentaje = c(100 - promedio_menores, promedio_menores)
)

ggplot(df_grafico, aes(x = 1, y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  coord_flip() + # Barra horizontal
  labs(title = "Distribución de Integrantes y Menores por Hogar",
       x = "",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        aspect.ratio = 1/3)  # Ajustamos la relación de aspecto para hacerla rectangular



####################################################################################
# GRÁFICO DE TORTA: Proporción de los que tienen acceso a espacios de ejercitación # 
####################################################################################
acceso_espacios_pc <- apply(espacios_pc, 1, function(fila) {
  if ("No existen tales espacios" %in% fila) {
    return("No tiene acceso")
  } else if (any(!is.na(fila) & fila != "")) {
    return("Tiene acceso")
  } else {
    return("Sin datos")
  }
})

tabla_acceso_espacios_pc <- table(acceso_espacios_pc)
porcentajes <- round(100 * tabla_acceso_espacios_pc / sum(tabla_acceso_espacios_pc), 1)
labels <- paste(names(tabla_acceso_espacios_pc), "  \n       ", porcentajes, "%      ", sep = "")

pie(tabla_acceso_espacios_pc,
    labels = labels,
    col = c("tomato", "lightgreen"),
    # radius = 0.9,
    main = "Acceso a espacios de esparcimiento")

mtext("Fuente: observatorio villero", side = 1, adj = 0)

###########################################################################
# GRÁFICO DE TORTA: Proporción de los que tienen acceso a espacios verdes # 
###########################################################################
acceso_espacios_verdes <- apply(espacios_verdes, 1, function(fila) {
  if ("No existen tales espacios" %in% fila) {
    return("No tiene acceso")
  } else if (any(!is.na(fila) & fila != "")) {
    return("Tiene acceso")
  } else {
    return("Sin datos")
  }
})

tabla_acceso_espacios_verdes <- table(acceso_espacios_verdes)
porcentajes <- round(100 * tabla_acceso_espacios_verdes / sum(tabla_acceso_espacios_verdes), 1)
labels <- paste(names(tabla_acceso_espacios_verdes), "  \n       ", porcentajes, "%      ", sep = "")

pie(tabla_acceso_espacios_verdes,
    labels = labels,
    col = c("tomato", "lightgreen"),
    # radius = 0.9,
    main = "Acceso a espacios verdes")

mtext("Fuente: observatorio villero", side = 1, adj = 0)



############################################################################
# GRÁFICO DE BARRAS: Cantidad de personas con cada espacio de ejercitación # 
############################################################################
espacios_pc_vector <- unlist(espacios_pc)
espacios_pc_vector <- espacios_pc_vector[
  espacios_pc_vector != "" &
    !is.na(espacios_pc_vector) &
    espacios_pc_vector != "No existen tales espacios"
]

frecuencias_pc <- as.data.frame(sort(table(espacios_pc_vector), decreasing = TRUE))
colnames(frecuencias_pc) <- c("espacio", "cantidad")

ggplot(frecuencias_pc, aes(x = espacio, y = cantidad, fill = espacio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Acceso a espacios de prácticas corporales",
    x = "Espacio",
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
  )

##################################################################
# GRÁFICO DE BARRAS: Cantidad de personas con cada espacio verde # 
##################################################################
espacios_verdes_vector <- unlist(espacios_verdes)
espacios_verdes_vector <- espacios_verdes_vector[
  espacios_verdes_vector != "" &
    !is.na(espacios_verdes_vector) &
    espacios_verdes_vector != "No existen tales espacios"
]

frecuencias_espacios_verdes <- as.data.frame(sort(table(espacios_verdes_vector), decreasing = TRUE))
colnames(frecuencias_espacios_verdes) <- c("espacio", "cantidad")

ggplot(frecuencias_espacios_verdes, aes(x = espacio, y = cantidad, fill = espacio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Acceso a espacios de prácticas corporales",
    x = "Espacio",
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
  )



# 
# 
# ###################
# # Modificar datos #
# ###################
# datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
# 	
# 	mutate(   # Para crear nuevas variables y editar las ya existentes
# 		
# 		# Veo valores min y max de la variable para elegir una
# 		# particion en intervalos apropiada
# 		# min(altura)
# 		# max(altura)
# 		# sqrt(nrow(datos))
# 		
# 		# Creo una variable nueva, con la partición en intervalos de altura
# 		altura_int = cut(altura,
# 										 breaks = seq(from=0, to=50, by = 5),
# 										 right = F),
# 		
# 		# Modifico las columnas de la variable de respuesta múltiple
# 		# para dejarlas como indicadoras con valores 1 (en caso de presentar
# 		# el atributo) y 0 (en caso de no presentarlo)
# 		atracnosis = ifelse( atracnosis == "atracnosis", 1, 0 ),
# 		roya = ifelse( roya == "roya", 1, 0 ),
# 		manchas = ifelse( manchas == "manchas", 1, 0 ),
# 		ampollas = ifelse( ampollas == "ampollas", 1, 0),
# 		# Notar que los NA no entran dentro de la categoría "no presentar 
# 		# el atributo", por lo que requieren un tratamiento particular:
# 		
# 		atracnosis = ifelse(is.na(atracnosis), 0, 1),
# 		roya = ifelse(is.na(roya), 0, 1),
# 		manchas = ifelse(is.na(manchas), 0, 1),
# 		ampollas = ifelse(is.na(ampollas), 0, 1),
# 		# Esto solo es correcto porque teníamos dos valores posibles en estas
# 		# columnas: presencia de atributo (nombre de la plaga) y ausencia (NA).
# 		# En los casos en los que se presenten ambas categorías además del NA
# 		# correspondería trabajarlos como tres valores distintos (presencia,
# 		# ausencia y faltante) y su tratamiento dependerá de lo que se desee hacer
# 		
# 		# Para condiciones ifelse múltiples puedo usar la función case_when
# 		inclinacion_cate = case_when(inclinacion == 0 ~ "Sin inclinación",
# 																 inclinacion < 15 ~ "Inclinación leve",
# 																 inclinacion < 30 ~ "Inclinación moderada",
# 																 TRUE ~ "Inclinación alta"),
# 		
# 		# Recodifico las etiquetas de una variable categórica
# 		especie = recode(especie, "ala" = "Álamo",
# 										 "casu" = "Casuarina",
# 										 "euca" = "Eucalipto",
# 										 "jaca" = "Jacarandá",
# 										 "palo"  = "Palo borracho"),
# 		
# 		# Especifico ordinalidad a las categorías de una variable
# 		tiempo = factor(tiempo,
# 										levels = 1:5,
# 										labels = c("Menos de 2 años", "Entre 2 y 5 años",
# 																				 "Entre 5 y 10 años", "Entre 10 y 20 años",
# 																				 "20 años o más"))
# 
# 	)
# 
# ##########################################
# # Seleccionar un subconjunto de columnas #
# ##########################################
# 
# # Opcion 1
# datos_chico1 <- datos_limpios %>%
# 	select(   # Seleccionar las columnas que quiero conservar
# 		id, altura, edad, follaje, inclinacion_cate
# 	)
# 
# # Opcion 2
# datos_chico2 <- datos_limpios %>%
# 	select(   # Eliminar las columnas que no quiero conservar
# 		-altura, -edad, -follaje, -inclinacion_cate
# 	)
# 
# # Opcion 3
# datos_orden <- datos_limpios %>%
# 	select(   # Reordeno columnas
# 		id, especie, tiempo, everything()
# 	)
# 
# 
# ###########################################
# # Seleccionar un subconjunto de registros #
# ###########################################
# 
# # Opción 1: por criterio
# datos_reducido1 <-datos_orden %>%
# 	filter((brotes > 4 & origen == "Nativo/Autóctono") | tiempo == "20 años o más")
# 
# # Opción 2: por indexación
# datos_reducido2 <-datos_orden %>%
# 	slice(1:500)

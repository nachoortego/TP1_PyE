# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
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

# Creo la columna de acceso
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







# Contar cuántas veces aparece cada tipo de espacio
espacios_pc_vector <- unlist(espacios_pc)
espacios_pc_vector <- espacios_pc_vector[espacios_pc_vector != "" & !is.na(espacios_pc_vector)]

frecuencias_pc <- sort(table(espacios_pc_vector), decreasing = TRUE)

# Graficar
barplot(frecuencias_pc,
        las = 2,
        col = "skyblue",
        main = "Acceso a espacios de prácticas corporales",
        ylab = "Cantidad de personas",
        cex.names = 0.8)

mtext("Fuente: observatorio villero", side = 1, adj = 0, line = 3, cex = 0.8)

espacios_verdes_vector <- unlist(espacios_verdes)
espacios_verdes_vector <- espacios_verdes_vector[espacios_verdes_vector != "" & !is.na(espacios_verdes_vector)]

frecuencias_verdes <- sort(table(espacios_verdes_vector), decreasing = TRUE)

barplot(frecuencias_verdes,
        las = 2,
        col = "lightgreen",
        main = "Acceso a espacios verdes",
        ylab = "Cantidad de personas",
        cex.names = 0.8)

mtext("Fuente: observatorio villero", side = 1, adj = 0, line = 3, cex = 0.8)

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

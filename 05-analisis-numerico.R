library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos)

# Funciones para obtener medidas
attach(datos)

# Cantidad total de espacios de practicas corporales a menos de 500 m
total_espacios_pc <- sum(frecuencias_pc$cantidad)

# Cantidad de canchas de futbol a menos de 500 m
futbol_count <- frecuencias_pc$cantidad[frecuencias_pc$espacio == "Cancha de fútbol"]

# Porcentaje de los espacios de practicas corporales que son canchas de fútbol
porcentaje_futbol <- round(100 * futbol_count / total_espacios_pc, 1)

# Cantidad total de espacios verdes a menos de 500 m
total_espacios_verdes <- sum(frecuencias_espacios_verdes$cantidad)

# Cantidad total de plazas de menos de 0.5 hectareas a menos de 500m
placita_count <- frecuencias_espacios_verdes$cantidad[
  frecuencias_espacios_verdes$espacio == "Placita, plazoleta, paseo (Menos de 0,5 hectáreas)"
]

# Porcentaje de los espacios verdes a menos de 500 m que son plazas de menos de 0.5 hectareas  
porcentaje_placita <- round(100 * placita_count / total_espacios_verdes, 1)

# Cantidad total de parques de mas de 5 hectareas
parques_count <- frecuencias_espacios_verdes$cantidad[
  frecuencias_espacios_verdes$espacio == "Parque Urbano (Más de 5 ha hectáreas)"
]

# Porcentaje de los espacios verdes a menos de 500 m que son parques de mas de 5 hectareas  
porcentaje_parques <- round(100 * placita_count / total_espacios_verdes, 1)

# Porcentaje de los que no tienen acceso a bicicletas publicas
porcentaje_no_bp <- datos %>%
  group_by(acceso_bp) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = round(100 * cantidad / sum(cantidad), 1)) %>%
  filter(acceso_bp == "No") %>%
  pull(porcentaje)


# Promedio del tiempo de residencia
mean(tiempo_residencia)
sd(tiempo_residencia) # Desvío estándar

# Mediana del tiempo de residencia y cuartiles
median(tiempo_residencia) 
quantile(tiempo_residencia) 


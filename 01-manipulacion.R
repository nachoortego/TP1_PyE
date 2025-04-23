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

acceso_espacios_pc <- apply(espacios_pc, 1, function(fila) {
  if ("No existen tales espacios" %in% fila) {
    return("No tiene acceso")
  } else if (any(!is.na(fila) & fila != "")) {
    return("Tiene acceso")
  } else {
    return("Sin datos")
  }
})

acceso_espacios_verdes <- apply(espacios_verdes, 1, function(fila) {
  if ("No existen tales espacios" %in% fila) {
    return("No tiene acceso")
  } else if (any(!is.na(fila) & fila != "")) {
    return("Tiene acceso")
  } else {
    return("Sin datos")
  }
})

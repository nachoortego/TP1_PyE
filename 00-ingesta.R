# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")

# Cargar librerías
library(googledrive)
library(readxl)


# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/16_zhdrZIW72I45SHIsVkGv-KYQw1oeup
# El id de esta hoja de cálculo es "16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
														overwrite = T)

# Cargo el archivo como .xlsx
rango <- "B3:DN1125"
datos <- readxl::read_excel("Datos_LP.xlsx", 
                            range = rango,
														skip = 3)

# Veo la estructura del dataset
str(datos)


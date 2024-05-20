# Instalo los paquetes necesarios (si aún no los tengo instalados)

# Si hay problemas para instalar el paquete "googledrive" proba instalando
# sudo apt-get install libcurl4-openssl-dev

install.packages("googledrive") # No funciona la instalacion
install.packages("readxl")

library(googledrive)
library(readxl)

file_name = "/home/octavio/Probabilidad_y_Estadistica/TP1_PyE2024/barrios_populares/Datos.xlsx"

# Descargamos datos del Relevamiento habitacional desde Google Drive
# El archivo se descarga en el workign directory. Despues lo eliminamos.
# googledrive::drive_download(as_id("1sD01MGvlotrAZuC_xPXwwNd5ipwqCHbo"),
#                            overwrite = T)

# Cargo el archivo como .xlsx
datos <- readxl::read_excel(file_name, 
														col_names = FALSE, 
														skip = 3)

# Borramos el archivo
# file.remove(file_name)

# Veo la estructura del dataset
str(datos)


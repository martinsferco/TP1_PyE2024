# Instalo los paquetes necesarios (si aún no los tengo instalados)

# install.packages("googledrive")
# install.packages("readxl")

# Si hay problemas para instalar el paquete "googledrive" proba instalando
# sudo apt-get install libcurl4-openssl-dev

library(googledrive)
library(readxl)

file_name = "Datos_LP.xlsx"

# Descargamos datos del Relevamiento habitacional desde Google Drive
# El archivo se descarga en el working directory.
# En el repositorio ya incluimos el archivo "Datos_LP.xlsx", por eso comentamos la descarga.
# De cualquier manera, se puede utilizar el script para descargar el archivo.
# googledrive::drive_download(as_id("1sD01MGvlotrAZuC_xPXwwNd5ipwqCHbo"),
#                            overwrite = T)

# Cargo el archivo como .xlsx
datos_df <- readxl::read_excel(file_name, 
														col_names = FALSE, 
														skip = 3)

# Borramos el archivo
# file.remove(file_name)


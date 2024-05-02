# Instalo los paquetes necesarios (si aún no los tengo instalados)
install.packages("googledrive") # No funciona la instalacion
install.packages("readxl")

library(googledrive)

# Descargamos datos del Relevamiento habitacional desde Google Drive
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy/edit#gid=1908972294"),
                            overwrite = T)

# Cargo el archivo como .xlsx
datos <- readxl::read_excel("Datos_LP.xlsx", 
														col_names = FALSE, 
														skip = 3)

# Veo la estructura del dataset
str(datos)

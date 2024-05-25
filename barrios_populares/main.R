# Para que funcione la descarga de datos y lectura de datos, tenemos que abrir este
# archivo con doble click desde la propia carpeta. 
# Esto hace que el working directory se setee en la carpeta en la que se 
# encuentra guardado todos los archivos. Si abrimos R y luego el archivo, el working directory se setea 
# en una ruta inicial en la que no podemos acceder el archivo "Datos_LP.xlsx" y tendríamos
# que setear una ruta absoluta al archivo, que cambia dependiendo la computadora
# en donde se este ejecutando.
source("descarga_datos.R")
source("limpieza_tabla.R")
source("recursos_graficos.R")
source("analisis_numerico.R")


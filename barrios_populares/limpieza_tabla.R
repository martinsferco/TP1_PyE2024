# Instalo los paquetes necesarios (si aún no los tengo instalados)
install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(magrittr)
library(dplyr)

# Fijo las columnas del dataset
attach(datos)

#########################################################################
# Guardamos las columnas de interes en distintas variables y dataframes #
#########################################################################


# Dataframe de variables de respuesta unica
datos_df <- data.frame(...1, ...2, ...6, ...5, ...19, ...89, ...90, ...14)
colnames(datos_df) <- c("Id", "Provincia", "Cantidad de integrantes", "Tiempo de residencia",
                        "Condicion del lugar que habitan", "Hay veredas", "Hay alumbrado publico",
                        "Numero maximos de personas por habitacion")


# Dataframes de variables de respuesta multiple
practicas_corporales_df <- data.frame(...2, ...96, ...97, ...98, ...99, ...100, ...101, ...102, ...103, ...104)
colnames(practicas_corporales_df) <- c("Provincia", "Polideportivo municipal", "Natatorio municipal", "Playón multiuso", "Cancha de futbol",  "Posta de ejercicio",
                                       "Skatepark", "Balnearios","No existen tales espacios", "Otro")

espacios_verdes_df <- data.frame(...2, ...106, ...107, ...108, ...109)
colnames(espacios_verdes_df) <- c("Provincia", "Placita, plazoleta, paseo (Menos de 0,5 hectáreas)",
                                  "Plaza (Entre 0,5 y 5 hectáreas)",
                                  "Parque Urbano (Más de 5 hectáreas)", 
                                  "No existen tales espacios")


#############################################
# Modificamos los datos para su utilizacion #
#############################################

particiones_tiempo = seq(from = 0, to = 120, 5)

datos_df <- datos_df %>%
  mutate(
    Tiempo de residencia intervalo = cut(Tiempo de residencia,
                                         breaks = particiones_tiempo,
                                         right = FALSE)
  )

practicas_corporales_df <- practicas_corporales_df %>%
  mutate (across(everything(), ~ifelse(is.na(.), 0, 1)))

espacios_verdes_df <- espacios_verdes_df %>% 
  mutate (across(everything(), ~ ifelse(is.na(.), 0, 1)))

# SELECT FILTER UTILES

# Separamos los datos de CABA en una tabla
datos_caba <- subset(datos_df, datos_df $Provincia == "CABA")

# Separamos los datos de las provincias del litoral en una tabla

litoral <- c("Formosa", "Chaco", "Misiones", "Corrientes", "Entre Ríos", "Santa Fe")
datos_lit <- subset(datos_df, datos_df $ Provincia %in% litoral)

# Y las de espacios verdes y practicas corporales tambien,

practicas_corporales_litoral_df = subset(practicas_corporales_df, practicas_corporales_df $ Provincia %in% litoral)
practicas_corporales_caba_df    = subset(practicas_corporales_df, practicas_corporales_df $ Provincia == "CABA")


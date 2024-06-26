# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(magrittr)
library(dplyr)

# Fijo las columnas del dataset
attach(datos_df)

#########################################################################
# Guardamos las columnas de interes en distintas variables y dataframes #
#########################################################################

# Dataframe de variables de respuesta unica
# Eliminamos la fila 1077 que tiene un tiempo de residencia incorrecto ( > 110)
datos_df <- datos_df[-1077,]

datos_df <- data.frame(...1, ...2, ...6, ...5, ...19, ...89, ...90, ...14)
colnames(datos_df) <- c("Id", "Provincia", "Cantidad de integrantes", "Tiempo de residencia",
                        "Condicion del lugar que habitan", "Hay veredas", "Hay alumbrado publico",
                        "Numero maximos de personas por habitacion")


# Dataframes de variables de respuesta multiple
provincias_df <- data.frame(...2)
colnames(provincias_df) <- c("Provincia")

practicas_corporales_df <- data.frame(...96, ...97, ...98, ...99, ...100, ...101, ...102, ...103, ...104)
colnames(practicas_corporales_df) <- c("Polideportivo municipal", "Natatorio municipal", "Playón multiuso", "Cancha de futbol",  "Posta de ejercicio",
                                       "Skatepark", "Balnearios","No existen tales espacios", "Otro")

espacios_verdes_df <- data.frame(...106, ...107, ...108, ...109)
colnames(espacios_verdes_df) <- c("Placita, plazoleta, paseo (Menos de 0,5 hectáreas)",
                                  "Plaza (Entre 0,5 y 5 hectáreas)",
                                  "Parque Urbano (Más de 5 hectáreas)", 
                                  "No existen tales espacios")


#############################################
# Modificamos los datos para su utilizacion #
#############################################

particiones_tiempo = seq(from = 0, to = 70, 5)

datos_df <- datos_df %>%
  mutate(
    `Tiempo de residencia intervalo` = cut(`Tiempo de residencia`,
                                         breaks = particiones_tiempo,
                                         right = FALSE)
  )

practicas_corporales_df <- practicas_corporales_df %>%
  mutate (across(everything(), ~ifelse(is.na(.), 0, 1)))

practicas_corporales_df <- data.frame(provincias_df, practicas_corporales_df)

espacios_verdes_df <- espacios_verdes_df %>% 
  mutate (across(everything(), ~ ifelse(is.na(.), 0, 1)))

espacios_verdes_df <- data.frame(provincias_df, espacios_verdes_df)
# SELECT FILTER UTILES

# Separamos los datos de CABA en una tabla
datos_caba <- subset(datos_df, datos_df $Provincia == "CABA")

# Separamos los datos de las provincias del litoral en una tabla

litoral <- c("Formosa", "Chaco", "Misiones", "Corrientes", "Entre Ríos", "Santa Fe")
datos_lit <- subset(datos_df, datos_df $ Provincia %in% litoral)

# Y las de espacios verdes y practicas corporales tambien,

practicas_corporales_litoral_df = subset(practicas_corporales_df, practicas_corporales_df$Provincia %in% litoral)[,-1]
practicas_corporales_caba_df    = subset(practicas_corporales_df, practicas_corporales_df$Provincia == "CABA")[,-1]

espacios_verdes_litoral_df = subset(espacios_verdes_df, espacios_verdes_df$Provincia %in% litoral)[, -1]
espacios_verdes_caba_df    = subset(espacios_verdes_df, espacios_verdes_df$Provincia == "CABA")[, -1]


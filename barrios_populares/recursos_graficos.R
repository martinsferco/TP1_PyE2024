# Aqui realizaremos las distintas graficas del TP
color_litoral = "#1E8EEC"
color_caba    = "#EC821E"


# CANTIDAD INTEGRANTES - BASTONES

# TIEMPO DE RESIDENCIA - HISTOGRAMA

hist(x = datos_caba$`Tiempo de residencia`,
     breaks = particiones_tiempo,
     right = FALSE,
     main = "Tiempo de residencia en CABA",
     xlab = "Tiempo de residencia (en años)",
     ylab = "Hogares",
     xaxt = "n",
     ylim = c(0, 100),
     col = color_caba
     
  )
axis(side = 1, particiones_tiempo)

hist(x = datos_lit$`Tiempo de residencia`,
     breaks = particiones_tiempo,
     right = FALSE,
     main = "Tiempo de residencia en el Litoral",
     xlab = "Tiempo de residencia (en años)",
     ylab = "Hogares",
     xaxt = "n",
     ylim = c(0, 100),
     col = color_litoral
     
)
axis(side = 1, particiones_tiempo)


# CONDICION DEL LUGAR QUE HABITAN
# En principio iba a ser un grafico de sectores, pero no resulta claro
# porque algunos sectores eran demasiado pequenios

cond_lugar_caba <- table(datos_caba $`Condicion del lugar que habitan`)
cond_lugar_caba <- cond_lugar_caba[order(cond_lugar_caba, decreasing = TRUE)]

barplot(height = cond_lugar_caba,
        width = 1,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en CABA",
        xlab = "Condición",
        ylab = "Hogares",
        ylim = c(0, 120),
        col = "#EC821E")

cond_lugar_lit <- table(datos_lit $`Condicion del lugar que habitan`)
cond_lugar_lit <- cond_lugar_lit[order(cond_lugar_lit, decreasing = TRUE)]

barplot(height = cond_lugar_lit,
        width = 1,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en el Litoral",
        xlab = "Condición",
        ylab = "Hogares",
        ylim = c(0, 140),
        col = "#1E8EEC")

# HAY VEREDAS - GRAFICO DE BARRAS ORDENADO POR CATEGORIA
levels_estado <- c("No", 
                    "Sí, hechas por vecinxs", 
                    "Sí, hechas por el Estado (municipio, provincia o Estado nacional)")

mi_orden <- factor(datos_caba $`Hay veredas`, levels = levels_estado)

datos_caba_ordenados <- datos_caba
datos_caba_ordenados$`Hay veredas` <- factor(datos_caba$`Hay veredas`, levels = levels_estado)

datos_litoral_ordenados <- datos_lit
datos_litoral_ordenados$`Hay veredas` <- factor(datos_lit$`Hay veredas`, levels = levels_estado)

# Luego, crea la tabla con los datos ordenados
veredas_caba <- table(datos_caba_ordenados$`Hay veredas`)
veredas_litoral <- table(datos_litoral_ordenados$`Hay veredas`)

# Ahora, crea el gráfico de barras
barplot(height = veredas_caba,
        names.arg = names(veredas_caba),  # Usa los nombres de las categorías
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Estado de veredas de barrios populares en CABA",
        xlab = "Estado de veredas",
        ylab = "Hogares",
        ylim = c(0, 200),
        col = "#EC821E")

barplot(height = veredas_litoral,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Estado de veredas de barrios populares en el Litoral",
        xlab = "Estado de veredas",
        ylab = "Hogares",
        ylim = c(0, 200),
        col = "#1E8EEC")

# HAY ALUMBRADO PUBLICO - GRAFICO DE BARRAS ORDENADO POR CATEGORIA

datos_caba_ordenados$`Hay alumbrado publico` <- factor(datos_caba$`Hay alumbrado publico`,
                                                       levels = levels_estado)

datos_litoral_ordenados$`Hay alumbrado publico` <- factor(datos_lit$`Hay alumbrado publico`,
                                                          levels = levels_estado)

alumbrado_caba <- table(datos_caba_ordenados $`Hay alumbrado publico`)
alumbrado_lit <- table(datos_litoral_ordenados $`Hay alumbrado publico`)

barplot(height = alumbrado_caba,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Estado de alumbrado de barrios populares en CABA",
        xlab = "Estado de alumbrado",
        ylab = "Hogares",
        ylim = c(0, 200),
        col = "#EC821E")

barplot(height = alumbrado_lit,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Estado de veredas de barrios populares en el Litoral",
        xlab = "Estado de alumbrado",
        ylab = "Hogares",
        ylim = c(0, 200),
        col = "#1E8EEC")

# ESPACIOS DE PRACTICAS CORPORALES - TABLA DE FRECUENCIAS

tot_poli_caba <- sum(practicas_corporales_caba_df $Polideportivo.municipal)
tot_nat_caba <- sum(practicas_corporales_caba_df $Natatorio.municipal)
tot_play_caba <- sum(practicas_corporales_caba_df $Playón.multiuso)
tot_can_caba <- sum(practicas_corporales_caba_df $Cancha.de.futbol)
tot_post_caba <- sum(practicas_corporales_caba_df $Posta.de.ejercicio)
tot_ska_caba <- sum(practicas_corporales_caba_df  $Skatepark)
tot_bal_caba <- sum(practicas_corporales_caba_df $Balnearios)
tot_noex_caba <- sum(practicas_corporales_caba_df $No.existen.tales.espacios)
tot_otr_caba <- sum(practicas_corporales_caba_df $Otro)

porc_caba <- c(tot_poli_caba / length(practicas_corporales_caba_df $Polideportivo.municipal) * 100,
          tot_nat_caba / length(practicas_corporales_caba_df $Natatorio.municipal) * 100,
          tot_play_caba / length(practicas_corporales_caba_df $Playón.multiuso) * 100,
          tot_can_caba / length(practicas_corporales_caba_df $Cancha.de.futbol) * 100,
          tot_post_caba / length(practicas_corporales_caba_df $Posta.de.ejercicio) * 100,
          tot_ska_caba / length(practicas_corporales_caba_df  $Skatepark) * 100,
          tot_bal_caba / length(practicas_corporales_caba_df $Balnearios) * 100,
          tot_noex_caba / length(practicas_corporales_caba_df $No.existen.tales.espacios) * 100,
          tot_otr_caba / length(practicas_corporales_caba_df $Otro) * 100)

dato_frec_espacios_caba <- data.frame(Espacio = colnames(practicas_corporales_caba_df), Porcentajes = porc_caba)

tot_poli_lit <- sum(practicas_corporales_litoral_df $Polideportivo.municipal)
tot_nat_lit <- sum(practicas_corporales_litoral_df $Natatorio.municipal)
tot_play_lit <- sum(practicas_corporales_litoral_df $Playón.multiuso)
tot_can_lit <- sum(practicas_corporales_litoral_df $Cancha.de.futbol)
tot_post_lit <- sum(practicas_corporales_litoral_df $Posta.de.ejercicio)
tot_ska_lit <- sum(practicas_corporales_litoral_df  $Skatepark)
tot_bal_lit <- sum(practicas_corporales_litoral_df $Balnearios)
tot_noex_lit <- sum(practicas_corporales_litoral_df $No.existen.tales.espacios)
tot_otr_lit <- sum(practicas_corporales_litoral_df $Otro)

porc_lit <- c(tot_poli_lit / length(practicas_corporales_litoral_df $Polideportivo.municipal) * 100,
              tot_nat_lit / length(practicas_corporales_litoral_df $Natatorio.municipal) * 100,
              tot_play_lit / length(practicas_corporales_litoral_df $Playón.multiuso) * 100,
              tot_can_lit / length(practicas_corporales_litoral_df $Cancha.de.futbol) * 100,
              tot_post_lit / length(practicas_corporales_litoral_df $Posta.de.ejercicio) * 100,
              tot_ska_lit / length(practicas_corporales_litoral_df  $Skatepark) * 100,
              tot_bal_lit / length(practicas_corporales_litoral_df $Balnearios) * 100,
              tot_noex_lit / length(practicas_corporales_litoral_df $No.existen.tales.espacios) * 100,
              tot_otr_lit / length(practicas_corporales_litoral_df $Otro) * 100)

dato_frec_espacios_litoral <- data.frame(Espacio = colnames(practicas_corporales_litoral_df), Porcentajes = porc_lit)

# ESPACIOS VERDES - TABLA DE FRECUENCIAS (sin acumular, es opcion multiple)

frecuencias_espacios_verdes_caba <- colSums(espacios_verdes_caba_df)
frecuencias_espacios_verdes_litoral <- colSums(espacios_verdes_litoral_df)
frecuencias_espacios_verdes_comparativa <- cbind(frecuencias_espacios_verdes_caba,
                                                 frecuencias_espacios_verdes_litoral)
colnames(frecuencias_espacios_verdes_comparativa) <- c("CABA", "Litoral")
rownames(frecuencias_espacios_verdes_comparativa) <- c("Plazoleta (menos de 0.5 hectareas)",
                                                       "Plaza (entre 0.5 y 5 hectareas)",
                                                       "Parque urbano (mas de 5 hectareas)",
                                                       "No existen")

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS

tabla_caba_veredas    <- table(datos_caba_ordenados$`Hay veredas`)
tabla_litoral_veredas <- table(datos_litoral_ordenados$`Hay veredas`)

tabla_combinada_veredas <- rbind(tabla_caba_veredas, tabla_litoral_veredas)
rownames(tabla_combinada_veredas) <- c("CABA", "Litoral")
maxval = max(max(tabla_caba_veredas), max(tabla_litoral_veredas))

# Crear el gráfico de barras agrupadas
barplot(tabla_combinada_veredas, beside = TRUE, col = c(color_caba, color_litoral),
        legend = rownames(tabla_combinada_veredas), 
        main = "Comparación de 'Hay veredas' entre CABA y Litoral",
        xlab = "Respuesta", ylab = "Frecuencia",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, 200))        


# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS

tabla_caba_alumbrado    <- table(datos_caba$`Hay alumbrado publico`)
tabla_litoral_alumbrado <- table(datos_lit$`Hay alumbrado publico`)

tabla_combinada_alumbrado <- rbind(tabla_caba_alumbrado, tabla_litoral_alumbrado)
rownames(tabla_combinada_alumbrado) <- c("CABA", "Litoral")
maxval = max(max(tabla_caba_alumbrado), max(tabla_litoral_alumbrado))

# Crear el gráfico de barras agrupadas
barplot(tabla_combinada_alumbrado, beside = TRUE, col = c("skyblue", "orange"),
        legend = rownames(tabla_combinada), 
        main = "Comparación de 'Hay alumbrado publico' entre CABA y Litoral",
        xlab = "Respuesta", ylab = "Frecuencia",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, 200))        


# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS

# Sumar las respuestas por columna para obtener el recuento total de cada 
# espacio de prácticas corporales en cada región
sum_caba_corporales <- colSums(practicas_corporales_caba_df)
sum_litoral_corporales <- colSums(practicas_corporales_litoral_df)

# Graficar
barplot(
  rbind(sum_caba_corporales, sum_litoral_corporales),
  beside = TRUE,
  legend.text = c("CABA", "Litoral"),
  # args.legend = list(title = "Región"),
  col = c(color_caba, color_litoral),
  main = "Comparativa de espacios de prácticas corporales entre CABA y el Litoral",
  xlab = "Espacios de Prácticas Corporales",
  ylab = "Frecuencia",
  cex.main = 1.2, # Tamaño del título
  cex.lab = 1.2,  # Tamaño de etiquetas de ejes
  cex.axis = 0.8, # Tamaño de los números de ejes
  cex.names = 0.6, # Tamaño de los nombres de barras
  names.arg = c("Polideportivo municipal", "Natatorio municipal",
                "Playón multiuso", "Cancha de futból", "Posta de ejercicio",
                "Skatepark", "Balnearios", "No existen", "Otro"),
  las = 1
  
)

# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS

# Sumar las respuestas por columna para obtener el recuento total de cada 
# espacio de prácticas corporales en cada región
sum_caba_verdes <- colSums(espacios_verdes_caba_df)
sum_litoral_verdes <- colSums(espacios_verdes_litoral_df)

# Graficar
barplot(
  rbind(sum_caba_verdes, sum_litoral_verdes),
  beside = TRUE,
  legend.text = c("CABA", "Litoral"),
  args.legend = list(title = "Región"),
  col = c(color_caba, color_litoral),
  main = "Comparativa de espacios verdes a menos de 500m entre CABA y el Litoral",
  xlab = "Espacios verdes",
  ylab = "Frecuencia",
  cex.main = 1.2, # Tamaño del título
  cex.lab = 1.2,  # Tamaño de etiquetas de ejes
  cex.axis = 0.8, # Tamaño de los números de ejes
  cex.names = 0.6, # Tamaño de los nombres de barras
  names.arg = c("Plazoleta (menos de 0.5 hectareas)",
                "Plaza (entre 0.5 y 5 hectareas)",
                "Parque urbano (mas de 5 hectareas)",
                "No existen"),
  las = 1,
  ylim = c(0, 200)
)


# PROVINCIA VS TIEMPO DE RESIDENCIA - BOXPLOT COMPARATIVO

tiempo_litoral = datos_lit[["Tiempo de residencia"]]
tiempo_caba    = datos_caba[["Tiempo de residencia"]]

boxplot(tiempo_caba, tiempo_litoral,
        names = c("CABA", "Litoral"),
        xlab = "Region",
        ylab = "Tiempo de residencia",
        main = "Comparacion de tiempo de residencia por region",
        col  = c(color_caba, color_litoral))




# INTEGRANES VS NRO MAXIMO DE PERSONAS - GRAFICO DE DISPERSION
# No lo hacemos
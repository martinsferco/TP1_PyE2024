# Aqui realizaremos las distintas graficas del TP

# CANTIDAD INTEGRANTES - BASTONES

# TIEMPO DE RESIDENCIA - HISTOGRAMA

hist(x = datos_caba$`Tiempo de residencia`,
     breaks = particiones_tiempo,
     right = FALSE,
     main = "Tiempo de residencia CABA",
     xlab = "Tiempo de residencia",
     ylab = "Frecuencia",
     xaxt = "n",
     ylim = c(0, 100)
     
  )
axis(side = 1, particiones_tiempo)

hist(x = datos_lit$`Tiempo de residencia`,
     breaks = particiones_tiempo,
     right = FALSE,
     main = "Tiempo de residencia Litoral",
     xlab = "Tiempo de residencia",
     ylab = "Frecuencia",
     xaxt = "n",
     ylim = c(0, 100)
     
)
axis(side = 1, particiones_tiempo)


# CONDICION DEL LUGAR QUE HABITAN - SECTORES
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
cond_lugar_lit <- cond_lugar_lit[order(names(cond_lugar_lit), decreasing = TRUE)]

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
mi_orden <- factor(datos_caba $`Hay veredas`, levels = c("No", 
                                                          "Sí, hechas por vecinxs", 
                                                          "Sí, hechas por el Estado (municipio, provincia o Estado nacional"))

veredas_caba <- table(datos_caba $`Hay veredas` [order(mi_orden)])
veredas_lit <-  table(datos_lit $`Hay veredas` [order(mi_orden)])

barplot(height = veredas_caba,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Estado de veredas de barrios populares en CABA",
        xlab = "Estado de veredas",
        ylab = "Hogares",
        ylim = c(0, 200),
        col = "#EC821E")

barplot(height = veredas_lit,
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
alumbrado_caba <- table(datos_caba $`Hay alumbrado publico` [order(mi_orden)])
alumbrado_lit <- table(datos_lit $`Hay alumbrado publico` [order(mi_orden)])

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

# ESPACIOS VERDES - TABLA DE FRECUENCIAS

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS

# Suponiendo que las columnas de los dataframes son las prácticas corporales
# Asegúrate de que tus datos estén en el formato correcto antes de continuar

# Suponiendo que las columnas de los dataframes son las prácticas corporales
# Asegúrate de que tus datos estén en el formato correcto antes de continuar

# Excluir la primera columna (Provincia) // ya no hace falta, lo hice
#directamente cuando limpie la tabla
# practicas_corporales_caba_df <- practicas_corporales_caba_df[, -1]
# practicas_corporales_litoral_df <- practicas_corporales_litoral_df[, -1]

# Sumar las respuestas por columna para obtener el recuento total de cada práctica en cada región
sum_caba <- colSums(practicas_corporales_caba_df)
sum_litoral <- colSums(practicas_corporales_litoral_df)

# Crear un dataframe con los datos agregados
datos_agrupados <- data.frame(
  Practica = names(sum_caba),
  CABA = sum_caba,
  Litoral = sum_litoral
)

# Graficar
barplot(
  #t(rbind(datos_agrupados$CABA, datos_agrupados$Litoral)),
  rbind(sum_caba, sum_litoral),
  beside = TRUE,
  # legend.text = rownames(datos_agrupados),
  # args.legend = list(title = "Región"),
  col = c("lightgreen", "yellow"),
  main = "Comparativa de prácticas corporales entre CABA y el Litoral",
  xlab = "Prácticas Corporales",
  ylab = "Cantidad",
  cex.main = 1.2, # Tamaño del título
  cex.lab = 1.2,  # Tamaño de etiquetas de ejes
  cex.axis = 0.8, # Tamaño de los números de ejes
  cex.names = 0.4, # Tamaño de los nombres de barras
  las = 1
)

# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS

tabla_caba_veredas    <- table(datos_caba$`Hay veredas`)
tabla_litoral_veredas <- table(datos_lit$`Hay veredas`)

tabla_combinada <- rbind(tabla_caba_veredas, tabla_litoral_veredas)
rownames(tabla_combinada) <- c("CABA", "Litoral")
maxval = max(max(tabla_caba), max(tabla_litoral))

# Crear el gráfico de barras agrupadas
barplot(tabla_combinada, beside = TRUE, col = c("skyblue", "orange"),
        legend = rownames(tabla_combinada), 
        main = "Comparación de 'Hay veredas' entre CABA y Litoral",
        xlab = "Respuesta", ylab = "Frecuencia",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, 200))        

# PROVINCIA VS TIEMPO DE RESIDENCIA - BOXPLOT COMPARATIVO

tiempo_litoral = datos_lit[["Tiempo de residencia"]]
tiempo_caba    = datos_caba[["Tiempo de residencia"]]

boxplot(tiempo_litoral, tiempo_caba,
        names = c("Litoral", "CABA"),
        xlab = "Region",
        ylab = "Tiempo de residencia",
        main = "Comparacion de tiempo de residencia por region",
        col  = "orange")




# INTEGRANES VS NRO MAXIMO DE PERSONAS - GRAFICO DE DISPERSION
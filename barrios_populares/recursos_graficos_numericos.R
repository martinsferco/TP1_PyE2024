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

# HAY VEREDAS - GRAFICO DE BARRAS ORDENADO POR CATEGORIA
# HAY ALUMBRADO PUBLICO - GRAFICO DE BARRAS ORDENADO POR CATEGORIA

# ESPACIOS DE PRACTICAS CORPORALES - TABLA DE FRECUENCIAS

# ESPACIOS VERDES - TABLA DE FRECUENCIAS

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS

# Suponiendo que las columnas de los dataframes son las prácticas corporales
# Asegúrate de que tus datos estén en el formato correcto antes de continuar

# Suponiendo que las columnas de los dataframes son las prácticas corporales
# Asegúrate de que tus datos estén en el formato correcto antes de continuar

# Excluir la primera columna (Provincia)
practicas_corporales_caba_df <- practicas_corporales_caba_df[, -1]
practicas_corporales_litoral_df <- practicas_corporales_litoral_df[, -1]

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
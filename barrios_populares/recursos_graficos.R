#color_litoral = "#1E8EEC"
color_litoral <- "#B0926F"
color_caba  <- "#FEBF00"


# CANTIDAD INTEGRANTES 
cantidad_integrantes_caba <- table(datos_caba$`Cantidad de integrantes`)

plot( 
     cantidad_integrantes_caba,
     col= color_caba,
     main = "Cantidad integrantes por hogar en CABA",
     type = "h",
     xaxt = "n",
     ylim = c(0,70),
     xlab = "Cantidad de integrantes",
     ylab = "Frecuencia"
)

axis(side = 1, at = seq(0,10,1))

abline( h = seq(0, 70, 10),
        lty = "dotted",)


cantidad_integrantes_lit <- table(datos_lit$`Cantidad de integrantes`)

plot( 
  cantidad_integrantes_lit,
  col = color_litoral,
  main = "Cantidad integrantes por hogar en el Litoral",
  type = "h",
  xaxt = "n",
  ylim = c(0,70),
  xlab = "Cantidad de integrantes",
  ylab = "Frecuencia"
)

axis(side = 1, at = seq(0,10,1))

abline( h = seq(0, 80, 10),
        lty = "dotted")


# TIEMPO DE RESIDENCIA - HISTOGRAMA

hist(x = datos_caba$`Tiempo de residencia`,
     breaks = particiones_tiempo,
     right = FALSE,
     main = "Tiempo de residencia en CABA",
     xlab = "Tiempo de residencia (en años)",
     ylab = "Hogares",
     xaxt = "n",
     ylim = c(0, 80),
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
     ylim = c(0, 80),
     col = color_litoral
     
)
axis(side = 1, particiones_tiempo)



# CONDICION DEL LUGAR QUE HABITAN
# En principio iba a ser un grafico de sectores, pero no resulta claro
# porque algunos sectores eran demasiado pequenios

cond_lugar_caba <- table(datos_caba $`Condicion del lugar que habitan`)
cond_lugar_caba <- cond_lugar_caba[order(cond_lugar_caba, decreasing = TRUE)]

barplot(height = cond_lugar_caba,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en CABA",
        xlab = "Condición",
        ylab = "Hogares",
        ylim = c(0, 120),
        col = color_caba)

cond_lugar_lit <- table(datos_lit $`Condicion del lugar que habitan`)
cond_lugar_lit <- cond_lugar_lit[order(cond_lugar_lit, decreasing = TRUE)]

barplot(height = cond_lugar_lit,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en el Litoral",
        xlab = "Condición",
        ylab = "Hogares",
        ylim = c(0, 140),
        col = color_litoral)


# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
levels_estado <- c("No", 
                   "Sí, hechas por vecinxs", 
                   "Sí, hechas por el Estado (municipio, provincia o Estado nacional)")

mi_orden <- factor(datos_caba $`Hay veredas`, levels = levels_estado)


datos_caba_ordenados <- datos_caba
datos_caba_ordenados$`Hay veredas` <- factor(datos_caba$`Hay veredas`, levels = levels_estado)

datos_litoral_ordenados <- datos_lit
datos_litoral_ordenados$`Hay veredas` <- factor(datos_lit$`Hay veredas`, levels = levels_estado)


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
barplot(tabla_combinada_alumbrado, beside = TRUE, col = c(color_caba, color_litoral),
        legend = rownames(tabla_combinada_alumbrado), 
        main = "Comparación de 'Hay alumbrado publico' entre CABA y Litoral",
        xlab = "Respuesta", ylab = "Frecuencia",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, 200))        


# PROVINCIA VS PRACTICAS CORPORALES - 
# Terminamos haciendo una tabla, ya que teniamos muchas categorias

# Sumar las respuestas por columna para obtener el recuento total de cada 
# espacio de prácticas corporales en cada región

prop_espacios_prac_corp_caba <- colSums(practicas_corporales_caba_df) / nrow(practicas_corporales_caba_df) 
prop_espacios_prac_corp_caba <- round(prop_espacios_prac_corp_caba, digits = 4)
prop_espacios_prac_corp_lit <- colSums(practicas_corporales_litoral_df) / nrow(practicas_corporales_litoral_df) 
prop_espacios_prac_corp_lit <- round(prop_espacios_prac_corp_lit, digits = 4)

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


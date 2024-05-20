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

barplot(x = table(datos_caba $`Condicion del lugar que habitan`),
        width = 1,
        horiz = FALSE)


# HAY VEREDAS - GRAFICO DE BARRAS ORDENADO POR CATEGORIA
# HAY ALUMBRADO PUBLICO - GRAFICO DE BARRAS ORDENADO POR CATEGORIA

# ESPACIOS DE PRACTICAS CORPORALES - TABLA DE FRECUENCIAS

# ESPACIOS VERDES - TABLA DE FRECUENCIAS

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS

# PROVINCIA VS TIEMPO DE RESIDENCIA - BOXPLOT COMPARATIVO

# INTEGRANES VS NRO MAXIMO DE PERSONAS - GRAFICO DE DISPERSION
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
        ylim = c(0, 120),
        col = "#EC821E")

cond_lugar_lit <- table(datos_lit $`Condicion del lugar que habitan`)
cond_lugar_lit <- cond_lugar_lit[order(names(cond_lugar_lit), decreasing = TRUE)]

barplot(height = cond_lugar_lit,
        width = 1,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
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
        ylim = c(0, 200),
        col = "#EC821E")

barplot(height = veredas_lit,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
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
        ylim = c(0, 200),
        col = "#EC821E")

barplot(height = alumbrado_lit,
        width = 0.5,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        ylim = c(0, 200),
        col = "#1E8EEC")

# ESPACIOS DE PRACTICAS CORPORALES - TABLA DE FRECUENCIAS


# ESPACIOS VERDES - TABLA DE FRECUENCIAS

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS
# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS

# PROVINCIA VS TIEMPO DE RESIDENCIA - BOXPLOT COMPARATIVO

# INTEGRANES VS NRO MAXIMO DE PERSONAS - GRAFICO DE DISPERSION
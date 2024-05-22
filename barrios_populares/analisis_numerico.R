install.packages("modeest")
library(modeest)


# CANTIDAD INTEGRANTES - BASTONES
# Media y desvio estandar (mediana y rango inter)

# TIEMPO DE RESIDENCIA
# Al ver el histograma, notamos en ambos graficos, una distribucion asimetrica
# a derecha. Por eso, utilizaremos medidas robustas como la mediana y el rango
# intercuartilico.

mediana_caba_tiempo_residencia <- median(datos_caba$`Tiempo de residencia`)
cuartiles_caba_tiempo_residencia <- quantile(datos_caba$`Tiempo de residencia`, c(0.25,0.5,0.75))
ranInter_caba_tiempo_residencia <- cuartiles_caba_tiempo_residencia[3] - cuartiles_caba_tiempo_residencia[1]
ranInter_caba_tiempo_residencia <- as.numeric(ranInter_caba_tiempo_residencia)

mediana_lit_tiempo_residencia <- median(datos_lit$`Tiempo de residencia`)
cuartiles_lit_tiempo_residencia <- quantile(datos_lit$`Tiempo de residencia`, c(0.25,0.5,0.75))
ranInter_lit_tiempo_residencia <- cuartiles_lit_tiempo_residencia[3] - cuartiles_lit_tiempo_residencia[1]
ranInter_lit_tiempo_residencia <- as.numeric(ranInter_lit_tiempo_residencia)

# CONDICION DEL LUGAR QUE HABITAN - BARRAS
moda_caba_condicion_lugar_habitan <- mfv(datos_caba$`Condicion del lugar que habitan`)
moda_lit_condicion_lugar_habitan <- mfv(datos_lit$`Condicion del lugar que habitan`)

#HAY VEREDAS - GRAFICO DE BARRAS ORDENADO POR CATEGORIA
# Proporcion
proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)

# HAY ALUMBRADO PUBLICO - GRAFICO DE BARRAS ORDENADO POR CATEGORIA
# Proporcion
proporcion_caba_alumbrado <- round(table(datos_caba$`Hay alumbrado publico`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_alumbrado <- round(table(datos_lit$`Hay alumbrado publico`) / nrow(datos_lit) * 100, digits = 2)

# ESPACIOS DE PRACTICAS CORPORALES - TABLA DE FRECUENCIAS
# Proporcion
proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)

# ESPACIOS VERDES - TABLA DE FRECUENCIAS
# Proporcion
proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)

# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
# No hacemos

# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS
# No hacemos

# PROVINCIA VS PRACTICAS CORPORALES - GRAFICO DE BARRAS AGRUPADAS
# No hacemos

# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS
# No hacemos

# PROVINCIA VS TIEMPO DE RESIDENCIA - BOXPLOT COMPARATIVO
# No hacemos


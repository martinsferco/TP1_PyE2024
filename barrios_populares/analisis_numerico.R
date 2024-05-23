# Instalo las librerias necesarias (si es que no las tengo instaladas)
# install.packages("modeest")

# Importo las librerias
library(modeest)

# CANTIDAD INTEGRANTES - NO SE CUAL MEDIDA USAR
mediana_caba_cant_integrantes <- median(datos_caba$`Cantidad de integrantes`)
cuartiles_caba_cant_integrantes <- quantile(datos_caba$`Cantidad de integrantes`, c(0.25,0.5,0.75))
ranInter_caba_cant_integrantes <- cuartiles_caba_cant_integrantes[3] - cuartiles_caba_cant_integrantes[1]
ranInter_caba_cant_integrantes <- as.numeric(ranInter_caba_cant_integrantes)

mediana_lit_cant_integrantes <- median(datos_lit$`Cantidad de integrantes`)
cuartiles_lit_cant_integrantes <- quantile(datos_lit$`Cantidad de integrantes`, c(0.25,0.5,0.75))
ranInter_lit_cant_integrantes <- cuartiles_lit_cant_integrantes[3] - cuartiles_lit_cant_integrantes[1]
ranInter_lit_cant_integrantes <- as.numeric(ranInter_lit_cant_integrantes)

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

# PROPORCIONES: VEREDAS, ALUMBRADO, ESPACIOS VERDES, ESPACIOS PRACTICAS
proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)

proporcion_caba_alumbrado <- round(table(datos_caba$`Hay alumbrado publico`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_alumbrado <- round(table(datos_lit$`Hay alumbrado publico`) / nrow(datos_lit) * 100, digits = 2)

proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)

proporcion_caba_veredas <- round(table(datos_caba$`Hay veredas`) / nrow(datos_caba) * 100, digits = 2)
proporcion_lit_veredas <- round(table(datos_lit$`Hay veredas`) / nrow(datos_lit) * 100, digits = 2)
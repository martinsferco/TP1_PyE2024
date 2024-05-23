color_litoral <- "#B0926F"
color_caba  <- "#FEBF00"

cant_entradas_caba <- nrow(datos_caba)
cant_entradas_litoral <- nrow(datos_lit)                        

legend_caba_litoral <- function() {
  legend("topright", legend = c("CABA", "Litoral"), fill = c(color_caba, color_litoral), ncol = 2, cex = 0.75, xpd = TRUE)
}

# Ahora podemos colocar siempre la misma leyenda con
# legend_caba_litoral()

# CANTIDAD INTEGRANTES 
cantidad_integrantes_caba <- table(datos_caba$`Cantidad de integrantes`)



porcentaje_cant_integrantes_caba <- cantidad_integrantes_caba / nrow(datos_caba) * 100

png("graficos/cant_integrantes_caba.png", width = 800, height = 600, res = 100)
plot( 
     porcentaje_cant_integrantes_caba,
     col= color_caba,
     main = "Cantidad integrantes por hogar en CABA",
     type = "h",
     xaxt = "n",
     ylim = c(0,25),
     xlab = "Cantidad de integrantes",
     ylab = "Porcentaje (%)"
)

axis(side = 1, at = seq(0,10,1))

abline( h = seq(0, 25, 5),
        lty = "dotted",
        col = color_caba)
dev.off()


cantidad_integrantes_lit <- table(datos_lit$`Cantidad de integrantes`)

porcentaje_cantidad_integranes_lit <- cantidad_integrantes_lit / nrow(datos_lit) * 100

png("graficos/cant_integrantes_lit.png", width = 800, height = 600, res = 100)
plot( 
  porcentaje_cantidad_integranes_lit,
  col = color_litoral,
  main = "Cantidad integrantes por hogar en el Litoral",
  type = "h",
  xaxt = "n",
  ylim = c(0,25),
  xlab = "Cantidad de integrantes",
  ylab = "Porcentaje (%)"
)

axis(side = 1, at = seq(0,10,1))

abline( h = seq(0, 25, 5),
        lty = "dotted",
        col = color_litoral)
dev.off()

# TIEMPO DE RESIDENCIA - HISTOGRAMA
library(lattice)

png("graficos/tiempo_residencia_caba.png", width = 800, height = 600, res = 100)
histogram(datos_caba$`Tiempo de residencia`,
          col = color_caba,
          main = "Tiempo de residencia en CABA",
          xlab = "Tiempo de residencia (en años)",
          ylab = "Hogares (en %)",
          ylim = c(0, 45),
          xlim = c(-5, 65),
          breaks = particiones_tiempo,
          outer = FALSE
)
dev.off()

png("graficos/tiempo_residencia_litoral.png", width = 800, height = 600, res = 100)
histogram(datos_lit$`Tiempo de residencia`,
          col = color_litoral,
          main = "Tiempo de residencia en el Litoral",
          xlab = "Tiempo de residencia (en años)",
          ylab = "Hogares (en %)",
          ylim = c(0, 45),
          xlim = c(-5, 65),
          breaks = particiones_tiempo,
          outer = FALSE
)
dev.off()

# CONDICION DEL LUGAR QUE HABITAN
cond_lugar_caba <- (table(datos_caba $`Condicion del lugar que habitan`) / cant_entradas_caba) * 100
cond_lugar_caba <- cond_lugar_caba[order(cond_lugar_caba, decreasing = TRUE)]

png("graficos/condicion_caba.png", width = 1600, height = 600, res = 100)
barplot(height = cond_lugar_caba,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en CABA",
        xlab = "Condición",
        ylab = "Hogares (en %)",
        ylim = c(0, 60),
        col = color_caba)

abline( h = seq(0, 60, 10),
        lty = "dotted",
        col = color_caba)
dev.off()

cond_lugar_lit <- (table(datos_lit $`Condicion del lugar que habitan`) / cant_entradas_litoral) * 100
cond_lugar_lit <- cond_lugar_lit[order(cond_lugar_lit, decreasing = TRUE)]

png("graficos/condicion_litoral.png", width = 1600, height = 600, res = 100)
barplot(height = cond_lugar_lit,
        horiz = FALSE,
        axes = TRUE,
        axisnames = TRUE,
        main = "Condición del hogar de barrios populares en el Litoral",
        xlab = "Condición",
        ylab = "Hogares (en %)",
        ylim = c(0, 60),
        col = color_litoral)

abline( h = seq(0, 60, 10),
        lty = "dotted",
        col = color_litoral)
dev.off()






# PROVINCIA VS VEREDAS - GRAFICO DE BARRAS AGRUPADAS
levels_estado <- c("No", 
                   "Sí, hechas por vecinxs", 
                   "Sí, hechas por el Estado (municipio, provincia o Estado nacional)")

color_no <- "#EDBB99"
color_si_vecinos <- "#DC7633"
color_si_estado <- "#BA4A00"

datos_caba_ordenados <- datos_caba
datos_caba_ordenados$`Hay veredas` <- factor(datos_caba$`Hay veredas`, levels = levels_estado)

datos_litoral_ordenados <- datos_lit
datos_litoral_ordenados$`Hay veredas` <- factor(datos_lit$`Hay veredas`, levels = levels_estado)


tabla_porcentajes_caba_veredas    <- (table(datos_caba_ordenados$`Hay veredas`) / cant_entradas_caba) * 100
tabla_porcentajes_litoral_veredas <- (table(datos_litoral_ordenados$`Hay veredas`) / cant_entradas_litoral) * 100


# Version alternando las barras
tabla_porcentajes_combinada_veredas <- cbind(tabla_porcentajes_caba_veredas, tabla_porcentajes_litoral_veredas)
colnames(tabla_porcentajes_combinada_veredas) <- c("CABA", "Litoral")
maxval = max(max(tabla_porcentajes_caba_veredas), max(tabla_porcentajes_litoral_veredas))


png("graficos/comparacion_veredas.png", width = 800, height = 600, res = 100)
barplot(tabla_porcentajes_combinada_veredas, beside = TRUE, col = c(color_no, color_si_vecinos, color_si_estado),
        # legend = rownames(tabla_combinada_alumbrado), 
        main = "Comparación de 'Hay veredas'\n entre CABA y el Litoral",
        xlab = "Región", ylab = "Porcentaje (en %)",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, maxval + 30),
        names.arg = c("CABA", "Litoral")
)        

legend("topright", legend = levels_estado, fill = c(color_no, color_si_vecinos, color_si_estado), ncol = 1, cex = 0.7, xpd = TRUE)


abline( h = seq(0, 60, 10),
        lty = "dotted",
        col = "grey")
dev.off()





# PROVINCIA VS ALUMBRADO- GRAFICO DE BARRAS AGRUPADAS


datos_caba_ordenados$`Hay alumbrado publico` <- factor(datos_caba$`Hay alumbrado publico`, levels = levels_estado)
datos_litoral_ordenados$`Hay alumbrado publico` <- factor(datos_lit$`Hay alumbrado publico`, levels = levels_estado)

tabla_caba_alumbrado    <- (table(datos_caba_ordenados$`Hay alumbrado publico`) / cant_entradas_caba) * 100
tabla_litoral_alumbrado <- (table(datos_litoral_ordenados$`Hay alumbrado publico`) / cant_entradas_litoral) * 100

tabla_combinada_alumbrado <- rbind(tabla_caba_alumbrado, tabla_litoral_alumbrado)
rownames(tabla_combinada_alumbrado) <- c("CABA", "Litoral")
maxval = max(max(tabla_caba_alumbrado), max(tabla_litoral_alumbrado))

# Version alternando las barras
tabla_combinada_alumbrado <- cbind(tabla_caba_alumbrado, tabla_litoral_alumbrado)
colnames(tabla_combinada_alumbrado) <- c("CABA", "Litoral")
maxval = max(max(tabla_caba_alumbrado), max(tabla_litoral_alumbrado))

png("graficos/comparacion_alumbrado.png", width = 800, height = 600, res = 100)
barplot(tabla_combinada_alumbrado, beside = TRUE, col = c(color_no, color_si_vecinos, color_si_estado), 
        # legend = rownames(tabla_combinada_alumbrado), 
        main = "Comparación de 'Hay alumbrado publico'\n entre CABA y el Litoral",
        xlab = "Región", ylab = "Porcentaje (en %)",
        args.legend = list(x = "topright", bty = "n"),
        ylim = c(0, maxval + 30),
        names.arg = c("CABA", "Litoral")
)        

legend("topright", legend = levels_estado, fill = c(color_no, color_si_vecinos, color_si_estado), ncol = 1, cex = 0.7, xpd = TRUE)


abline( h = seq(0, 60, 10),
        lty = "dotted",
        col = "grey")
dev.off()

# PROVINCIA VS PRACTICAS CORPORALES - 
# Terminamos haciendo una tabla, ya que teniamos muchas categorias

# Sumar las respuestas por columna para obtener el recuento total de cada 
# espacio de prácticas corporales en cada región

frecuencia_espacios_prac_corp_caba <- sort(colSums(practicas_corporales_caba_df), decreasing = TRUE)

prop_espacios_prac_corp_caba <- sort(frecuencia_espacios_prac_corp_caba / nrow(practicas_corporales_caba_df) * 100,
                                     decreasing = TRUE)
prop_espacios_prac_corp_caba <- round(x = prop_espacios_prac_corp_caba, digits = 2)

tabla_espacios_prac_corp_caba = data.frame(frecuencia_espacios_prac_corp_caba, prop_espacios_prac_corp_caba)


frecuencia_espacios_prac_corp_lit  <- sort(colSums(practicas_corporales_litoral_df), decreasing = TRUE)

prop_espacios_prac_corp_lit <- sort(frecuencia_espacios_prac_corp_lit / nrow(practicas_corporales_litoral_df) * 100,
                                    decreasing = TRUE)
prop_espacios_prac_corp_lit <- round(x = prop_espacios_prac_corp_lit, digits = 2)

tabla_espacios_prac_corp_lit = data.frame(frecuencia_espacios_prac_corp_lit, prop_espacios_prac_corp_lit)

# PROVINCIA VS ESPACIOS VERDES - GRAFICO DE BARRAS AGRUPADAS

# Sumar las respuestas por columna para obtener el recuento total de cada 
# espacio de prácticas corporales en cada región
sum_caba_verdes <- (colSums(espacios_verdes_caba_df) / cant_entradas_caba) * 100
sum_litoral_verdes <- (colSums(espacios_verdes_litoral_df) / cant_entradas_litoral) * 100

porcentaje_caba_verdes <- c(sum_caba_verdes[4], sum_caba_verdes[1:3])
porcentaje_litoral_verdes <- c(sum_litoral_verdes[4], sum_litoral_verdes[1:3])

color_no_existen <- "#EDBB99"
color_plazoleta <- "#DC7633"
color_plaza <- "#BA4A00"
color_parque_urbano <- "#784212"

levels_parques <- c("No existen",
                    "Plazoleta (menos de 0.5 hectareas)",
                    "Plaza (entre 0.5 y 5 hectareas)",
                    "Parque urbano (mas de 5 hectareas)")

# Graficar con los datos reordenados
png("graficos/comparacion_verdes.png", width = 800, height = 600, res = 100)
barplot(
  cbind(porcentaje_caba_verdes, porcentaje_litoral_verdes),
  beside = TRUE,
  col = c(color_no_existen, color_plazoleta, color_plaza, color_parque_urbano),
  main = "Comparativa de espacios verdes a menos de 500m del hogar",
  xlab = "Region",
  ylab = "Porcentaje (%)",
  cex.main = 1.2,
  cex.lab = 1.2,
  cex.axis = 0.8,
  cex.names = 1,
  names.arg = c("CABA", "Litoral"),
  las = 1,
  ylim = c(0, 80)
)

abline( h = seq(0, 80, 10),
        lty = "dotted",
        col = "grey")

legend("topright", legend = levels_parques, fill = c(color_no_existen, color_plazoleta, color_plaza, color_parque_urbano), ncol = 1, cex = 0.7, xpd = TRUE)
dev.off()

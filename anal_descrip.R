# 1. Carga de paquetes
if(!requireNamespace("ggplot2", quietly = TRUE)){
  install.packages("ggplot2")
}
library(ggplot2)

# 2. Lectura y depuración de la base de datos
datos <- read.csv("https://raw.githubusercontent.com/miglopezm14-dev/Articulo_regresion/refs/heads/main/datos.csv")

# Creacion de backup de la base de datos original
datos_backup <- datos

# Estructura de la base de datos
str(datos)
nombres <- names(datos)

# Transformación de variables chr a factor (donde aplica)
datos$city <- as.factor(datos$city)

# Transformación de variables chr a numericas (donde aplica)
nombres_no_city <- nombres[-1]
for (nombre in nombres_no_city){
  if (is.character(datos[[nombre]])){
    datos[[nombre]] <- as.numeric(gsub(pattern = ",",replacement = ".",datos[[nombre]]))
  }
}
str(datos)

# 3. Definiciones

# Kg/hab: Kilogramos por habitante
# res: Generación de residuos residuales (no separados/basura comun) en Kg/hab
# bio: Cantidad de biorresiduos (materia organica) separados en Kg/hab
# dry: Total de reciclables secos (papel, plastico, vidrio, metal) recogidos por separado en Kg/hab
# pap: Cantidad de papel y carton recogidos separadamente en Kg/hab
# popden: Densidad de poblacion de la ciudad en miles de habitantes por km^2
# gdppc: PIB per caputa de la ciudad en miles de Euros
# mat_depr: Tasa de deprivación material y social de la población (%)
# info_env: Porcentaje de ciudadanos que se consideran bien informados sobre temas ambientales
# payt: El PAYT (Pay As You Throw) es un sistema tarifario de "pago por generacion" de basura (1 si esta implementado, 0 en caso contrario)

# 4. Resumen de estadisticos en variables numericas (media, mediana, sd, min, max)
vars_num <- c("res", "bio", "dry", "pap", "popden", "gdppc", "mat_depr", "info_env")
resumen_num <- data.frame(Media = round(sapply(X = datos[vars_num], FUN = mean, na.rm = TRUE),2),
                          Mediana = round(sapply(X = datos[vars_num], FUN = median, na.rm = TRUE),2),
                          Desviacion_estandar = round(sapply(X = datos[vars_num], FUN = sd, na.rm = TRUE),2),
                          Minimo = round(sapply(X = datos[vars_num], FUN = min, na.rm = TRUE),2),
                          Maximo = round(sapply(X = datos[vars_num], FUN = max, na.rm = TRUE),2))

print(resumen_num)

# 5. Graficos

# Histograma de la generación de residuos residuales (kg/capita)
ggplot(datos, aes(x = res)) +
  geom_histogram(binwidth = 30, fill = "#2b5c8f", color = "white", alpha = 0.8) +
  labs(title = "Distribución de Residuos Residuales por Cápita",
       x = "Residuos Residuales (kg/cápita/año)",
       y = "Número de Ciudades")

# Comparación por residuos residuales según implementación de PAYT (Boxplot)
datos$payt_factor <- factor(datos$payt, labels = c("Sin PAYT", "Con PAYT"))

ggplot(datos, aes(x = payt_factor, y = res, fill = payt_factor)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("#e74c3c", "#2ecc71")) +
  labs(title = "Generación de Residuos Residuales según Sistema PAYT",
       x = "Sistema Pay-As-You-Throw (PAYT)",
       y = "Residuos Residuales (kg/cápita/año)")



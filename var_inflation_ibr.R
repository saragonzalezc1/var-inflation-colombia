
# -----------------------------------------------------------------------------
# 0. INSTALACIÓN Y CARGA DE PAQUETES
# -----------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(vars)
library(ggplot2)
library(gridExtra)
library(MTS)
library(tseries)
library(tidyr)
library(scales)

select <- dplyr::select
filter <- dplyr::filter

# -----------------------------------------------------------------------------
# 1. CARGA Y PROCESAMIENTO DE DATOS
# -----------------------------------------------------------------------------
ruta_archivo <- "macroeconomic_data.xlsx"  # Ajusta el path según tu directorio de trabajo

# ---- 1.1 Lectura y limpieza de la hoja IPC ----
ipc_raw <- read_excel(ruta_archivo, sheet = "IPC")

# Renombrar columnas por posición
names(ipc_raw)[1] <- "fecha_raw"
names(ipc_raw)[2] <- "ipc"

ipc_raw$fecha <- as.Date(floor_date(as.Date(ipc_raw$fecha_raw), unit = "month"))
ipc_raw$ipc   <- as.numeric(ipc_raw$ipc)

# Construir dataframe limpio usando indexación directa
ipc <- ipc_raw[, c("fecha", "ipc")]
ipc <- ipc[order(ipc$fecha), ]
ipc <- ipc[ipc$fecha >= as.Date("2008-01-01") &
           ipc$fecha <= as.Date("2025-09-01"), ]

cat("IPC:", nrow(ipc), "observaciones mensuales\n")
cat("Período:", format(min(ipc$fecha), "%B %Y"), "a",
    format(max(ipc$fecha), "%B %Y"), "\n\n")

# ---- 1.2 Lectura y limpieza de la hoja IBR ----
ibr_raw <- read_excel(ruta_archivo, sheet = "IBR")

# Renombrar columnas por posición
names(ibr_raw)[1] <- "fecha"
names(ibr_raw)[2] <- "tasa_efectiva"

# Convertir tipos
ibr_raw$fecha         <- as.Date(ibr_raw$fecha)
ibr_raw$tasa_efectiva <- as.numeric(ibr_raw$tasa_efectiva)

# Filtrar período de análisis
ibr_raw <- ibr_raw[ibr_raw$fecha >= as.Date("2008-01-01") &
                   ibr_raw$fecha <= as.Date("2025-09-30"), ]

# ---- 1.3 Calcular promedio mensual del IBR (diario → mensual) ----
# Crear columna de año-mes (primer día del mes) para agrupar
ibr_raw$anio_mes <- as.Date(floor_date(ibr_raw$fecha, unit = "month"))

ibr_prom  <- aggregate(tasa_efectiva ~ anio_mes, data = ibr_raw, FUN = mean)
ibr_ndias <- aggregate(tasa_efectiva ~ anio_mes, data = ibr_raw, FUN = length)

# Construir dataframe mensual
ibr_mensual <- data.frame(
  fecha    = as.Date(ibr_prom$anio_mes),
  ibr_prom = ibr_prom$tasa_efectiva,
  n_dias   = ibr_ndias$tasa_efectiva
)

# Filtrar período y ordenar cronológicamente
ibr_mensual <- ibr_mensual[ibr_mensual$fecha >= as.Date("2008-01-01") &
                           ibr_mensual$fecha <= as.Date("2025-09-01"), ]
ibr_mensual <- ibr_mensual[order(ibr_mensual$fecha), ]

# ---- 1.4 Calcular la primera diferencia mensual del IBR ----
# ΔIBR_t = IBR_t - IBR_{t-1} 
ibr_mensual$d_ibr <- c(NA, diff(ibr_mensual$ibr_prom))

cat("IBR mensual:", nrow(ibr_mensual), "observaciones\n")
cat("Período:", format(min(ibr_mensual$fecha), "%B %Y"), "a",
    format(max(ibr_mensual$fecha), "%B %Y"), "\n\n")

# ---- 1.5 Unir IPC e IBR en un solo dataframe ----
datos <- merge(ipc, ibr_mensual, by = "fecha", all = FALSE)

# Eliminar primera fila con NA en d_ibr 
datos <- datos[!is.na(datos$d_ibr), ]
datos <- datos[order(datos$fecha), ]

cat("Dataset final:", nrow(datos), "observaciones\n")
cat("Período:", format(min(datos$fecha), "%B %Y"), "a",
    format(max(datos$fecha), "%B %Y"), "\n\n")
print(head(datos, 3))

# -----------------------------------------------------------------------------
# 2. GRÁFICAS DE LAS SERIES (PUNTO a)
# -----------------------------------------------------------------------------

# ---- 2.1 Gráfica del IPC ----
g_ipc <- ggplot(datos, aes(x = fecha, y = ipc)) +
  geom_line(color = "#1a6faf", linewidth = 0.8) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = as.Date("2009-01-01"), y = 3.3,
           label = "Meta: 3%", size = 3, color = "gray40") +
  labs(
    title    = "Variación Anual del IPC Agregado (Base 2018)",
    subtitle = "Colombia | Febrero 2008 - Septiembre 2025",
    x        = NULL,
    y        = "Variación anual (%)",
    caption  = "Fuente: Banco de la República"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ---- 2.2 Gráfica del IBR promedio mensual (nivel) ----
g_ibr <- ggplot(datos, aes(x = fecha, y = ibr_prom)) +
  geom_line(color = "#c0392b", linewidth = 0.8) +
  labs(
    title    = "IBR Overnight - Promedio Mensual (Tasa Efectiva)",
    subtitle = "Colombia | Febrero 2008 - Septiembre 2025",
    x        = NULL,
    y        = "Tasa efectiva (%)",
    caption  = "Fuente: Banco de la República"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ---- 2.3 Gráfica de la primera diferencia del IBR ----
g_dibr <- ggplot(datos, aes(x = fecha, y = d_ibr)) +
  geom_line(color = "#27ae60", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title    = "Primera Diferencia del IBR Overnight (ΔIBR)",
    subtitle = "Colombia | Febrero 2008 - Septiembre 2025",
    x        = NULL,
    y        = "ΔIBR (puntos porcentuales)",
    caption  = "Fuente: Banco de la República"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ---- 2.4 Gráfica conjunta IPC vs IBR nivel ----
datos_long <- data.frame(
  fecha = c(datos$fecha, datos$fecha),
  valor = c(datos$ipc, datos$ibr_prom),
  serie = c(rep("IPC (var. anual %)", nrow(datos)),
            rep("IBR overnight (%)",  nrow(datos)))
)

g_conjunta <- ggplot(datos_long, aes(x = fecha, y = valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("IPC (var. anual %)" = "#1a6faf",
                                "IBR overnight (%)"  = "#c0392b")) +
  labs(
    title    = "IPC e IBR Overnight: Evolución Conjunta",
    subtitle = "Colombia | 2008 - 2025",
    x        = NULL,
    y        = "(%)",
    color    = NULL,
    caption  = "Fuente: Banco de la República"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Mostrar gráficas
print(g_ipc)
print(g_ibr)
print(g_dibr)
print(g_conjunta)

# -----------------------------------------------------------------------------
# 3. PREPARACIÓN DE SERIES PARA EL VAR
# -----------------------------------------------------------------------------

# Orden: [IPC, ΔIBR] — inflación primero, política monetaria segundo
fecha_inicio <- c(year(min(datos$fecha)), month(min(datos$fecha)))

Y <- cbind(
  IPC  = ts(datos$ipc,   start = fecha_inicio, frequency = 12),
  dIBR = ts(datos$d_ibr, start = fecha_inicio, frequency = 12)
)

cat("Dimensiones de Y:", dim(Y), "\n\n")

# -----------------------------------------------------------------------------
# 4. SELECCIÓN DEL NÚMERO DE REZAGOS (PUNTO b)
# -----------------------------------------------------------------------------

# VARselect evalúa AIC, HQ, SC(BIC) y FPE para rezagos 1 a 12
seleccion_rezagos <- VARselect(Y, lag.max = 12, type = "const")

cat("=== CRITERIOS DE SELECCIÓN DE REZAGOS ===\n")
print(seleccion_rezagos$criteria)
cat("\nRezago óptimo según cada criterio:\n")
print(seleccion_rezagos$selection)
cat("\n")

# Graficar criterios AIC, HQ y SC/BIC
criterios_mat <- t(seleccion_rezagos$criteria)

criterios_long <- data.frame(
  rezago   = rep(1:12, 3),
  valor    = c(criterios_mat[, 1], criterios_mat[, 2], criterios_mat[, 3]),
  criterio = rep(c("AIC", "HQ", "SC/BIC"), each = 12)
)

g_criterios <- ggplot(criterios_long, aes(x = rezago, y = valor, color = criterio)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title  = "Criterios de Información para Selección de Rezagos VAR",
    x      = "Número de rezagos (p)",
    y      = "Valor del criterio",
    color  = "Criterio"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

print(g_criterios)

# Seleccionar rezago óptimo por AIC 
p_optimo <- as.integer(seleccion_rezagos$selection["AIC(n)"])
cat("Rezago seleccionado (AIC):", p_optimo, "\n\n")

# -----------------------------------------------------------------------------
# 5. ESTIMACIÓN DEL MODELO VAR (PUNTO b - continuación)
# -----------------------------------------------------------------------------

# Estimar VAR con p_optimo rezagos y término constante
modelo_var <- vars::VAR(Y, p = p_optimo, type = "const")

cat("=== RESUMEN DEL MODELO VAR(", p_optimo, ") ===\n")
summary(modelo_var)

cat("\n--- Coeficientes: Ecuación IPC ---\n")
print(round(coef(modelo_var)$IPC, 4))

cat("\n--- Coeficientes: Ecuación ΔIBR ---\n")
print(round(coef(modelo_var)$dIBR, 4))

# -----------------------------------------------------------------------------
# 6. PRUEBA MULTIVARIADA DE LJUNG-BOX Y PROCESO TSAY (PUNTO c)
# -----------------------------------------------------------------------------

# ---- 6.1 Prueba inicial sobre residuales del VAR ----
cat("\n=== PRUEBA MULTIVARIADA DE LJUNG-BOX (MODELO INICIAL) ===\n")
cat("H0: No hay autocorrelación en los residuales\n")
cat("Si p-valor < 0.05 → evidencia de autocorrelación → ajustar modelo\n\n")
residuales <- residuals(modelo_var)
lb_inicial <- mq(residuales, lag = 20)

# ---- 6.2 Identificar coeficientes no significativos (proceso Tsay) ----
cat("\n=== IDENTIFICACIÓN DE COEFICIENTES NO SIGNIFICATIVOS (Tsay) ===\n")
resumen_var <- summary(modelo_var)

cat("P-valores ecuación IPC:\n")
print(round(resumen_var$varresult$IPC$coefficients[, "Pr(>|t|)"], 4))

cat("\nP-valores ecuación ΔIBR:\n")
print(round(resumen_var$varresult$dIBR$coefficients[, "Pr(>|t|)"], 4))

# ---- 6.3 Re-estimar modelo restringido eliminando no significativos ----
# thresh = 2.0 → elimina coeficientes con |t| < 2 (equivale a p > 0.05)
modelo_restringido <- restrict(modelo_var, method = "ser", thresh = 2.0)

cat("\n=== RESUMEN DEL MODELO VAR RESTRINGIDO ===\n")
summary(modelo_restringido)

# ---- 6.4 Ljung-Box sobre residuales del modelo restringido ----
cat("\n=== PRUEBA MULTIVARIADA DE LJUNG-BOX (MODELO RESTRINGIDO) ===\n")
residuales_rest <- residuals(modelo_restringido)
lb_restringido  <- mq(residuales_rest, lag = 20)

# -----------------------------------------------------------------------------
# 7. PRONÓSTICOS 12 MESES ADELANTE (PUNTO d)
# -----------------------------------------------------------------------------

# Generar pronósticos h = 12 pasos con intervalo de confianza al 95%
pronostico <- predict(modelo_restringido, n.ahead = 12, ci = 0.95)

cat("\n=== PRONÓSTICOS 12 MESES ADELANTE ===\n")

# Fechas futuras: octubre 2025 - septiembre 2026
ultima_fecha   <- max(datos$fecha)
fechas_futuras <- seq(ultima_fecha %m+% months(1),
                      by = "month", length.out = 12)

# Organizar pronósticos en dataframes
pron_ipc <- data.frame(
  fecha = fechas_futuras,
  pred  = pronostico$fcst$IPC[, "fcst"],
  lower = pronostico$fcst$IPC[, "lower"],
  upper = pronostico$fcst$IPC[, "upper"]
)

pron_dibr <- data.frame(
  fecha = fechas_futuras,
  pred  = pronostico$fcst$dIBR[, "fcst"],
  lower = pronostico$fcst$dIBR[, "lower"],
  upper = pronostico$fcst$dIBR[, "upper"]
)


cat("\nPronósticos IPC (variación anual %):\n")
print(data.frame(
  fecha = format(pron_ipc$fecha, "%B %Y"),
  pred  = round(pron_ipc$pred,  3),
  lower = round(pron_ipc$lower, 3),
  upper = round(pron_ipc$upper, 3)
))

cat("\nPronósticos ΔIBR (puntos porcentuales):\n")
print(data.frame(
  fecha = format(pron_dibr$fecha, "%B %Y"),
  pred  = round(pron_dibr$pred,  3),
  lower = round(pron_dibr$lower, 3),
  upper = round(pron_dibr$upper, 3)
))

# ---- Gráfica pronósticos IPC ----
datos_recientes <- datos[datos$fecha >= as.Date("2020-01-01"), ]

g_pron_ipc <- ggplot() +
  geom_line(data = datos_recientes,
            aes(x = fecha, y = ipc, color = "Histórico"), linewidth = 0.8) +
  geom_line(data = pron_ipc,
            aes(x = fecha, y = pred, color = "Pronóstico"),
            linewidth = 0.8, linetype = "dashed") +
  geom_ribbon(data = pron_ipc,
              aes(x = fecha, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "#1a6faf") +
  scale_color_manual(values = c("Histórico" = "#1a6faf", "Pronóstico" = "#e74c3c")) +
  labs(
    title    = "Pronóstico IPC - 12 meses adelante",
    subtitle = "Intervalo de confianza al 95%",
    x = NULL, y = "Variación anual (%)", color = NULL,
    caption  = "Fuente: Banco de la República / Cálculos propios"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

# ---- Gráfica pronósticos ΔIBR ----
g_pron_dibr <- ggplot() +
  geom_line(data = datos_recientes,
            aes(x = fecha, y = d_ibr, color = "Histórico"), linewidth = 0.8) +
  geom_line(data = pron_dibr,
            aes(x = fecha, y = pred, color = "Pronóstico"),
            linewidth = 0.8, linetype = "dashed") +
  geom_ribbon(data = pron_dibr,
              aes(x = fecha, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "#27ae60") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  scale_color_manual(values = c("Histórico" = "#27ae60", "Pronóstico" = "#e74c3c")) +
  labs(
    title    = "Pronóstico ΔIBR - 12 meses adelante",
    subtitle = "Intervalo de confianza al 95%",
    x = NULL, y = "ΔIBR (p.p.)", color = NULL,
    caption  = "Fuente: Banco de la República / Cálculos propios"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(g_pron_ipc)
print(g_pron_dibr)

# -----------------------------------------------------------------------------
# 8. FUNCIONES DE IMPULSO RESPUESTA - ORDEN ORIGINAL: IPC, ΔIBR (PUNTO e)
# -----------------------------------------------------------------------------

cat("\n=== IRF ORDEN ORIGINAL: [IPC, dIBR] ===\n")
cat("Supuesto Cholesky: IPC más exógeno (no responde contemporáneamente al IBR)\n\n")

# IRF ortogonalizadas con descomposición de Cholesky
# runs = 500: bootstrap para calcular bandas de confianza al 95%
irf_original <- irf(modelo_restringido,
                    n.ahead = 12,
                    ortho   = TRUE,
                    ci      = 0.95,
                    runs    = 500)

png("irf_orden_original.png", width = 1100, height = 900, res = 100)
plot(irf_original)
dev.off()
cat("IRF orden original guardadas en: irf_orden_original.png\n\n")
plot(irf_original)

# -----------------------------------------------------------------------------
# 9. FUNCIONES DE IMPULSO RESPUESTA - ORDEN INVERTIDO: ΔIBR, IPC (PUNTO f)
# -----------------------------------------------------------------------------

cat("\n=== RE-ESTIMACIÓN VAR CON ORDEN INVERTIDO: [dIBR, IPC] ===\n")
cat("Supuesto alternativo: IBR más exógeno (política monetaria lidera)\n\n")

# Reordenar series: ΔIBR primero, IPC segundo
Y2 <- cbind(
  dIBR = ts(datos$d_ibr, start = fecha_inicio, frequency = 12),
  IPC  = ts(datos$ipc,   start = fecha_inicio, frequency = 12)
)

# Re-estimar con el mismo número de rezagos
modelo_var2         <- vars::VAR(Y2, p = p_optimo, type = "const")
modelo_restringido2 <- restrict(modelo_var2, method = "ser", thresh = 2.0)

# IRF con orden invertido
irf_invertido <- irf(modelo_restringido2,
                     n.ahead = 12,
                     ortho   = TRUE,
                     ci      = 0.95,
                     runs    = 500)

png("irf_orden_invertido.png", width = 1100, height = 900, res = 100)
plot(irf_invertido)
dev.off()
cat("IRF orden invertido guardadas en: irf_orden_invertido.png\n\n")

plot(irf_invertido)


library(seasonalview)   # X-13ARIMA-SEATS
library(dplyr); library(lubridate)

# 1) Carga
ipc_var <- Real_IPC_2010_2024_Mensual

# 2) Pasar Mes español a número
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
           "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

ipc_var <- ipc_var %>% 
  mutate(
    Mes_num = match(Mes, meses),
    Fecha   = make_date(Anho, Mes_num, 1)   # 1° de cada mes
  ) %>% 
  arrange(Fecha)


ipc_ts <- ts(
  data      = ipc_var$`Variación IPC`,
  start     = c(2010, 1),        # año inicial, mes inicial
  frequency = 12                 # mensual
)



ajuste <- seas(
  ipc_ts,
  transform.function = "none",      # la serie ya está en %
  x11 = "",                         # método X-11; seats = "" también vale
  regression.aictest = c("td","easter"),   # días hábiles y Semana Santa
  outlier = NULL                    # detección automática de outliers
)

ipc_sa <- final(ajuste)             # serie desestacionalizada



summary(ajuste)          # mira M4 (≤ 1.8) y Ljung-Box
seasonalview(ajuste)     # espectro: sin pico en 12 meses = buen ajuste
# Serie original y ajustada
plot(cbind(Original = ipc_ts, Ajustada = ipc_sa),
     main = "IPC: variación mensual (NSA vs. SA)")

# Componentes X-11
plot(ajuste)        # 4 paneles: final, seasonal, trend, irregular





























# ============================================================
#  Desestacionalización Tasa de Desempleo (Chile, 2010-2024)
#  Procedimiento estándar X-13ARIMA-SEATS
# ============================================================

# ── 0) Paquetes ──────────────────────────────────────────────
library(seasonal)        # X-13ARIMA-SEATS
library(dplyr)
library(lubridate)

# ── 1) Cargar la base ───────────────────────────────────────
tdes <- Tasa_Desempleo_2010_2024_Mensual   # data.frame ya en memoria

meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

tdes <- tdes %>% 
  mutate(
    Mes_num = match(Mes, meses_es),
    Fecha   = make_date(Anho, Mes_num, 1)
  ) %>% 
  arrange(Fecha)

# ── 2) Serie ts  (porcentaje) ───────────────────────────────
desem_ts <- ts(tdes$`1. Nacional`, start = c(2010, 3), frequency = 12)

# ── 3) Ajuste X-13ARIMA-SEATS 100 % automático ──────────────
#     • transform.function = "none"  (ya es %)
#     • X-13 elegirá modelo ARIMA, TD, Easter, outliers, forecast 12 m
aj_desem <- seas(
  x                   = desem_ts,
  transform.function  = "none",
  x11                 = ""                # usa X-11 + SEATS por defecto
)

# ── 4) Serie desestacionalizada ─────────────────────────────
desem_sa <- final(aj_desem)               # Tasa de desempleo SA (%)

tdes <- tdes %>% 
  mutate(Desempleo_SA = as.numeric(desem_sa))

summary(aj_desem)


# desem_ts  : tasa de desempleo original (ts)
# desem_sa  : tasa de desempleo desestacionalizada (ts)

plot(desem_ts,
     col  = "black",  lwd = 1,
     ylab = "Porcentaje (%)",
     xlab = "Tiempo",
     main = "Tasa de desempleo nacional (NSA vs. SA)")

lines(ts(Desestacionalizados$Desempleo, start=c(2010,1), frequency = 12),
      col = "red",   lwd = 2)   # se dibuja encima de la negra

legend("topleft",
       legend = c("Original (NSA)", "Ajustada (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")




# ── 6) Exportar ─────────────────────────────────────────────
write.csv(tdes,
          "Tasa_Desempleo_Desestacionalizada.csv",
          row.names = FALSE)
























# ============================================================
#  Desestacionalización del IMACEC (Chile, 2010-2024)
#  Especificación estándar X-13ARIMA-SEATS
# ============================================================

# ── 0) Paquetes ──────────────────────────────────────────────
library(seasonal)       # X-13ARIMA-SEATS
library(dplyr)
library(lubridate)

# ── 1) Cargar y preparar la base ─────────────────────────────
imacec <- IMACEC_2010_2024_Mensual      # data-frame en memoria

meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

imacec <- imacec %>% 
  mutate(
    Mes_num = match(Mes, meses_es),
    Fecha   = make_date(Anho, Mes_num, 1)
  ) %>% 
  arrange(Fecha)

# ── 2) Serie ts (nivel del índice) ───────────────────────────
ima_ts <- ts(imacec$`1. Imacec`, start = c(2010, 1), frequency = 12)

# ── 3) Ajuste X-13ARIMA-SEATS (automático, multiplicativo) ──
ima_adj <- seas(
  x                   = ima_ts,
  transform.function  = "log",   # ⇒ desestacionalización multiplicativa
  x11                 = "",      # X-11 + SEATS
  regression.aictest  = c("td","easter"),   # efectos días hábiles + Semana Santa
  outlier             = ""       # detección automática
)

summary(ima_adj)


# — Extraer la serie ajustada —
imacec_sa <- final(ima_adj)  # regresar del log al nivel

# — Gráfico —
plot(ima_ts,
     col  = "black", lwd = 1,
     ylab = "Índice (base 2018 = 100)",
     xlab = "Tiempo",
     main = "IMACEC: Nivel original (NSA) vs. Desestacionalizado (SA)")

lines(imacec_sa, col = "red", lwd = 2)   # se dibuja encima

legend("topleft",
       legend = c("Original (NSA)", "Ajustado (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")




# ── 5) Diagnósticos M-stats ─────────────────────────────────
if ("quality" %in% ls("package:seasonal")) {
  diag <- quality(ima_adj)$diagnostics$summary
} else {
  diag <- ima_adj$diagnostics$mstat
}
diag[c("M4","QS")]      # M4 ≤ 1.8 y QS = 0 ⇒ ajuste válido

# ── 6) Exportar ─────────────────────────────────────────────
write.csv(imacec,
          "IMACEC_Desestacionalizado.csv",
          row.names = FALSE)






































# ============================================================
#  Paquetes y utilidades (cargar una sola vez)
# ============================================================
library(seasonal)      # X-13ARIMA-SEATS
library(dplyr); library(lubridate)

base <- Agregado_Monetario_2010_2024_Mensual   # tu data.frame
meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
base <- base %>%
  mutate(
    Mes_num = match(Mes, meses_es),
    Fecha   = make_date(Anho, Mes_num, 1)
  ) %>% arrange(Fecha)

# ============================================================
#  BLOQUE 1  —  M0
# ============================================================
m0_ts <- ts(base$M0, start = c(2010, 1), frequency = 12)

m0_adj <- seas(
  x                  = m0_ts,
  transform.function = "log",
  regression.aictest = c("td","easter"),
  x11                = "",
  outlier            = ""
)

M0_SA <- final(m0_adj)              # <- serie desestacionalizada en nivel


summary(m0_adj)


# — Gráfico —
plot(M0_SA,
     col  = "black", lwd = 1,
     ylab = "Índice (base 2018 = 100)",
     xlab = "Tiempo",
     main = "IMACEC: Nivel original (NSA) vs. Desestacionalizado (SA)")

lines(m0_ts, col = "red", lwd = 2)   # se dibuja encima

legend("topleft",
       legend = c("Original (NSA)", "Ajustado (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")

summary(m0_adj)


# ============================================================
#  BLOQUE 2  —  M1
# ============================================================
m1_ts <- ts(base$M1, start = c(2010, 1), frequency = 12)

m1_adj <- seas(
  x                  = m1_ts,
  transform.function = "log",
  regression.aictest = c("td","easter"),
  x11                = "",
  outlier            = ""
)

M1_SA <- final(m1_adj)
summary(m1_adj)


# — Gráfico —
plot(M1_SA,
     col  = "black", lwd = 1,
     ylab = "Índice (base 2018 = 100)",
     xlab = "Tiempo",
     main = "IMACEC: Nivel original (NSA) vs. Desestacionalizado (SA)")

lines(m1_ts, col = "red", lwd = 2)   # se dibuja encima

legend("topleft",
       legend = c("Original (NSA)", "Ajustado (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")


# ============================================================
#  BLOQUE 3  —  M2
# ============================================================
m2_ts <- ts(base$M2, start = c(2010, 1), frequency = 12)

m2_adj <- seas(
  x                  = m2_ts,
  transform.function = "log",
  regression.aictest = c("td","easter"),
  x11                = "",
  outlier            = ""
)

M2_SA <- final(m2_adj)
summary(m2_adj)

# — Gráfico —
plot(M2_SA,
     col  = "black", lwd = 1,
     ylab = "Índice (base 2018 = 100)",
     xlab = "Tiempo",
     main = "IMACEC: Nivel original (NSA) vs. Desestacionalizado (SA)")

lines(m2_ts, col = "red", lwd = 2)   # se dibuja encima

legend("topleft",
       legend = c("Original (NSA)", "Ajustado (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")

# ============================================================
#  BLOQUE 4  —  M3
# ============================================================
m3_ts <- ts(base$M3, start = c(2010, 1), frequency = 12)

m3_adj <- seas(
  x                  = m3_ts,
  transform.function = "log",
  regression.aictest = c("td","easter"),
  x11                = "",
  outlier            = ""
)

M3_SA <- final(m3_adj)
summary(m3_adj)

# — Gráfico —
plot(m3_ts,
     col  = "black", lwd = 1,
     ylab = "Índice (base 2018 = 100)",
     xlab = "Tiempo",
     main = "IMACEC: Nivel original (NSA) vs. Desestacionalizado (SA)")

lines(M3_SA, col = "red", lwd = 2)   # se dibuja encima

legend("topleft",
       legend = c("Original (NSA)", "Ajustado (SA)"),
       col    = c("black", "red"),
       lwd    = c(1, 2),
       bty    = "n")
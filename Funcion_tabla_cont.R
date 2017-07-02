## Funcion para generar tablas de contingencia
# Requiere los paquetes "vcd", "vcdExtra" y "data.table"
# 
# tabla.cont(df, x, y, pc="s", asoc = FALSE)
#
# Produce una tabla de frecuencias cruzadas de dos variables categóricas,
# excluyendo los NA. Además, calcula los porcentajes verticales u horizontales;
# los estadísticos de Chi Cuadrado de la tabla; las medidas de asociación 
# nominal basadas en Chi Cuadrado (Phi, Coeficiente de Contingencia y V de 
# Cramer); y el coeficiente Gamma de Goodman y Kruskal.
#
# Opciones:
#
# df : Data frame que contiene las variables (factores) de análisis
# x : Variable (factor) en las columnas, debe estar entre comillas
# y : Variable (factor) en las filas, debe estar entre comillas.
# pc : "s" sin porcentajes; "col" porcentaje en las columnas; "fila" porcentaje
#       en las filas
# asoc : si es TRUE produce los estadísticos de Chi Cuadrado y el Gamma de Goodman y Kruskal

tabla.cont <- function(df, x, y, pc="s", asoc = FALSE){
  library(vcd)
  library(vcdExtra)
  library(data.table)
  as <- c()
  gam <- c()
  misvars <- c(y, x)
  data <- na.omit(df[misvars])
  tab.0 <- table(data)
  asoc.0 <- assocstats(tab.0)
  gam.0 <- GKgamma(tab.0)
  if(pc=="s"){
    tab.1 <- addmargins(tab.0, 1, FUN = list(list(TOTALc = sum)))
    tab.1 <- addmargins(tab.1, 2, FUN = list(list(TOTALf = sum)))
    if(asoc==TRUE){
      as <- asoc.0
      gam <- gam.0}
  return(list(tab.1, as, gam))}
  if (pc=="col"){
    tab.2 <- addmargins(tab.0, 2, FUN = list(list(TOTAL = sum)))
    tab.2 <- round(prop.table(tab.2, 2)*100,2)
    tab.2 <- round(addmargins(tab.2, 1, FUN = list(list(TOTAL = sum))),1)
    tab.0 <- tab.0
    tab.0 <- addmargins(tab.0, 2, FUN = list(list(TOTAL = sum)))
    tab.0 <- addmargins(tab.0, 1, FUN = list(list(Nvalid = sum)))
    tab.2 <- rbind(tab.2, tail(tab.0, 1))
    if(asoc==TRUE){
      as <- asoc.0
      gam <- gam.0
    }
  return(list(tab.2, as, gam))}
  if (pc=="fila"){
    tab.2 <- addmargins(tab.0, 1, FUN = list(list(TOTAL = sum)))
    tab.2 <- round(prop.table(tab.2, 1)*100,2)
    tab.2 <- round(addmargins(tab.2, 2, FUN = list(list(TOTAL = sum))),1)
    tab.0 <- tab.0
    tab.0.1 <- addmargins(tab.0, 1, FUN = list(list(TOTAL = sum)))
    tab.0.1 <- addmargins(tab.0.1, 2, FUN = list(list(Nvalid = sum)))
    tab.2 <- cbind(tab.2, Nvalid=tab.0.1[, "Nvalid"])
    if(asoc==TRUE){
      as <- asoc.0
      gam <- gam.0
    }
    return(list(tab.2, as, gam))}
}

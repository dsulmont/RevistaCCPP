# CSES Data 2011

# Carga de datos del CSES moludo 3

load("cses3_p3.rdata")
load("gicel1.Rdata")

# Tabla IZDE de Partidos y elector

table(cses3.pe$C3011_B)
table(cses3.pe$C3011_C)
table(cses3.pe$C3013)

cses3.pe$izdehum <- cses3.pe$C3011_A
cses3.pe$izdefuji <- cses3.pe$C3011_B
cses3.pe$izdeppk <- cses3.pe$C3011_C
cses3.pe$izdetol <- cses3.pe$C3011_D
cses3.pe$izdee <- cses3.pe$C3013

cses3.pe$izdee[cses3.pe$izdee == 98] <- NA
cses3.pe$izdehum[cses3.pe$izdehum == 98] <- NA
cses3.pe$izdefuji[cses3.pe$izdefuji == 98] <- NA
cses3.pe$izdeppk[cses3.pe$izdeppk == 98] <- NA

table(cses3.pe$izdee)
table(cses3.pe$izdefuji)
table(cses3.pe$izdeppk)

## Desciptivos Izde 2011

library(pastecs)

stat.desc(cses3.pe$izdee)

myvars <- c("izdee", "izdehum", "izdefuji", "izdeppk")

izder <- cses3.pe[myvars]

pos.iz11 <- t(stat.desc(izder))


# Izquierda - derecha 2016

gicel1$izde.fuji <- gicel1$P19A
gicel1$izde.ppk <- gicel1$P19B
gicel1$izde.vero <- gicel1$P19C

gicel1$izde.fuji[gicel1$izde.fuji > 10] <- NA
gicel1$izde.ppk[gicel1$izde.ppk > 10] <- NA
gicel1$izde.vero[gicel1$izde.vero > 10] <- NA

myvars2 <- c("izde.e", "izde.fuji", "izde.ppk", "izde.vero")

gicel.izde <- gicel1[myvars2]

pos.iz16 <- t(stat.desc(gicel.izde))
pos.iz16

# Posiciones izquierda - derecha 2011 - 2016 

pos.iz11 <- as.data.frame(pos.iz11)
pos.iz11$year <- c("2011")
pos.iz11$eval <- c(1, 2, 3, 4)
pos.iz11

pos.iz16 <- as.data.frame(pos.iz16)
pos.iz16$year <- c("2016")
pos.iz16$eval <- c(1, 3, 4, 5)

pos.izder <- rbind(pos.iz11, pos.iz16)
pos.izder

pos.izder$eval <- factor(pos.izder$eval)
levels(pos.izder$eval) <- c("Elector", "Humala", "Fujimori", "PPK", "Mendoza")

pos.izder$mean10 <- pos.izder$mean*10
pos.izder$ci10 <- pos.izder$CI.mean.0.95*10

library(ggplot2)

g.izde11_16 <- ggplot(pos.izder, aes(x = eval, y = mean10)) + geom_point() +
  ylim(20, 80) +
  geom_errorbar(aes(ymin = mean10 - ci10, 
                    ymax = mean10 + ci10), width = 0.1) +
  facet_grid(.~ year, scales = "free", space = "free") +
  ylab("Escala Izquierda - Derecha") + xlab("") +
  theme_bw() +
  ggtitle("Posición media del elector y de los principales candidatos\n presidenciales 2011 y 2016 en la escala Izquierda - Derecha\nMedia e intervalo de confianza al 95%") +
  theme(plot.title = element_text(hjust = 0.5))

png("g_izde11y16.png", width = 600, height = 500, res = 100)
g.izde11_16
dev.off()

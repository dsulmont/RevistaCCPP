# DATOS

library(car)
library(ggplot2)
load("gicel1.Rdata")


# VOTO Y ESCALAS

table(gicel1$P6)
voto1v <- (as.numeric(gicel1$P6))-1
table(voto1v)

voto1v <- recode(voto1v, "7:10 = 7; 11:12 = 8; 13:hi=NA")
voto1v <- factor(voto1v)
levels(voto1v) <- c("Fujimori", "PPK", "Mendoza","Barnechea", "García", "Santos", "Otros", "B/V" )

table(gicel1$P10)

voto2v <- as.numeric(gicel1$P10)-1

voto2v <- recode(voto2v, "0=NA; 3:4=3; 5=4; 6=NA")
table(voto2v)

voto2v <- factor(voto2v)
levels(voto2v) <- c("Fujimori", "PPK", "B/V", "NS")

gicel1$voto1v <- voto1v
gicel1$voto2v <- voto2v


# Escalas según voto

library(Rmisc)
ilib1v <- summarySE(gicel1, measurevar = "ind.ilib", groupvars = "voto1v", na.rm=T)
estat1v <- summarySE(gicel1, measurevar = "ind.est", groupvars = "voto1v", na.rm=T)
izde1v <- summarySE(gicel1, measurevar = "izde.e", groupvars = "voto1v", na.rm=T)
ilib2v <- summarySE(gicel1, measurevar = "ind.ilib", groupvars = "voto2v", na.rm=T)
estat2v <- summarySE(gicel1, measurevar = "ind.est", groupvars = "voto2v", na.rm=T)
izde2v <- summarySE(gicel1, measurevar = "izde.e", groupvars = "voto2v", na.rm=T)



anova1 <- aov(gicel1$ind.ilib~gicel1$voto1v)
summary(anova1)
TukeyHSD(anova1)


anova2 <- aov(gicel1$ind.ilib~gicel1$voto2v)
summary(anova2)
TukeyHSD(anova2)

anova3 <- aov(gicel1$ind.est~gicel1$voto1v)
summary(anova3)
TukeyHSD(anova3)


anova4 <- aov(gicel1$ind.est~gicel1$voto2v)
summary(anova4)
TukeyHSD(anova4)

anova5 <- aov(gicel1$izde.e~gicel1$voto1v)
summary(anova5)
TukeyHSD(anova5)


anova6 <- aov(gicel1$izde.e~gicel1$voto2v)
summary(anova6)
TukeyHSD(anova6)


ilib1v

ggplot(ilib1v[-9, ], aes(x=voto1v, y=ind.ilib)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.ilib-ci, ymax=ind.ilib+ci), width=0.2) +
  ylim(30,70) + xlab("Voto 1ra vuelta") +
  ylab("Escala de Discrecionalidad") + geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Discrecionalidad en el Ejercicio del Poder según\n Voto en 1ra Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

ilib2v

ggplot(ilib2v[-5, ], aes(x=voto2v, y=ind.ilib)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.ilib-ci, ymax=ind.ilib+ci), width=0.2) +
  ylim(30,70) + xlab("Voto 2da vuelta") +
  ylab("Escala de Discrecionalidad") + geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Discrecionalidad en el Ejercicio del Poder según\n Voto en 2ra Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

estat1v

ggplot(estat1v[-9, ], aes(x=voto1v, y=ind.est)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.est-ci, ymax=ind.est+ci), width=0.2) +
  ylim(60,90) + xlab("Voto 1ra vuelta") +
  ylab("Escala de Estatismo") + geom_hline(aes(yintercept=76.77), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Estatismo Económico según Voto en\n 1ra Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

estat2v

ggplot(estat2v[-5, ], aes(x=voto2v, y=ind.est)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.est-ci, ymax=ind.est+ci), width=0.2) +
  ylim(60,90) + xlab("Voto 2da vuelta") +
  ylab("Escala de Estatismo") + geom_hline(aes(yintercept=76.77), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Estatismo Económico según Voto en\n 2da Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

izde1v
mean(gicel1$izde.e, na.rm=T)

ggplot(izde1v[-9, ], aes(x=voto1v, y=izde.e)) + geom_point() + 
  geom_errorbar(aes(ymin=izde.e-ci, ymax=izde.e+ci), width=0.2) +
  ylim(2,9) + xlab("Voto 1ra vuelta") +
  ylab("Izquierda - Derecha") + geom_hline(aes(yintercept=6.53), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Izquierda - Derecha según Voto en 1ra Vuelta\n de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

izde2v
mean(gicel1$izde.e, na.rm=T)

ggplot(izde2v[-5, ], aes(x=voto2v, y=izde.e)) + geom_point() + 
  geom_errorbar(aes(ymin=izde.e-ci, ymax=izde.e+ci), width=0.2) +
  ylim(2,9) + xlab("Voto 2da vuelta") +
  ylab("Izquierda - Derecha") + geom_hline(aes(yintercept=6.53), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Izquierda - Derecha según Voto en 2da Vuelta\n de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()



# Gráfico Matriz


escalas1v <- cbind(ilib1v[-9, c(1,3)], estat1v[-9, 3])
colnames(escalas1v) <- c("voto1v", "ind.ilib", "ind.estat")
escalas1v

escalas2v <- cbind(ilib2v[-5, c(1,3)], estat2v[-5, 3])
colnames(escalas2v) <- c("voto2v", "ind.ilib", "ind.estat")
escalas2v


escalas1v$tour <- c("1ra v.")
escalas2v$tour <- c("2da v.")
escalas2v

colnames(escalas1v)[1] <- "voto"
colnames(escalas2v)[1] <- "voto"

escalas.voto <- rbind(escalas1v, escalas2v)
escalas.voto

escalas1v

ggplot(escalas1v, aes(x=ind.estat, y=ind.ilib)) + geom_point() +
  xlim(70, 85) + ylim(45,60) +
  geom_hline(aes(yintercept=53.01), linetype="dashed", show.legend = T) +
  geom_vline(aes(xintercept=76.77), linetype="dashed", show.legend =  T) +
  geom_text(aes(label=voto), size=4, hjust=-0.15, vjust=-0.15)


escalas2v <- cbind(ilib2v[-5, c(1,3)], estat2v[-5, 3])
colnames(escalas2v) <- c("voto2v", "ind.ilib", "ind.estat")
escalas2v

ggplot(escalas2v, aes(x=ind.estat, y=ind.ilib)) + geom_point() +
  xlim(70, 85) + ylim(45,60) +
  geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = T) +
  geom_vline(aes(xintercept=76.77), linetype="dashed", show_guide = T) +
  geom_text(aes(label=voto2v), size=4, hjust=-0.15, vjust=-0.15)

g3.matriz <- ggplot(escalas.voto, aes(x=ind.estat, y=ind.ilib, shape=tour)) + geom_point(size=3) +
  xlim(71, 85) + ylim(49,59) +  geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = F) + scale_shape_manual(values=c(10,17)) + labs(shape="Elección") +
  geom_vline(aes(xintercept=76.77), linetype="dashed", show_guide = F) +
  geom_text(aes(label=voto), size=3, hjust=-0.2, vjust=-0.2) +
  xlab("Estatismo") + ylab("Discrecionalidad") +
  ggtitle("Media de los votantes de candidatos en 1ra y 2da vuelta en\n las escalas de Estatismo y Discrecionalidad\n (Las líneas punteadas representan al elector medio)") +
  theme_bw()

g3.matriz

ppi <- 300
png("gmatriz.png", width = 7*ppi, height = 4.5*ppi, res=ppi)
g3.matriz
dev.off()

```

# POSICIONES DE LOS CANDIDATOS


## Eje económico
```{r}
names(gicel1)
cand.eco <- gicel1[, 128:133]
names(cand.eco)

table(cand.eco$P37A)
table(gicel1$P37A)
cand.eco <- lapply(cand.eco, function (x) recode(x, "-1=NA; 12:99=NA"))
cand.eco <- as.data.frame(lapply(cand.eco, function (x) {x <- (((x-10)*(-1))*10)}))
names(cand.eco)

colnames(cand.eco) <- c("p37ar","p37br","p37cr","p37dr","p37er","p37fr")
library(pastecs)

stat.desc(cand.eco, basic=F)

```

## Eje discrecional

```{r}
names(gicel1)
cand.disc <- gicel1[, 134:139]
names(cand.disc)

table(cand.disc$P38A)
table(gicel1$P38A)
cand.disc <- lapply(cand.disc, function (x) recode(x, "-1=NA; 12:99=NA"))
cand.disc <- as.data.frame(lapply(cand.disc, function (x) {x <- (((x-10)*(-1))*10)}))
names(cand.disc)

colnames(cand.disc) <- c("p38ar","p38br","p38cr","p38dr","p38er","p38fr")

stat.desc(cand.disc, basic=F)

```

# OPCIONES SOBRE GASTO PÚBLICO

```{r}
table(gicel1$P1A)
names(gicel1)
gasto <-gicel1[, 10:17]

indx2 <- sapply(gasto, is.factor)

gasto[indx2] <- lapply(gasto[indx2], function(x) as.numeric(x))

table(gasto$P1A)

gasto[indx2] <- lapply(gasto[indx2], function (x) recode(x, "1=NA; 7:8=NA"))
gasto[indx2] <- lapply(gasto[indx], function (x) {x <- ((x-2)-4)*(-1)})
names(gasto)

fit <- princomp(na.omit(gasto), cor=T)
summary(fit)
loadings(fit)

library(psych)
fit <- principal(na.omit(gasto), nfactors=2, rotate="varimax")
fit
alpha(gasto)

gasto$ind.gasto <- rowSums(gasto)

gasto$ind.gasto <- (gasto$ind.gasto/32)*100

table(gasto$ind.gasto, exclude=NULL)
summary(gasto$ind.gasto)
hist(gasto$ind.gasto)

gicel1$ind.gasto <- gasto$ind.gasto

misvar <- c("ind.ilib", "ind.est", "izde.e", "ind.gasto")
data1 <- gicel1[misvar]
cor(na.omit(data1))

library(Hmisc)

rcorr(as.matrix(data1))


names(gasto)

gasto$izde <- gicel1$izde.e

rcorr(as.matrix(gasto))

```
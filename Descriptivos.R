library(Rmisc)

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

library(Rmisc)
ilib1v <- summarySE(gicel1, measurevar = "ind.ilib", groupvars = "voto1v", na.rm=T)
estat1v <- summarySE(gicel1, measurevar = "ind.est", groupvars = "voto1v", na.rm=T)
izde1v <- summarySE(gicel1, measurevar = "izde.e", groupvars = "voto1v", na.rm=T)
ilib2v <- summarySE(gicel1, measurevar = "ind.ilib", groupvars = "voto2v", na.rm=T)
estat2v <- summarySE(gicel1, measurevar = "ind.est", groupvars = "voto2v", na.rm=T)
izde2v <- summarySE(gicel1, measurevar = "izde.e", groupvars = "voto2v", na.rm=T)


ilib1v
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

ggplot(ilib1v[-9, ], aes(x=voto1v, y=ind.ilib)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.ilib-ci, ymax=ind.ilib+ci), width=0.2) +
  ylim(30,70) + xlab("Voto 1ra vuelta") +
  ylab("Escala de Discrecionalidad") + geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Discrecionalidad en el Ejercicio del Poder según\n Voto en 1ra Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

ggplot(ilib2v[-5, ], aes(x=voto2v, y=ind.ilib)) + geom_point() + 
  geom_errorbar(aes(ymin=ind.ilib-ci, ymax=ind.ilib+ci), width=0.2) +
  ylim(30,70) + xlab("Voto 2da vuelta") +
  ylab("Escala de Discrecionalidad") + geom_hline(aes(yintercept=53.01), linetype="dashed", show_guide = T) +
  ggtitle("Media e intervalo de confianza al 95% en la escala de\n Discrecionalidad en el Ejercicio del Poder según\n Voto en 2ra Vuelta de las Elecciones Presidenciales\n (Línea punteada representa a votante medio)") + theme_bw()

summarySE(data = gicel1, measurevar = "izde.e", groupvars = 
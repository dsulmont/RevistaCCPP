# Voto 2da Vuelta

table(gicel$voto2v)

library(car)

gicel$voto2v2 <- recode(gicel1$voto2v, "'B/V'=NA; 'NS'=NA")
table(gicel1$voto2v2)

gicel1$voto.fuji2 <- gicel1$voto2v2 == "Fujimori"
table(gicel1$voto.fuji2)


reg.voto2.a <- glm(voto.fuji2 ~ const.kf100 + const.ppk100 + mdo.kf100 + mdo.ppk100 +
                     ind.ilib + ind.est + izde.e10,
                   data = gicel1,
                   family = "binomial")
summary(reg.voto2.a)
NagelkerkeR2(reg.voto2.a)


reg.voto2.b <- glm(voto.fuji2 ~ const.kf100 + const.ppk100 + mdo.kf100 + mdo.ppk100 +
                     ind.ilib + ind.est + izde.e10 + gusta.kf100 + gusta.ppk100,
                   data = gicel1,
                   family = "binomial")
summary(reg.voto2.b)
NagelkerkeR2(reg.voto2.b)


# Gráfico

## Modelo 1
coef.2v <- confint(reg.voto2.a)

coef.2v <- cbind(coef.2v, coefficients(reg.voto2.a))

colnames(coef.2v) <- c("low", "hi", "b")

coef.2v <- as.data.frame(coef.2v)

idvar <- c("Constante", "Ley-Disc. KF", "Ley-Disc. PPK", "Mdo.-Est. KF",
           "Mdo.-Est. PPK", "Discrec.", "Estatism.", "Izq.-Der.")

coef.2v$idvar <- idvar
coef.2v$modelo <- 1
coef.2v

## Modelo 2

coef.2v2 <- confint(reg.voto2.b)
coef.2v2 <- cbind(coef.2v2, coefficients(reg.voto2.b))
colnames(coef.2v2) <- c("low", "hi", "b")

coef.2v2 <- as.data.frame(coef.2v2)

idvar <- c("Constante", "Ley-Disc. KF", "Ley-Disc. PPK", "Mdo.-Est. KF",
           "Mdo.-Est. PPK", "Discrec.", "Estatism.", "Izq.-Der.", "Pref. KF", "Pref. PPK")

coef.2v2$idvar <- idvar
coef.2v2$modelo <- 2
coef.2v2

coef.2dav <- rbind(coef.2v, coef.2v2)
coef.2dav <- subset(coef.2dav, idvar != "Constante")
coef.2dav

coef.2dav$modelo2 <- factor(coef.2dav$modelo, 
                            labels = c("Modelo 1, Pseudo R^{2} = 0.59",
                                       "Modelo 2, Pseudo R^{2},= 0.88"))

coef.2dav$modelo2 <- factor(coef.2dav$modelo, 
                            labels = c("Pseudo-R[1]^{2}==0.59",
                                       "Pseudo-R[2]^{2}==0.88"))

coef.2dav <- as.data.frame(coef.2dav)
coef.2dav

library(ggplot2)

g <- ggplot(coef.2dav, aes(x=idvar, y=b)) + geom_point() +
  geom_errorbar(aes(ymin = low, ymax = hi), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("b") +
  facet_grid(.~ modelo2, labeller = label_parsed) +
  coord_flip() + theme_bw() +
  ggtitle("Modelos de regresión logística binomial para intención\n de voto por Fujimori (1) vs PPK (0) en segunda vuelta\n Coeficientes e intervalo de confianza al 95%") +
  theme(plot.title = element_text(hjust = 0.5))

g
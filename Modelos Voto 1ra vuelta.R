# Voto 1ra vuelta

# Voltear variables

gicel1$mdo.kf100 <- ((gicel1$mdo.kf-10)*(-1))*10
gicel1$mdo.ppk100 <- ((gicel1$mdo.ppk-10)*(-1))*10
gicel1$mdo.vm100 <- ((gicel1$mdo.vm-10)*(-1))*10

gicel1$const.kf100 <- ((gicel1$const.kf-10)*(-1))*10
gicel1$const.ppk100 <- ((gicel1$const.ppk-10)*(-1))*10
gicel1$const.vm100 <- ((gicel1$const.vm-10)*(-1))*10


reg.votokf1 <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10,
                   data = gicel1,
                   family = "binomial")
summary(reg.votokf1)
NagelkerkeR2(reg.votokf1)

reg.votokf2 <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10+gusta.kf100,
                   data = gicel1,
                   family = "binomial")
summary(reg.votokf2)
NagelkerkeR2(reg.votokf2)


reg.votoppk1 <- glm(v.ppk1 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+
                      izde.e10,
                    data = gicel1,
                    family = "binomial")
summary(reg.votoppk1)
NagelkerkeR2(reg.votoppk1)

reg.votoppk2 <- glm(v.ppk1 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+
                      izde.e10+gusta.ppk100,
                    data = gicel1,
                    family = "binomial")
summary(reg.votoppk2)
NagelkerkeR2(reg.votoppk2)

reg.votovm1 <- glm(v.vm1 ~ const.vm100+mdo.vm100+ind.ilib+ind.est+
                     izde.e10,
                   data = gicel1,
                   family = "binomial")
summary(reg.votovm1)
NagelkerkeR2(reg.votovm1)

reg.votovm2 <- glm(v.vm1 ~ const.vm100+mdo.vm100+ind.ilib+ind.est+
                     izde.e10+gusta.vm100,
                   data = gicel1,
                   family = "binomial")
summary(reg.votovm2)
NagelkerkeR2(reg.votovm2)

# Data frame de coeficientes por candidato

b1.fuji1 <- as.data.frame(cbind(confint(reg.votokf1), coefficients(reg.votokf1)))
b1.fuji2 <- as.data.frame(cbind(confint(reg.votokf2), coefficients(reg.votokf2)))

b1.ppk1 <- as.data.frame(cbind(confint(reg.votoppk1), coefficients(reg.votoppk1)))
b1.ppk2 <- as.data.frame(cbind(confint(reg.votoppk2), coefficients(reg.votoppk2)))

b1.vm1 <- as.data.frame(cbind(confint(reg.votovm1), coefficients(reg.votovm1)))
b1.vm2 <- as.data.frame(cbind(confint(reg.votovm2), coefficients(reg.votovm2)))


b1.fuji1$cand <- 1
b1.fuji2$cand <- 1
b1.ppk1$cand <- 2
b1.ppk2$cand <- 2
b1.vm1$cand <- 3
b1.vm2$cand <- 3

b1.fuji1$mod <- "Modelo 1"
b1.fuji2$mod <- "Modelo 2"
b1.ppk1$mod <- "Modelo 1"
b1.ppk2$mod <- "Modelo 2"
b1.vm1$mod <- "Modelo 1"
b1.vm2$mod <- "Modelo 2"



var.n1 <- c("Constante", "Ley-Disc.", "Mdo.-Est.","Discrec.","Estatism.", "Izq.-Der.")
var.n2 <- c("Constante", "Ley-Disc.", "Mdo.-Est.","Discrec.","Estatism.", "Izq.-Der.",
            "Pref. Cand.")


b1.fuji1$var.n <- var.n1
b1.fuji2$var.n <- var.n2
b1.ppk1$var.n <- var.n1
b1.ppk2$var.n <- var.n2
b1.vm1$var.n <- var.n1
b1.vm2$var.n <- var.n2

b1.fuji1
b1.fuji2
b1.ppk1
b1.ppk2
b1.vm1
b1.vm2

coef.voto1v <- rbind(b1.fuji1,b1.fuji2,b1.ppk1,b1.ppk2,b1.vm1,b1.vm2)

coef.voto1v

coef.voto1v$cand <- factor(coef.voto1v$cand, labels = c("Fujimori", "PPK", "Mendoza"))

colnames(coef.voto1v) <- c("lo", "hi", "b", "Candidato", "Modelo", "var" )


coef.voto1v <- subset(coef.voto1v, var != "Constante")
coef.voto1v

library(ggplot2)

pd <- position_dodge(0.5) # move them .05 to the left and right

g.1ra <- ggplot(coef.voto1v, aes(x=var, y=b, shape=Candidato)) +
  geom_point(position = pd, size = 2) + 
  geom_errorbar(aes(ymin = lo, ymax=hi), width = 0.2, position = pd) +
  geom_hline(yintercept = 0, linetype = "dashed",colour = "#666666") +
  facet_grid(.~ Modelo) +
  coord_flip() + theme_bw() +
  ggtitle("Modelos de regresión logística binomial para voto\n en 1ra vuelta por principales candidatos\n Coeficientes e intervalo de confianza al 95%") +
  theme(plot.title = element_text(hjust = 0.5)) 

g.1ra

png("modelos1v.png", width = 800, height = 500, res = 100)
g.1ra
dev.off() 

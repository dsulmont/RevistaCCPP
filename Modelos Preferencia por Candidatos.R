# Variables sociodemográficas
names(gicel1)
table(gicel1$DOMINIO)
gicel1$nivedu <- as.numeric(gicel1$P54) - 2
gicel1$nse.r <- (as.numeric(gicel1$nse1)-6)*(-1)
table(gicel1$SEXO)
table(gicel1$EDAD)

# Variables en unidades porcentuales

gicel1$gusta.kf100 <- gicel1$gusta.kf*10
gicel1$gusta.ppk100 <- gicel1$gusta.ppk*10
gicel1$gusta.vm100 <- gicel1$gusta.vm*10

gicel1$const.kf100 <- gicel1$const.kf*10
gicel1$const.ppk100 <- gicel1$const.ppk*10
gicel1$const.vm100 <- gicel1$const.vm*10

gicel1$mdo.kf100 <- gicel1$mdo.kf*10
gicel1$mdo.ppk100 <- gicel1$mdo.ppk*10
gicel1$mdo.vm100 <- gicel1$mdo.vm*10

gicel1$izde.e10 <- gicel1$izde.e*10


# Regresión Gusta KF

reg.gustakf1 <- lm(gusta.kf100~SEXO+EDAD+nivedu+nse.r+DOMINIO, data = gicel1)
summary(reg.gustakf1)

reg.gustakf2 <- lm(gusta.kf100 ~ SEXO+EDAD+nivedu+nse.r+DOMINIO+
                     const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10, data = gicel1)
summary(reg.gustakf2)

# Regresión Gusta PPK

reg.gustappk1 <- lm(gusta.ppk100~SEXO+EDAD+nivedu+nse.r+DOMINIO, data = gicel1)
summary(reg.gustappk1)

reg.gustappk2 <- lm(gusta.ppk100 ~ SEXO+EDAD+nivedu+nse.r+DOMINIO+
                     const.ppk100+mdo.ppk100+ind.ilib+ind.est+izde.e10, data = gicel1)
summary(reg.gustappk2)

# Regresión Gusta VM

reg.gustavm1 <- lm(gusta.vm100~SEXO+EDAD+nivedu+nse.r+DOMINIO, data = gicel1)
summary(reg.gustavm1)

reg.gustavm2 <- lm(gusta.vm100 ~ SEXO+EDAD+nivedu+nse.r+DOMINIO+
                      const.vm100+mdo.vm100+ind.ilib+ind.est+izde.e10, data = gicel1)
summary(reg.gustavm2)

# Voto

table(gicel1$voto1v)

gicel1$v.fuji1 <- gicel1$voto1v == "Fujimori"
gicel1$v.ppk1 <- gicel1$voto1v == "PPK"
gicel1$v.vm1 <- gicel1$voto1v == "Mendoza"

gicel1$v.fuji2 <- gicel1$voto2v == "Fujimori"
gicel1$v.ppk2 <- gicel1$voto2v == "PPK"

table(gicel1$v.ppk2)

# Regresión Voto KF1

library(fmsb)

reg.votokf1.a <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10,
                     data = gicel1,
                     family = "binomial")
summary(reg.votokf1.a)
NagelkerkeR2(reg.votokf1.a)

reg.votokf1.b <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10+gusta.kf100,
                     data = gicel1,
                     family = "binomial")
summary(reg.votokf1.b)
NagelkerkeR2(reg.votokf1.b)

reg.votokf2.a <- glm(v.fuji2 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10,
                     data = gicel1,
                     family = "binomial")
summary(reg.votokf2.a)
NagelkerkeR2(reg.votokf2.a)

reg.votokf2.b <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10+gusta.kf100+
                       gusta.ppk100,
                     data = gicel1,
                     family = "binomial")
summary(reg.votokf2.b)
NagelkerkeR2(reg.votokf2.b)

# Regresión Voto PPK


reg.votoppk1.a <- glm(v.ppk1 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+izde.e10,
                     data = gicel1,
                     family = "binomial")
summary(reg.votoppk1.a)
NagelkerkeR2(reg.votoppk1.a)

reg.votoppk1.b <- glm(v.ppk1 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+izde.e10+
                        gusta.ppk100,
                      data = gicel1,
                      family = "binomial")
summary(reg.votoppk1.b)
NagelkerkeR2(reg.votoppk1.b)

reg.votoppk2.a <- glm(v.ppk2 ~  const.ppk100+mdo.ppk100+ind.ilib+ind.est+izde.e10,
                      data = gicel1,
                      family = "binomial")
summary(reg.votoppk2.a)
NagelkerkeR2(reg.votoppk2.a)

reg.votoppk2.b <- glm(v.ppk2 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+izde.e10+
                        gusta.kf100+gusta.ppk100,
                     data = gicel1,
                     family = "binomial")
summary(reg.votoppk2.b)
NagelkerkeR2(reg.votoppk2.b)

cor(gicel1$gusta.ppk100, gicel1$gusta.vm100, use = "complete.obs")

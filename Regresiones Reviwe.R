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

reg.gustappk2 <- lm(gusta.vm100 ~ SEXO+EDAD+nivedu+nse.r+DOMINIO+
                      const.vm100+mdo.vm100+ind.ilib+ind.est+izde.e10, data = gicel1)
summary(reg.gustappk2)

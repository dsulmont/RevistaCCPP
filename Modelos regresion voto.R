# Voto 1ra vuelta

reg.votokf1 <- glm(v.fuji1 ~ const.kf100+mdo.kf100+ind.ilib+ind.est+izde.e10+gusta.kf100,
                     data = gicel1,
                     family = "binomial")
summary(reg.votokf1)
NagelkerkeR2(reg.votokf1)

reg.votoppk1 <- glm(v.ppk1 ~ const.ppk100+mdo.ppk100+ind.ilib+ind.est+
                      izde.e10+gusta.ppk100,
                   data = gicel1,
                   family = "binomial")
summary(reg.votoppk1)
NagelkerkeR2(reg.votoppk1)

reg.votovm1 <- glm(v.vm1 ~ const.vm100+mdo.vm100+ind.ilib+ind.est+
                      izde.e10+gusta.vm100,
                    data = gicel1,
                    family = "binomial")
summary(reg.votovm1)
NagelkerkeR2(reg.votovm1)

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

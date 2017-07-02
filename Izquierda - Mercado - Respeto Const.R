# Izquierda - derecha 2016

gicel1$izde.kf <- gicel1$P19A
gicel1$izde.ppk <- gicel1$P19B
gicel1$izde.vm <- gicel1$P19C

gicel1$izde.kf[gicel1$izde.kf > 10] <- NA
gicel1$izde.ppk[gicel1$izde.ppk > 10] <- NA
gicel1$izde.vm[gicel1$izde.vm > 10] <- NA

# Estatismo - mercado 

gicel1$mdo.kf <- gicel1$P37A
gicel1$mdo.ppk <- gicel1$P37B
gicel1$mdo.vm <- gicel1$P37C

gicel1$mdo.kf <- recode(gicel1$mdo.kf, "-1=NA; 12:99=NA")
gicel1$mdo.ppk <- recode(gicel1$mdo.ppk, "-1=NA; 12:99=NA")
gicel1$mdo.vm <- recode(gicel1$mdo.vm, "-1=NA; 12:99=NA")

# Respeto constitución

gicel1$const.kf <- gicel1$P38A
gicel1$const.ppk <- gicel1$P38B
gicel1$const.vm <- gicel1$P38C

gicel1$const.kf <- recode(gicel1$const.kf, "-1=NA; 12:99=NA")
gicel1$const.ppk <- recode(gicel1$const.ppk, "-1=NA; 12:99=NA")
gicel1$const.vm <- recode(gicel1$const.vm, "-1=NA; 12:99=NA")

misvar <- c("izde.kf", "izde.ppk", "izde.vm", "mdo.kf", "mdo.ppk", "mdo.vm", 
            "const.kf", "const.ppk", "const.vm", "ind.est", "ind.ilib", "izde.e")
data <- gicel1[misvar]

rcorr(as.matrix(data))

misvar <- c("gusta.kf","gusta.ppk","gusta.vm", "mdo.kf", "mdo.ppk", "mdo.vm", 
            "const.kf", "const.ppk", "const.vm", "ind.est", "ind.ilib", "izde.e")
data <- gicel1[misvar]

library(Hmisc)
rcorr(as.matrix(data))

summarySE(data = gicel1, measurevar = "const.kf", groupvars = "gusta.kf", na.rm = T)


gicel1$g.kf2 <- cut(gicel1$gusta.kf, breaks = 3)
gicel1$g.ppk2 <- cut(gicel1$gusta.ppk, breaks = 3)
gicel1$g.vm2 <- cut(gicel1$gusta.vm, breaks = 3)

prop.table(table(g.kf2))*100
prop.table(table(g.ppk2))*100
prop.table(table(g.vm2))*100


prop.table(table(gicel1$voto1v, g.kf2),2)*100
prop.table(table(gicel1$voto1v, g.ppk2),2)*100
prop.table(table(gicel1$voto1v, g.vm2),2)*100

prop.table(table(gicel1$voto2v, g.kf2),2)*100
prop.table(table(gicel1$voto2v, g.ppk2),2)*100
prop.table(table(gicel1$voto2v, g.vm2),2)*100

summarySE(data = gicel1, measurevar = "izde.e", groupvars = c("g.kf2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "izde.e", groupvars = c("g.ppk2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "izde.e", groupvars = c("g.vm2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.ilib", groupvars = c("g.kf2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.ilib", groupvars = c("g.ppk2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.ilib", groupvars = c("g.vm2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.est", groupvars = c("g.kf2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.est", groupvars = c("g.ppk2"), 
          na.rm = T)

summarySE(data = gicel1, measurevar = "ind.est", groupvars = c("g.vm2"), 
          na.rm = T)

summary(aov(ind.est~g.ppk2, data = gicel1))

TukeyHSD(aov(ind.est~g.ppk2, data = gicel1))

summary(aov(ind.ilib~g.vm2, data = gicel1))

names(gicel1)

summarySE(data = gicel1, measurevar = "const.kf", 
          groupvars = c("ind.ilib.g", "g.kf2"), na.rm=T)

reg.gustakf <- lm(gusta.kf~const.kf+ind.ilib+ind.est+mdo.kf+izde.e, data = gicel1)

reg.gusta.ppk <- lm(gusta.ppk~const.ppk+ind.ilib+ind.est+mdo.ppk+izde.e, data = gicel1)

reg.gusta.vm <- lm(gusta.vm~const.vm+ind.ilib+ind.est+mdo.vm+izde.e, data = gicel1)

library(MASS)

summary(reg.gustakf)
summary(reg.gusta.ppk)
summary(reg.gusta.vm)

names(gicel1)


library(Rmisc)
summarySE(data = gicel1, measurevar = "ind.ilib", groupvars = "izdee.g", na.rm = T)
summarySE(data = gicel1, measurevar = "ind.est", groupvars = "izdee.g", na.rm = T)

summary(aov(ind.est~izdee.g, data = gicel1))

TukeyHSD(aov(ind.est~izdee.g, data = gicel1))

cor.test(gicel1$izde.e, gicel1$ind.est, use = "complete.obs")

tabla.cont(gicel1, "ind.ilib.g", "izdee.g", pc = "col", asoc = T)

names(gicel1)


summary(lm(izde.kf~const.kf+mdo.kf, data = gicel1))
summary(lm(izde.ppk~const.ppk+mdo.ppk, data = gicel1))
summary(lm(izde.vm~const.vm+mdo.vm, data = gicel1))

boxplot(const)

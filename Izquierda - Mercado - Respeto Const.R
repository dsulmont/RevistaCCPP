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

rcorr(as.matrix(data))

summarySE(data = gicel1, measurevar = "const.kf", groupvars = "gusta.kf", na.rm = T)


summary(data)



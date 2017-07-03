p24 <- gicel1$P24

p24[is.na(p24)] <- "Sin respuesta"

data.p24 <- data.frame(model.matrix(~p24))

names(data.p24)

data.p24.r <- data.p24[, c(2,3,4)]
names(data.p24.r)
colnames(data.p24.r) <- c("PF", "PPK", "FA")

part <- data.p24.r


gicel1 <- cbind(gicel1, part)
names(gicel1)

load("gicel.t.Rdata")
names(gicel.t)

str(gicel.t$candidato2)

summary(gicel.t$izde.part)

gicel.t$izde.part10 <- gicel.t$izde.part*10

library(Rmisc)

# Posición de los candidatos

s.iz.c <- summarySE(data = gicel.t, measurevar = "izde.part10", groupvars = "candidato2", na.rm = T)
s.est.c <- summarySE(data = gicel.t, measurevar = "estat.cand", groupvars = "candidato2", na.rm = T)
s.dis.c <- summarySE(data = gicel.t, measurevar = "disc.cand", groupvars = "candidato2", na.rm = T)

nombres <- c("candidato", "n", "media", "sd", "se", "ci")

colnames(s.iz.c) <- nombres
colnames(s.est.c) <- nombres
colnames(s.dis.c) <- nombres

s.iz.c$var <- c(1)
s.est.c$var <- c(2)
s.dis.c$var <- c(3)



pos.cand <- rbind(s.iz.c, s.est.c, s.dis.c)
pos.cand$var <- factor(pos.cand$var)
levels(pos.cand$var) <- c("Izq.-Der", "Mercado - Estado", "Ley - Disc.")
pos.cand

library(ggplot2)

g.pos.cand <- ggplot(pos.cand, aes(x=candidato, y=media)) + geom_point() +
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width = 0.2) +
  ylim(20,100) + xlab("") + ylab("") +
  facet_grid(.~var) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Posición de los candidatos en las escalas de Izquierda -\n Derecha, Mercado - Estado y Respeto a la Ley\n Media e intervalo de confianza al 95%") +
  theme(plot.title = element_text(hjust = 0.5))

g.pos.cand

# Posición del Elector:

data.pose <- subset(gicel.t, candidato == "P18A")

s.iz.e <- summarySE(data = data.pose, measurevar = "izde.e10", groupvars = "voto1v", na.rm = T)
s.est.e <- summarySE(data = data.pose, measurevar = "ind.est", groupvars = "voto1v", na.rm = T)
s.dis.e <- summarySE(data = data.pose, measurevar = "ind.ilib", groupvars = "voto1v", na.rm = T)

nombres <- c("voto", "n", "media", "sd", "se", "ci")

colnames(s.iz.e) <- nombres
colnames(s.est.e) <- nombres
colnames(s.dis.e) <- nombres

s.iz.e$var <- c(1)
s.est.e$var <- c(2)
s.dis.e$var <- c(3)

pos.elect <- rbind(s.iz.e, s.est.e, s.dis.e)
pos.elect$var <- factor(pos.elect$var)
levels(pos.elect$var) <- c("Izq.-Der", "Estatismo", "Discrecionalidad")


excluir <- c(7,8,9,16,17,18,25,26,27)

pos.elect <- pos.elect[-(excluir), ]
pos.elect <- subset(pos.elect, voto == "Fujimori" | voto == "PPK" | voto == "Mendoza")

g.pos.elec <- ggplot(pos.elect, aes(x=voto, y=media)) + geom_point() +
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width = 0.2) +
  ylim(20,100) + xlab("") + ylab("") +
  facet_grid(.~var) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Posición del Elector en las escalas de Izquierda - Derecha,\n Estatismo y Discrecionalidad, según voto en 1ra Vuelta\n Media e intervalo de confianza al 95%") +
  theme(plot.title = element_text(hjust = 0.5))

g.pos.elec


png("pos_elec.png", width = 600, height = 500, res = 100)
g.pos.elec
dev.off()

png("pos_cand.png", width = 600, height = 500, res = 100)
g.pos.cand
dev.off()

# Anticandidatos

gicel.t$pref.c <- cut(gicel.t$gusta.c, breaks = 3)
levels(gicel.t$pref.c) <- c("Negativa", "Media", "Positiva")
table(gicel.t$pref.c) 

anti1 <- summarySE(gicel.t, measurevar = "disc.cand", groupvars = c("candidato2", "pref.c"), 
          na.rm = T)

summarySE(gicel.t, measurevar = "estat.cand", groupvars = c("candidato2", "pref.c"), 
          na.rm = T)

summarySE(gicel.t, measurevar = "ind.ilib", groupvars = c("candidato2", "pref.c"), 
          na.rm = T)

anti1 <- na.omit(anti1[1:8, ])
anti1

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(anti1, aes(x=pref.c, y=disc.cand, group = candidato2, shape=candidato2)) +
  geom_errorbar(aes(ymin=disc.cand-ci, ymax=disc.cand+ci), width=0.2, position=pd) +
  geom_line(aes(linetype=candidato2), position=pd) +
  geom_point(position=pd, size = 2.5) +
  ylim(0,100)



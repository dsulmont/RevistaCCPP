library(Rmisc)
names(gicel.t)
sum.iz.est <- summarySE(data = gicel.t, measurevar = "izde.part10", 
          groupvars = c("candidato2", "estat.cand"), na.rm=T)

sum.iz.dis <- summarySE(data = gicel.t, measurevar = "izde.part10", 
          groupvars = c("candidato2", "disc.cand"), na.rm=T)


nombres <- c("cand", "escala", "n", "izde.p", "sd", "se", "ci")

colnames(sum.iz.dis) <- nombres
colnames(sum.iz.est) <- nombres

sum.iz.dis$var <- "Ley - Disc."
sum.iz.est$var <- "Mercado - Estado"

sum.pos.cand <- rbind(sum.iz.dis, sum.iz.est)
sum.pos.cand

df.sum <- na.omit(subset(sum.pos.cand, cand %in% c("Fujimori", "PPK", "Mendoza")))

library(ggplot2)

g.mdol<- ggplot(df.sum, aes(x=escala, y=izde.p, group = cand, shape = cand)) + 
  geom_point(size = 2) + geom_line(aes(linetype = cand)) + 
  scale_shape_discrete(name = "Candidatos") +
  scale_linetype_discrete(name = "Candidatos") +
  xlab("") + ylab("Media Izq. - Der.") + ylim(0, 100) +
  facet_grid(.~var) + theme_bw() +
  ggtitle("Posición media de los candidatos en las escala Izquierda -\n Derecha, según ubicación en escalas de\n Mercado - Estado y Respeto a la Ley, por candidato") +
  theme(plot.title = element_text(hjust = 0.5))

g.mdol
  
png("cand_mdo_ley.png", width = 600, height = 500, res = 100)
g.mdol
dev.off() 
  
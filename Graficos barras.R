
names(gicel.t)
t1 <- as.data.frame(prop.table(table(cut(gicel.t$disc.cand, breaks=3), 
                                     gicel.t$candidato2),2)*100)

t2 <- as.data.frame(prop.table(table(cut(gicel.t$estat.cand, breaks=3), 
                                     gicel.t$candidato2),2)*100)

t3 <- as.data.frame(prop.table(table(cut(gicel.t$izde.part, breaks=3), 
                                     gicel.t$candidato2),2)*100)

t1$ind <- "Ley - Disc."
t2$ind <- "Mercado - Estado"
t3$ind <- "Izquierda - Derecha"

t.comb <- rbind(t1, t2, t3)

ggplot(t1, aes(x=Var2, y=Freq, fill = Var1)) + 
  geom_bar(stat = "identity", position=position_fill(reverse = T)) + 
  scale_fill_grey(start = 0.8, end = 0.25) + theme_bw() +
  coord_flip()



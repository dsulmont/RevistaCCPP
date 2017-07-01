library(foreign)
gicel.2 <- read.spss("GICEL2.sav", to.data.frame = T)

names(gicel.2)

gicel.2a <- gicel.2[, c(1, 240:269)] 
names(gicel.2a)

gicel.3 <- merge(gicel1, gicel.2a, by="NRO", all=T)
names(gicel.3)

table(gicel.3$PANEL)
table(gicel.3$P8_PANEL)

gicel.3$izde_panel <- gicel.3$P8_PANEL

gicel.3$izde_panel[gicel.3$izde_panel > 10 | gicel.3$izde_panel < 0] <- NA
table(gicel.3$izde_panel)

t.test(gicel.3$izde.e, gicel.3$izde_panel, paired = T)

gicel_panel <- subset(gicel.3, PANEL == "Sí participó")

table(gicel_panel$P3_PANEL)


prop.table(table(gicel_panel$P3_PANEL, gicel_panel$P10),2)*100

names(gicel_panel)


table(gicel_panel$P8_PANEL)
table(gicel_panel$izde_panel)


t.test(gicel_panel$izde.e, gicel_panel$izde_panel, paired = T)


mis
data<-read.csv("Bats and Plants.csv")
data$mtype <-factor(data$mtype)

#name the rows
rownames(data) <- c("Sp_1", "NL", "MC1", "MP", "PP", "Ce", "Ca", "MC2", "FB", "ME", "MA", "DY", "AM", "MT", "An")


#run the NMDS - Bray Curtis distances, 20 attempts, track final stress values
library(vegan)
ord <- metaMDS(data, distance = "bray", trymax=20 ,trace=1)

#plot
#vegan interprets rows and columns as sites and species automatically, so you can just type "sites" or "species" for what you want to plot - you don't have to define those - here I'm pretending the plants are "sites"

plot(ord)
ordilabel(ord, display = "sites", font = 3, fill = "indianred", col = "white")
ordilabel (ord, display = "species", font = 2, fill ="steelblue", col = "white")
#import dataset, without an initial row of row names; call it DATA
#columns should all be numeric
#define row names

comm2<-read.csv("2012_abundancematrix.csv")

#use this one to give the site names
rownames(comm2) <- c("B1GA", "B1GB", "BIUA", "B1UB", "B2GA", "B2GB", "B2UA", "B2UB", "I1GA", "I1GB", "I1UA", "I1UB", "I2GA", "I2GB", "I2UA", "I2UB", "K1GA", "K1GB", "K1UA", "K1UB", "K2GA", "K2GB", "K2UA", "K2UB")

#or use this one to give the bank heights
rownames(comm2) <- c("77", "17", "177", "117", "82", "19", "182", "119", "65", "30", "165", "130", "99", "13", "199", "113", "196", "29", "296", "129", "126", "34", "226", "134")


#run the NMDS
library(vegan)
ord <- metaMDS(comm2)

#plot the ordination without species showing, and with sites shown in text
#vegan interprets rows and columns as sites and species automatically, so you can just type "sites" or "species" for what you want to plot - you don't have to define those
#cex varies the text size
plot(ord, type="n")
text(ord, display ="sites", cex =0.7)
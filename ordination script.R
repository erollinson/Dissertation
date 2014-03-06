#import dataset, without an initial row of row names; call it COMM
#columns should all be numeric
#define row names

comm<-read.csv("2012_abundancematrix.csv")
comm$mtype <-factor(comm$mtype)

#use this one to give the site names
rownames(comm) <- c("B1GA", "B1GB", "BIUA", "B1UB", "B2GA", "B2GB", "B2UA", "B2UB", "I1GA", "I1GB", "I1UA", "I1UB", "I2GA", "I2GB", "I2UA", "I2UB", "K1GA", "K1GB", "K1UA", "K1UB", "K2GA", "K2GB", "K2UA", "K2UB")

#or use this one to give the bank heights
rownames(comm) <- c("77", "17", "177", "117", "82", "19", "182", "119", "65", "30", "165", "130", "99", "13", "199", "113", "196", "29", "296", "129", "126", "34", "226", "134")


#run the NMDS - Bray Curtis distances, 20 attempts, track final stress values
library(vegan)
ord <- metaMDS(comm, distance = "bray", trymax=20 ,trace=1)

#plot the ordination without species showing, and with sites shown in text
#vegan interprets rows and columns as sites and species automatically, so you can just type "sites" or "species" for what you want to plot - you don't have to define those
#cex varies the text size
plot(ord, type="n")
text(ord, display ="sites", cex =0.7)

#plot to look at the sites with the species names superimposed
plot(ord)
text(ord, display="species", cex=0.7)

#clean up the plot - give plotting priority to species that are the most abundant (draw the more common ones on top when labels must overlap.  invsimpson is Hill's N2, the inverse of the Simpson diversity measure.  for MARGIN, 1 = rows and 2 = columns, so we use 2 here to indicate priority for species labels)
priSPP<-diversity(comm, index = "invsimpson", MARGIN = 2)
plot(ord)
ordilabel(ord, display = "sites", font = 3, fill = "goldenrod1", col = "black")
ordilabel (ord, display = "species", font = 2, priority = priSPP)

#to plot side by side instead
priSPP<-diversity(comm, index = "invsimpson", MARGIN = 2)
layout(matrix(1:2, ncol=2))
plot(ord, type = "n")
ordilabel(ord, display = "sites", font = 3, fill = "goldenrod1", col = "black", scaling = scl)
plot(ord, type = "n")
ordilabel(ord, display = "species", font = 2, priority = priSPP)

#make a better ordination plot
library(ggplot2)
pts<-as.data.frame(ord$points)
ord2<-cbind(comm,pts)
write.csv(ord2, file="NMDS_GL_US.csv")
#add columns for bank type and river in excel, then reimport
ord3<-read.csv("NMDS_GL_US.csv")
ord3$BankType<-factor(ord3$Bank.Type, levels=c("greenline", "upslope"), labels=c("greenline", "upslope"))
ord3$River<-factor(ord3$River, levels=c("Ballston", "Indian", "Kayaderosseras"), labels=c("Ballston", "Indian", "Kayaderosseras"))
plot<-qplot(MDS1, MDS2, data=ord3)


#these aren't working, they're plotting the points twice and not stratifying the shapes and fill as I need
plot + geom_point(aes(shape=River), size=4) + geom_point(aes(fill=BankType), size=4) + scale_shape_manual(values=c(1, 2, 5)) + scale_fill_manual(values=c("white", "black")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("NMDS1") + ylab("NMDS2")

plot + geom_point(aes(shape=River, fill=BankType), size=4) + scale_shape_manual(values=c(1, 2, 5)) + scale_fill_manual(values=c("white", "black")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("NMDS1") + ylab("NMDS2") 
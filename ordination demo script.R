library(ggplot2)
library(vegan)

b <- read.csv("butterflies.csv")
b$mtype <- factor(b$mtype)

# Perform mds
mds <- metaMDS(b[, -(1:3)], k = 2, distance = 'bray', 
  autotransform = F, expand = F)
str(mds)
# Some different plotting options
# - the basic idea is to extract the ordination from mds and then do
# all the plots ourselves

pts <- as.data.frame(mds$points)
ord <- cbind(b, pts)

# Your turn: what can you say about the differences between meadow types and 
# regions.  What species are making the most difference?

qplot(MDS1, MDS2, data = ord)
qplot(MDS1, MDS2, data = ord, colour = mtype)
qplot(MDS1, MDS2, data = ord, colour = mtype) + facet_wrap(~ region)

qplot(MDS1, MDS2, data = ord, colour = year)
qplot(MDS1, MDS2, data = ord, colour = region)
qplot(MDS1, MDS2, data = ord, colour = region, geom = "path" , arrow = arrow(length = unit(0.05, "npc"))) + 
  facet_wrap(~ mtype)

qplot(V1, V2, data = ord, size = Plebejus.saepiolus) + scale_area()
qplot(V1, V2, data = ord, size = Speyeria.mormonia) + scale_area()


# Is there a difference over time? -------------------------------------------

# Focus on Teutons since they have the most data over time
teutons <- subset(ord, region == "T")

qplot(V1, V2, data = teutons, colour = mtype, geom = "path", 
  arrow = arrow(length = unit(0.05, "npc")))

# Compute averages
delta <- ddply(teutons, "mtype", summarise, 
  V1_v = diff(V1) / diff(year), 
  V2_v = diff(V2) / diff(year))
qplot(V1_v, V2_v, data = delta, colour = mtype, geom = "path")

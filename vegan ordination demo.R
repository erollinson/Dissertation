#Ordinations in vegan

#vegan uses a site by species matrix - it can actually be anything, but the package uses the terms "sites" and "species" throughout, where "sites" are rows and "species" are columns - reframe these terms for your own purposes accordingly, but you'll have to use the terms "sites" and "species" in the code.

#running a metaMDS gives you a distance matrix, you can choose any distance measure out of the following:  "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao" or "cao".

#The MDS won't run if your matrix has extra columns that are factors (e.g., row names as a column) so make sure that the data frame's row names are defined properly in the data frame as "row.names" and not just as a named columns.  You can hack your way around this if need be by doing this:

data<-read.csv("yourdata.csv")
data2<-data[,-1] #this makes a copy of your data frame minus the first column (subtract as many as necessary to get to just your matrix of numbers and no columns containing factors); you'll do analyses on this copy
rownames(data2)<-data[,1] #this takes the first column of your original data frame and makes it the row names in the new data frame, in a format metaMDS can understand


#code for running the metaMDS


#read in the site by species matrix, n.b. this data frame has no row names so it will just plot the row numbers
comm<-read.csv("2012_abundancematrix.csv")


#run the NMDS - Bray Curtis distances, 20 attempts, trace=1 tracks the stress values,  k=whatever number of dimensions you want.
library(vegan)
ord <- metaMDS(comm, distance = "bray", k=2, trymax=20 ,trace=1)


#plotting

plot(ord) #plots both sites and species

#plot the ordination without species showing, and with sites shown in text
#vegan interprets rows and columns as sites and species automatically, so you can just type "sites" or "species" for what you want to plot - you don't have to define those
#cex varies the text size

plot(ord, type="n")
text(ord, display ="sites", cex =0.7) #this just plots row numbers

#plot to look at the sites with the species (column) names superimposed
plot(ord)
text(ord, display="species", cex=0.7)


#the metaMDS function produces the output in an odd format, so use this if you want the distances in a normal data frame to export to other places or use for other purposes

pts<-as.data.frame(ord$points) #this gives you a data frame with columns for each NMDS





#transform data frame to site x species matrix

library(reshape)
cquada <- cast(cquad, Name + Quad ~ Species, value='Count', FUN=mean)
cquada <-as.data.frame(cquada)
cquada[is.na(cquada)] = 0
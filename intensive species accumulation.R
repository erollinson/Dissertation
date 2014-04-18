
#load packages
require(vegan)
require(RCurl)
require(reshape)

#import data
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
raw <- getURL("mydata") #insert the  raw URL for the data file on github here
data <- read.csv(text = raw) #read in the github file


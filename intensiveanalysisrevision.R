#import datasheet as "data"

require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
raw <- getURL("https://raw.github.com/erollinson/Dissertation/master/2012%20Data%20Summary%20with%20Averages%203_3_14%20for%20R.csv") #insert the  raw URL for the data file on github here
data <- read.csv(text = raw) #read in the github file

#importing the second data sheet with species origin as a condition (for figures to share axes)

require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
raw <- getURL("https://raw.github.com/erollinson/Dissertation/master/2012%20Data%20Summary%20with%20Averages%203_3_14%20for%20R%20by%20origin%20for%20figure%20merge.csv") #insert the  raw URL for the data file on github here
databyor <- read.csv(text = raw) #read in the github file



#Poisson regression for: species richness (#int; #nat), # indivs (#int; #nat) - unless they are seriously over/under dispersed or have a lot of zeroes


#logistic regression for: %cover (%int; %nat)


#multinomial diversity model MDM for Shannon diversity (see De'ath 2012 Ecology 93:2286-2296)
#this needs to start with a site by species abundance matrix, create a class MDM out of it
require(MDM)
data(spider6)
fit0<- mdm(y2p(spider6[,1:6])~1, data=spider6)
fit1 <-mdm(y2p(spider6[,1:6])~Water, data=spider6)
fit2 <-mdm(y2p(spider6[,1:6])~Water+Herbs, data=spider6)
fit3 <-mdm(y2p(spider6[,1:6])~Site, data=spider6, alpha=TRUE)
anova(fit0,fit1,fit2,fit3)

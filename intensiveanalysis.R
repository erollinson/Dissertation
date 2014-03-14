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


#load libraries
library(lme4)
library(ggplot2)
library(nlme)

#add overall cover (inv + nat) to data

summed<-rowSums(data[,c(28,29)])
data <-cbind(data, summed)
names(data)[names(data)=="summed"] <- "HerbCov"

#to look at qq plots
ggplot(data, aes(sample=CountSp)) + stat_qq()
ggplot(data, aes(sample=CountSpPerM)) + stat_qq()
ggplot(data, aes(sample=CountInd)) + stat_qq()
ggplot(data, aes(sample=CountIndPerM)) + stat_qq()
ggplot(data, aes(sample=ShanDiv)) + stat_qq()
ggplot(data, aes(sample=CountInvSp)) + stat_qq()
ggplot(data, aes(sample=CountInvSpPerM)) + stat_qq()
ggplot(data, aes(sample=CountNatSp)) + stat_qq()
ggplot(data, aes(sample=CountNatSpPerM)) + stat_qq()
ggplot(data, aes(sample=CountInvInd)) + stat_qq()
ggplot(data, aes(sample=CountInvIndPerM)) + stat_qq()
ggplot(data, aes(sample=CountNatInd)) + stat_qq()
ggplot(data, aes(sample=CountNatIndPerM)) + stat_qq()
ggplot(data, aes(sample=InvHerbCov)) + stat_qq()
ggplot(data, aes(sample=NatHerbCov)) + stat_qq()

#to test for normality (P>0.05 = normal)

shapiro.test(data$CountSp)
shapiro.test(data$CountSpPerM)
shapiro.test(data$CountInd)
shapiro.test(data$CountIndPerM)
shapiro.test(data$ShanDiv)
shapiro.test(data$CountInvSp)
shapiro.test(data$CountInvSpPerM)
shapiro.test(data$CountNatSp)
shapiro.test(data$CountNatSpPerM)
shapiro.test(data$CountInvInd)
shapiro.test(data$CountInvIndPerM)
shapiro.test(data$CountNatInd)
shapiro.test(data$CountNatIndPerM)
shapiro.test(data$InvHerbCov)
shapiro.test(data$NatHerbCov)

#response variables that are non-normal are: CountInd, ShanDiv, CountInvInd, CountInvIndPerM, CountNatInd, CountNatIndPerM, InvHerbCov, NatHerbCov
#transform counts and ShanDiv as square root; transform cover data as arcsin

#to transform data and add a column to the dataframe with the transformed data
data <-cbind(data, sqrt(data$CountInd))
names(data)[names(data)=="sqrt(data$CountInd)"] <- "SQRTCountInd"

data <-cbind(data, sqrt(data$ShanDiv))
names(data)[names(data)=="sqrt(data$ShanDiv)"] <- "SQRTShanDiv"

data <-cbind(data, sqrt(data$CountInvInd))
names(data)[names(data)=="sqrt(data$CountInvInd)"] <- "SQRTCountInvInd"

data <-cbind(data, sqrt(data$CountInvIndPerM))
names(data)[names(data)=="sqrt(data$CountInvIndPerM)"] <- "SQRTCountInvIndPerM"

data <-cbind(data, sqrt(data$CountNatInd))
names(data)[names(data)=="sqrt(data$CountNatInd)"] <- "SQRTCounNattInd"

data <-cbind(data, sqrt(data$CountNatIndPerM))
names(data)[names(data)=="sqrt(data$CountNatIndPerM)"] <- "SQRTCountNatIndPerM"

data <-cbind(data, asin(data$InvHerbCov))
names(data)[names(data)=="asin(data$InvHerbCov)"] <- "ASINInvHerbCov"

data <-cbind(data, asin(data$NatHerbCov))
names(data)[names(data)=="asin(data$NatHerbCov)"] <- "ASINNatHerbCov"

data <-cbind(data, sqrt(data$ASINInvHerbCov)) 
names(data)[names(data)=="sqrt(data$ASINInvHerbCov)"] <- "SQRTASINInvHerbCov" #don't need this one, S-W on arcsin was okay

data <-cbind(data, sqrt(data$ASINNatHerbCov))
names(data)[names(data)=="sqrt(data$ASINNatHerbCov)"] <- "SQRTASINNatHerbCov"

data<-cbind(data,sqrt(asin(data$HerbCov)))
names(data)[names(data)=="sqrt(asin(data$HerbCov))"] <- "SQRTASINHerbCov"

#test transformed data for normality


shapiro.test(data$SQRTCountInd)
shapiro.test(data$SQRTShanDiv)
shapiro.test(data$SQRTCountInvInd)
shapiro.test(data$SQRTCountInvIndPerM)
shapiro.test(data$SQRTCountNatInd)
shapiro.test(data$SQRTCountNatIndPerM)
shapiro.test(data$ASINInvHerbCov)
shapiro.test(data$ASINNatHerbCov)

#everything is okay except InvCover; change that to square-root-arcsin transform (replace NaN with 0)

data[is.na(data)] <-0 #to replace NaN with 0


shapiro.test(data$SQRTASINInvHerbCov)
shapiro.test(data$SQRTASINNatHerbCov)

#analyze - working linear models  (note that the SS and MS are correct, but it does not default to using the correct denominator for the F test of a nested model - that is easily fixed by hand, but is an open problem with the code for now)

richnessperm<-lm(CountSpPerM ~ (River/Site) + (Site/Bank), data)
anova(richnessperm)

indivsperm<-lm(CountIndPerM ~ (River/Site) + (Site/Bank), data)
anova(indivsperm)

shandiv<-lm(SQRTShanDiv ~ (River/Site) + (Site/Bank), data)
anova(shandiv)

ctinvperm<-lm(CountInvSpPerM ~ (River/Site) + (Site/Bank), data)
anova(ctinvperm)

ctnatperm<-lm(CountNatSpPerM ~ (River/Site) + (Site/Bank), data)
anova(ctnatperm)

ctinvindperm<-lm(SQRTCountInvIndPerM ~ (River/Site) + (Site/Bank), data)
anova(ctinvindperm)

ctnatindperm<-lm(SQRTCountNatIndPerM ~ (River/Site) + (Site/Bank), data)
anova(ctnatindperm)

invherbcov<-lm(SQRTASINInvHerbCov ~ (River/Site) + (Site/Bank), data)
anova(invherbcov)

natherbcov<-lm(ASINNatHerbCov ~ (River/Site) + (Site/Bank), data)
anova(natherbcov)

herbcov <- lm(SQRTASINHerbCov ~ (River/Site) + (Site/Bank), data)
anova(herbcov)

#attempting to get MANOVA to work - this works fine but it does not include the nested structure of the data

fixed = cbind(data$CountSpPerM, data$CountIndPerM, data$SQRTShanDiv, data$CountInvSpPerM, data$CountNatSpPerM, data$SQRTCountInvIndPerM, data$SQRTCountNatIndPerM, data$SQRTASINInvHerbCov, data$ASINNatHerbCov) ~ data$Bank
fit <- manova(Y ~ data$River) #or data$Site
summary(fit)
summary.aov(fit)

#another approach that seems to be working better.
###also adding a second Site2 column that repeats the 1 & 2 labels for each nested site instead of K1 K2 B1 B2 etc

site2<-c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
data$site2<-cbind(site2)

model <- lm(cbind(CountSpPerM, CountIndPerM, SQRTShanDiv, CountInvSpPerM, CountNatSpPerM, SQRTCountInvIndPerM, SQRTCountNatIndPerM, SQRTASINInvHerbCov, SQRTASINNatHerbCov) ~ River + Site + Bank, data=data)
anova(model, test="Wilks")

#an approach using the Biodiversity R package which might be able to interpret nestedness
require(vegan)
require(BiodiversityR)

factors=cbind(data$CountSpPerM, data$CountIndPerM, data$SQRTShanDiv, data$CountInvSpPerM, data$CountNatSpPerM, data$SQRTCountInvIndPerM, data$SQRTCountNatIndPerM, data$SQRTASINInvHerbCov, data$ASINNatHerbCov)

nested.npmanova(factors~River+Site, data=data, method="euclidean", permutations=1000)

#using MCMCglmm package for a multivariate generalized linear mixed model instead

require(MCMCglmm)

fixed<-cbind(CountSpPerM, CountIndPerM, ShanDiv, CountInvSpPerM, CountNatSpPerM, CountInvIndPerM, CountNatIndPerM, InvHerbCov, NatHerbCov) ~ Bank
family<-("gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian")
random = ~Bank:Site + Site:River

MCMCglmm(fixed, random, family = family, data=data)

#testing MCMCglmm sample code
require(MCMCglmm)
data("BTdata")
data("BTped")

m1<- MCMCglmm(
  fixed=cbind(tarsus, back) ~ trait:sex + trait:hatchdate ~ 1,
  random = ~us(trait):animal + us(trait):fosternest,
  rcov = ~us(trait):units,
  family = c("gaussian", "caussian"), nitt = 60000, burnin = 10000,
  thin = 25, data = BTdata)

# i have yet to find a satisfactory approach that both elegantly handles multiple responses and accomodates the nestedness of the linear model


#plotting results (color for PPT)

require(ggplot2)

#18 128 214; 132 24 199; 255 52 17; 255 185 0; 96 199 23 colors for ppt theme

palette1 <- c("#0868AC", "#62C27A") #define color palette in hex; can then use fill=$var and scale_fill_manual(values=YOURPALETTE) to add colors.
palette2 <- c("#0868AC", "#43A2CA", "#62C27A")
palette3 <-c("#0868AC", "#42ABF6","#62C27A", "#CBEBD3")
palette4 <-c("#0868AC", "#42ABF6","#43A2CA","#B4DAEA" ,"#62C27A", "#CBEBD3")

richnessplot <- qplot(data=data, y=CountSp, x=Bank, geom=("boxplot"), fill=Bank)
richnessplot + xlab("Bank Type") + ylab("Species Richness") + scale_fill_manual(values=bankpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)

#guides(fill=FALSE) removes the legend
#under theme(), text = element_text(size=##) sets point size; axis.title.y=element_text(vjust=##) affects the spacing between the y axis title and the numbers

#plotting results - shared axis for figures Jessica requested formatted as such; requires use of dataframe "databyor" and not "data".  Plots that require this method are: native/introduced sp richness; native/introduced sp per area; native/introduced individuals, native/introduced cover.  both by bank type and by river. 

bwpalette<-c("white", "white")
bwpalette2<-c("white", "white", "white")

#####Number of Individual Plants Per Meter
#######By Bank
ggplot(databyor, aes(x=Bank, y=IndPerM)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("Bank Type") + ylab(expression(paste("Number of Individual Plants Per  ", m^{2}))) + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)

#######By River
ggplot(databyor, aes(x=River, y=IndPerM)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("River") + ylab(expression(paste("Number of Individual Plants Per  ", m^{2}))) + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)

#####Number of Species
#######By Bank

ggplot(databyor, aes(x=Bank, y=SpRich)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("Bank Type") + ylab(expression(paste("Species Richness"))) + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)+ scale_y_continuous(limits=c(0,50))

######By River

ggplot(databyor, aes(x=River, y=SpRich)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("River") + ylab(expression(paste("Species Richness"))) + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)+ scale_y_continuous(limits=c(0,50))


#####Number of Species per Meter
#######By Bank
ggplot(databyor, aes(x=Bank, y=SpPerM)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("Bank Type") + ylab(expression(paste("Number of Species Per  ", m^{2}))) + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)

#######By River
ggplot(databyor, aes(x=River, y=SpPerM)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("River") + ylab(expression(paste("Number of Species Per  ", m^{2}))) + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)


##### Cover
#######By Bank
ggplot(databyor, aes(x=Bank, y=Cover)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("Bank Type") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)+ scale_y_continuous(limits=c(0, 1.25), breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))

#######By River
ggplot(databyor, aes(x=River, y=Cover)) + geom_boxplot(aes(fill=Origin), position=position_dodge(0.8)) + xlab("River") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE) + scale_y_continuous(limits=c(0,1.25), breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))


#plotting results that don't need to be by origin (overall species richness, overall number of individuals, shannon diversity)


#####species richness
########by bank
qplot(data=data, y=CountSp, x=Bank, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("Bank Type") + ylab("Species Richness") + scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50))
########by river
qplot(data=data, y=CountSp, x=River, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("River") + ylab("Species Richness")  + scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50))

####number of individuals per meter
########by bank
qplot(data=data, y=CountIndPerM, x=Bank, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("Bank Type") + ylab(expression(paste("Number of Individual Plants Per  ", m^{2})))+ scale_y_continuous(limits=c(0,60))
########by river
qplot(data=data, y=CountIndPerM, x=River, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("River") + ylab(expression(paste("Number of Individual Plants Per  ", m^{2})))+ scale_y_continuous(limits=c(0,60))

####shannon diversity
########by bank
qplot(data=data, y=ShanDiv, x=Bank, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("Bank Type") + ylab("Shannon Diversity")
########by river
qplot(data=data, y=ShanDiv, x=River, geom=c("boxplot")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("River") + ylab("Shannon Diversity")




#tufte boxplot test

ggplot(data=data, y=ShanDiv, x=River) + geom_tufteboxplot()

q<-ggplot(data, aes(factor(River), ShanDiv))
q+geom_tufteboxplot() + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("River") + ylab("Shannon Diversity")


#older plots (black and white, used in original manuscript draft - the plots above are probably better)

require(ggplot2)

richnessplot <- qplot(data=data, y=CountSp, x=Bank, geom=c("boxplot"))
richnessplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25))  + xlab("Bank Type") + ylab("Species Richness")

qplot (data=data, y=CountSp, x=Bank, geom="boxplot")


indivplot <- qplot(data=data, y=CountInd, x=Bank, geom="boxplot")
indivplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Individual Plants")

shandivplot <- qplot(data=data, y=ShanDiv, x=Bank, geom="boxplot")
shandivplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Shannon Diversity")

invsprichplot <- qplot(data=data, y=CountInvSp, x=Bank, geom="boxplot")
invsprichplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Introduced Species")

natrichplot <- qplot(data=data, y=CountNatSp, x=Bank, geom="boxplot")
natrichplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Native Species")

invctplot <- qplot(data=data, y=CountInvInd, x=Bank, geom="boxplot")
invctplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Introduced Individuals")

natctplot <- qplot(data=data, y=CountNatInd, x=Bank, geom="boxplot")
natctplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Native Individuals")

invcovplot <- qplot(data=data, y=InvCov, x=Bank, geom="boxplot")
invcovplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Percent Cover of Introduced Herbs/Forbs")

natcovplot <- qplot(data=data, y=NatCov, x=Bank, geom="boxplot")
natcovplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Percent Cover of Native Herbs/Forbs")


richnesspermplot <- qplot(data=data, y=CountSpPerM, x=Bank, geom="boxplot")
richnesspermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Species per m^2")

countindpermplot <- qplot(data=data, y=CountIndPerM, x=Bank, geom="boxplot")
countindpermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Individual Plants per m^2")

countinvsppermplot <- qplot(data=data, y=CountInvSpPerM, x=Bank, geom="boxplot")
countinvsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Introduced Species per m^2")

countnatsppermplot <- qplot(data=data, y=CountNatSpPerM, x=Bank, geom="boxplot")
countnatsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Native Species per m^2")

countinvsppermplot <- qplot(data=data, y=CountInvSpPerM, x=Bank, geom="boxplot")
countinvsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Introduced Species per m^2")

countnatindpermplot <- qplot(data=data, y=CountNatIndPerM, x=Bank, geom="boxplot")
countnatsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Native Individuals per m^2")

countinvindpermplot <- qplot(data=data, y=CountInvIndPerM, x=Bank, geom="boxplot")
countinvindpermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("Bank Type") + ylab("Number of Introduced Individuals per m^2")

#plotting results among rivers formatted for publication (b&w)

require(ggplot2)

richnessplot <- qplot(data=data, y=CountSp, x=River, geom="boxplot")
richnessplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Species Richness")

indivplot <- qplot(data=data, y=CountInd, x=River, geom="boxplot")
indivplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Individual Plants")

shandivplot <- qplot(data=data, y=ShanDiv, x=River, geom="boxplot")
shandivplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Shannon Diversity")

invsprichplot <- qplot(data=data, y=CountInvSp, x=River, geom="boxplot")
invsprichplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Introduced Species")

natrichplot <- qplot(data=data, y=CountNatSp, x=River, geom="boxplot")
natrichplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Native Species")

invctplot <- qplot(data=data, y=CountInvInd, x=River, geom="boxplot")
invctplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Introduced Individuals")

natctplot <- qplot(data=data, y=CountNatInd, x=River, geom="boxplot")
natctplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Native Individuals")

invcovplot <- qplot(data=data, y=InvHerbCov, x=River, geom="boxplot")
invcovplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Percent Cover of Introduced Herbs/Forbs")

natcovplot <- qplot(data=data, y=NatHerbCov, x=River, geom="boxplot")
natcovplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Percent Cover of Native Herbs/Forbs")


richnesspermplot <- qplot(data=data, y=CountSpPerM, x=River, geom="boxplot")
richnesspermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Species per m^2")

countindpermplot <- qplot(data=data, y=CountIndPerM, x=River, geom="boxplot")
countindpermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Individual Plants per m^2")

countinvsppermplot <- qplot(data=data, y=CountInvSpPerM, x=River, geom="boxplot")
countinvsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Introduced Species per m^2")

countnatsppermplot <- qplot(data=data, y=CountNatSpPerM, x=River, geom="boxplot")
countnatsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Native Species per m^2")

countinvsppermplot <- qplot(data=data, y=CountInvSpPerM, x=River, geom="boxplot")
countinvsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Introduced Species per m^2")

countnatindpermplot <- qplot(data=data, y=CountNatIndPerM, x=River, geom="boxplot")
countnatsppermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Native Individuals per m^2")

countinvindpermplot <- qplot(data=data, y=CountInvIndPerM, x=River, geom="boxplot")
countinvindpermplot + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA))  + xlab("River") + ylab("Number of Introduced Individuals per m^2")

#plotting two figures next to each other
par(mfrow = c(1,2))
#then add the two plots







##### Cover
#######By Bank
require(ggplot2)
ggplot(data, aes(x=Bank, y=HerbCov)) + geom_boxplot(position=position_dodge(0.8)) + xlab("Bank Type") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) + guides(fill=FALSE) + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))
#######By River
ggplot(data, aes(x=River, y=HerbCov)) + geom_boxplot(position=position_dodge(0.8)) + xlab("River") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)+ scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))

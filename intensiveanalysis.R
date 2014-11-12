#import datasheet as "data"

require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
raw <- getURL("https://raw.githubusercontent.com/erollinson/Dissertation/master/2012%20Data%20Summary%20with%20Averages%203_3_14%20for%20R.csv") #insert the  raw URL for the data file on github here
data <- read.csv(text = raw) #read in the github file

#importing the second data sheet with species origin as a condition (for figures to share axes)

require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
raw <- getURL("https://raw.githubusercontent.com/erollinson/Dissertation/master/2012%20Data%20Summary%20with%20Averages%203_3_14%20for%20R%20by%20origin%20for%20figure%20merge.csv") #insert the  raw URL for the data file on github here
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

#to test for homogeneity of variances (P>0.05 = homogeneous)
bartlett.test(data$CountSp, data$River)
bartlett.test(data$CountSpPerM, data$River)
bartlett.test(data$CountInd, data$River)
bartlett.test(data$CountIndPerM, data$River)
bartlett.test(data$ShanDiv, data$River)
bartlett.test(data$CountInvSp, data$River)
bartlett.test(data$CountInvSpPerM, data$River)
bartlett.test(data$CountNatSp, data$River)
bartlett.test(data$CountNatSpPerM, data$River)
bartlett.test(data$CountInvInd, data$River)
bartlett.test(data$CountInvIndPerM, data$River)
bartlett.test(data$CountNatInd, data$River)
bartlett.test(data$CountNatIndPerM, data$River)  ### non-homogeneous
bartlett.test(data$InvHerbCov, data$River)  ### non-homogeneous
bartlett.test(data$NatHerbCov, data$River)

bartlett.test(data$CountSp, data$Site)
bartlett.test(data$CountSpPerM, data$Site)
bartlett.test(data$CountInd, data$Site) ### non-homogeneous
bartlett.test(data$CountIndPerM, data$Site)
bartlett.test(data$ShanDiv, data$Site)
bartlett.test(data$CountInvSp, data$Site)
bartlett.test(data$CountInvSpPerM, data$Site)
bartlett.test(data$CountNatSp, data$Site)
bartlett.test(data$CountNatSpPerM, data$Site)
bartlett.test(data$CountInvInd, data$Site)
bartlett.test(data$CountInvIndPerM, data$Site)
bartlett.test(data$CountNatInd, data$Site)
bartlett.test(data$CountNatIndPerM, data$Site) ### non-homogeneous
bartlett.test(data$InvHerbCov, data$Site)
bartlett.test(data$NatHerbCov, data$Site)

bartlett.test(data$CountSp, data$Bank)
bartlett.test(data$CountSpPerM, data$Bank)
bartlett.test(data$CountInd, data$Bank)
bartlett.test(data$CountIndPerM, data$Bank)
bartlett.test(data$ShanDiv, data$Bank)
bartlett.test(data$CountInvSp, data$Bank)
bartlett.test(data$CountInvSpPerM, data$Bank)
bartlett.test(data$CountNatSp, data$Bank)
bartlett.test(data$CountNatSpPerM, data$Bank)
bartlett.test(data$CountInvInd, data$Bank)
bartlett.test(data$CountInvIndPerM, data$Bank)
bartlett.test(data$CountNatInd, data$Bank)
bartlett.test(data$CountNatIndPerM, data$Bank) ### non-homogeneous
bartlett.test(data$InvHerbCov, data$Bank)
bartlett.test(data$NatHerbCov, data$Bank)


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

#analyze - working linear models  (note that the SS and MS are correct, but it does not default to using the correct denominator for the F test of a nested model - that is easily fixed by hand, but is an open problem with the code for now) These are the models that are used in the original draft of the manuscript

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


#Post-transformation for those marked as non-homogeneous - all are OK now
bartlett.test(data$SQRTCountNatIndPerM, data$River)
bartlett.test(data$SQRTASINInvHerbCov, data$River)  
bartlett.test(data$SQRTCountInd, data$Site) ### 
bartlett.test(data$SQRTCountNatIndPerM, data$Site) 
bartlett.test(data$SQRTCountNatIndPerM, data$Bank) 


#----------------------------------------------------------------
#SOME MANOVA APPROACHES - GENERALLY NOT HANDLING NESTING WELL

#another approach that seems to be working better.
###also adding a second Site2 column that repeats the 1 & 2 labels for each nested site instead of K1 K2 B1 B2 etc

site2<-c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
data$site2<-cbind(site2)

model <- lm(cbind(CountSpPerM, CountIndPerM, SQRTShanDiv, CountInvSpPerM, CountNatSpPerM, SQRTCountInvIndPerM, SQRTCountNatIndPerM, SQRTASINInvHerbCov, SQRTASINNatHerbCov, SQRTASINHerbCov) ~ (River/site2) + (site2/Bank), data=data)
anova(model, test="Pillai")
anova(model, test="Wilks")
anova(model, test="Hotelling")
anova(model, test="Roy")


#run it again without the origin partitions

site2<-c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
data$site2<-cbind(site2)

model_2 <- lm(cbind(CountSpPerM, CountIndPerM, SQRTShanDiv, SQRTASINHerbCov) ~ (River/Site) + (Site/Bank) , data=data)
anova(model_2, test="Pillai")
anova(model_2, test="Wilks")
anova(model_2, test="Hotelling")
anova(model_2, test="Roy")


#Secondary MANOVA that uses the "databyor" sheet for comparing richness, counts and cover for invasive species

databyor<-cbind(databyor, sqrt(databyor$IndPerM))
names(databyor)[names(databyor)=="sqrt(databyor$IndPerM)"] <- "SQRTIndPerM"

databyor<-cbind(databyor, sqrt(databyor$SpPerM))
names(databyor)[names(databyor)=="sqrt(databyor$SpPerM)"] <- "SQRTSpPerM"

databyor<-cbind(databyor, sqrt(asin(databyor$Cover)))
names(databyor)[names(databyor)=="sqrt(asin(databyor$Cover))"] <- "SQRTASINCover"

databyor[is.na(databyor)] <-0 #to replace NaN with 0

model_origin<-lm(cbind(SQRTIndPerM, SQRTSpPerM, SQRTASINCover) ~ Origin + Origin*Bank + Origin*River, data=databyor)
anova(model_origin, test="Pillai")
anova(model_origin, test="Wilks")
anova(model_origin, test="Hotelling")
anova(model_origin, test="Roy")


summary(model_origin)

#anova to get post-hoc p value for interaction between origin and river, substitute in whichever response variable is of interest where it currently says "SQRTASINCover"
anova(lm(SQRTASINCover ~ Origin*River, data=databyor))


#-----------------------------------------------------------------------------------------------------------------------------------
#A NUMBER OF ANOVA/MANOVA MODELS THAT EXPLICITY DO NOT ADDRESS MY DATA CORRECTLY, BUT I USED TO TEST THAT OUTPUT WORKED AS I WAS EXPECTING

#what if i nest site but not bank - because i think bank matters...
#So I think I should be treating this as a nested factorial design where sites are nested but banks are a "treatment" - each bank type exists in each site and within each 

site2<-c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
data$site2<-cbind(site2)

model_3 <- lm(cbind(CountSpPerM, CountIndPerM, SQRTShanDiv, SQRTASINHerbCov) ~ (1|River) + (River/Site) + Bank, data=data)
anova(model_3, test="Pillai")
anova(model_3, test="Wilks")
anova(model_3, test="Hotelling")
anova(model_3, test="Roy")

summary(model_3)

#a model with just bank to make me feel better

model_4 <- lm(cbind(CountSpPerM, CountIndPerM, SQRTShanDiv, SQRTASINHerbCov) ~ Bank, data=data)
anova(model_4, test="Pillai")
anova(model_4, test="Wilks")
anova(model_4, test="Hotelling")
anova(model_4, test="Roy")

summary(model_4)

#just one variable?

model_5 <-lm(CountSpPerM ~ Bank, data=data)
anova(model_5)

#--------------------------------------------------------------------------------------------------------
#MISCELLANEOUS ALTERNATE APPROACHES TO THIS ANALYSIS USING VARIOUS LIBRARIES...none of which seem to help
# i have yet to find a satisfactory approach that both elegantly handles multiple responses and accomodates the nestedness of the linear model
#this test code is commented out for the time being

#An approach using the Biodiversity R package which might be able to interpret nestedness - it appears to only be able to interpret one level of nestedness, and I have two.
##require(vegan)
##require(BiodiversityR)

##factors=cbind(data$CountSpPerM, data$CountIndPerM, data$SQRTShanDiv, data$CountInvSpPerM, data$CountNatSpPerM, data$SQRTCountInvIndPerM, data$SQRTCountNatIndPerM, data$SQRTASINInvHerbCov, data$ASINNatHerbCov)

##nested.npmanova(factors~(River+Site), data=data, method="euclidean", permutations=1000)


#--------------------
#using MCMCglmm package for a multivariate generalized linear mixed model instead

##require(MCMCglmm)

##fixed<-cbind(CountSpPerM, CountIndPerM, ShanDiv, CountInvSpPerM, CountNatSpPerM, CountInvIndPerM, CountNatIndPerM, InvHerbCov, NatHerbCov) ~ Bank
##family<-("gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian","gaussian")
##random = ~Bank:Site + Site:River

##MCMCglmm(fixed, random, family = family, data=data)

#--------------------
#testing MCMCglmm sample code
##require(MCMCglmm)
##data("BTdata")
##data("BTped")

##m1<- MCMCglmm(
  ##fixed=cbind(tarsus, back) ~ trait:sex + trait:hatchdate ~ 1,
  ##random = ~us(trait):animal + us(trait):fosternest,
  ##rcov = ~us(trait):units,
  ##family = c("gaussian", "caussian"), nitt = 60000, burnin = 10000,
  ##thin = 25, data = BTdata)





#-------------------------------------------------------------------------------------
#EVERYTHING BELOW THIS POINT IS CODE FOR PLOTTING VARIOUS FIGURES - NO FURTHER ANALYSIS


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
qplot(data=data, y=CountSp, x=Bank, geom=c("point")) + theme_bw() + theme (panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=25), axis.title.y=element_text(vjust=0.2))  + xlab("Bank Type") + ylab("Species Richness") + scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50))
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


#testingboxplot formats for mary!

testplot<-ggplot(data, aes(x=River, y=CountSp)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "text", label="-----------", size= 10, color= "red")
testplot
testplot2<-ggplot(data, aes(x=River, y=CountSp)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "text", label="____________", size= 10, color= "red", vjust=-0.16)
testplot2

#plotting two figures next to each other
par(mfrow = c(1,2))
#then add the two plots




##### Cover
#######By Bank
require(ggplot2)
ggplot(data, aes(x=Bank, y=HerbCov)) + geom_boxplot(position=position_dodge(0.8)) + xlab("Bank Type") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) + guides(fill=FALSE) + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))
#######By River
ggplot(data, aes(x=River, y=HerbCov)) + geom_boxplot(position=position_dodge(0.8)) + xlab("River") + ylab("Proportion of Cover") + scale_fill_manual(values=bwpalette2) + theme_bw() + theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), text = element_text(size=20), axis.title.y=element_text(vjust=0.2)) +guides(fill=FALSE)+ scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1,1.25), labels=c("0", "25", "50", "75", "100", "125"))

#------------------------------------------------------------------------------------------------------------------------------------


### August making some new figures for ESA PPT


#code to generate SE for the dot and SE figures - I will need: number of species, number of individual plants, ShanDiv, % Cover
#Repeat again for the invasive subset and native subset of those data

#use databyor for the split one and group by origin, river
#use data for the pooled one and group by bank

require(bear)

##by river
rivrich<-summarySE(data, measurevar="CountSp", groupvars=c("River"))
rivindm<-summarySE(data, measurevar="CountIndPerM", groupvars=c("River"))
rivshan<-summarySE(data, measurevar="ShanDiv", groupvars=c("River"))
rivcov<-summarySE(data, measurevar="HerbCov", groupvars=c("River"))

#figures (by river)

pd <- position_dodge(.1)  #call on this to shift points so as not to overlap when there are multiple group vars

#rivrich
ggplot(rivrich, aes(x=River, y=CountSp)) + 
  geom_errorbar(aes(ymin=CountSp-ci, ymax=CountSp+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Stream") +
  ylab("Species Richness") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(0,46), breaks=c(0,5,10,15,20,25,30,35,40,45), labels=c("0", "5","10","15","20","25", "30","35","40","45"))

#rivindm

ggplot(rivindm, aes(x=River, y=CountIndPerM)) + 
  geom_errorbar(aes(ymin=CountIndPerM-ci, ymax=CountIndPerM+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Stream") +
  ylab("Number of Individual Plants per m2") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50), labels=c("0", "10", "20", "30", "40", "50"))

#rivshan
ggplot(rivshan, aes(x=River, y=ShanDiv)) + 
  geom_errorbar(aes(ymin=ShanDiv-ci, ymax=ShanDiv+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Stream") +
  ylab("Shannon Diversity") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
scale_y_continuous(limits=c(0,4), breaks=c(0,1,2,3,4), labels=c("0", "1", "2", "3", "4"))

#rivcov

ggplot(rivcov, aes(x=River, y=HerbCov)) + 
  geom_errorbar(aes(ymin=HerbCov-ci, ymax=HerbCov+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Stream") +
  ylab("Herbaceous Cover (%)") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
scale_y_continuous(limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c("0", "20", "40", "60", "80", "100"))


#by bank

brich<-summarySE(data, measurevar="CountSp", groupvars=c("Bank"))
bind<-summarySE(data, measurevar="CountIndPerM", groupvars=c("Bank"))
bshan<-summarySE(data, measurevar="ShanDiv", groupvars=c("Bank"))
bcov<-summarySE(data, measurevar="HerbCov", groupvars=c("Bank"))

#brich
ggplot(brich, aes(x=Bank, y=CountSp)) + 
  geom_errorbar(aes(ymin=CountSp-ci, ymax=CountSp+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Bank") +
  ylab("Species Richness") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) + 
  scale_y_continuous(limits=c(0,41), breaks=c(0,10,20,30,40), labels=c("0", "10", "20", "30", "40"))

#bind
ggplot(bind, aes(x=Bank, y=CountIndPerM)) + 
  geom_errorbar(aes(ymin=CountIndPerM-ci, ymax=CountIndPerM+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Bank") +
  ylab("Number of Individuals per m2") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
  scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40,50), labels=c("0", "10", "20", "30", "40", "50"))

#bshan
ggplot(bshan, aes(x=Bank, y=ShanDiv)) + 
  geom_errorbar(aes(ymin=ShanDiv-ci, ymax=ShanDiv+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Bank") +
  ylab("Shannon Diversity") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
  scale_y_continuous(limits=c(0,3.2), breaks=c(0,1,2,3), labels=c("0", "1", "2", "3"))

#bcov
ggplot(bcov, aes(x=Bank, y=HerbCov)) + 
  geom_errorbar(aes(ymin=HerbCov-ci, ymax=HerbCov+ci), colour="black", width=.1) +
  geom_line() +
  geom_point(size=5, shape=21, fill="black") +
  xlab("Bank") +
  ylab("Herbaceous Cover (%)") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
  scale_y_continuous(limits=c(0,0.7), breaks=c(0,.1,.2,.3, .4,.5,.6,.7), labels=c("0", "10", "20", "30", "40", "50", "60", "70"))


#by river, origin split

orrivrich<-summarySE(databyor, measurevar="SpRich", groupvars=c("River", "Origin"))
orrivind<-summarySE(databyor, measurevar="IndPerM", groupvars=c("River", "Origin"))
orrivcov<-summarySE(databyor, measurevar="Cover", groupvars=c("River", "Origin"))

#figures

pd <- position_dodge(.1)  #call on this to shift points so as not to overlap when there are multiple group vars.  the dodge function produces a bug message when you plot the graph which reads "ymax not defined: adjusting position using y instead" - this is irrelevant and appears to be a bug; the figure is drawn correctly.

#orrivrich
ggplot(orrivrich, aes(x=River, y=SpRich, group=Origin)) + 
  geom_errorbar(aes(ymin=SpRich-ci, ymax=SpRich+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Stream") +
  ylab("Species Richness") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
  scale_y_continuous(limits=c(0,40), breaks=c(0,10,20,30, 40), labels=c("0", "10", "20", "30","40"))

#orrivind
ggplot(orrivind, aes(x=River, y=IndPerM, group=Origin)) + 
  geom_errorbar(aes(ymin=IndPerM-ci, ymax=IndPerM+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Stream") +
  ylab("Number of Individual Plants per m2") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(-5,40), breaks=c(0,10,20,30,40), labels=c("0", "10", "20", "30", "40"))

#orrivcov

ggplot(orrivcov, aes(x=River, y=Cover, group=Origin)) + 
  geom_errorbar(aes(ymin=Cover-ci, ymax=Cover+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Stream") +
  ylab("Herbaceous Cover (%)") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(-0.03,0.7), breaks=c(0,.1,.2,.3, .4, .5, .6, .7), labels=c("0", "10", "20", "30", "40", "50", "60", "70"))

#by bank, origin split

orbrich<-summarySE(databyor, measurevar="SpRich", groupvars=c("Bank", "Origin"))
orbind<-summarySE(databyor, measurevar="IndPerM", groupvars=c("Bank", "Origin"))
orbcov<-summarySE(databyor, measurevar="Cover", groupvars=c("Bank", "Origin"))


#orbrich
ggplot(orbrich, aes(x=Bank, y=SpRich, group=Origin)) + 
  geom_errorbar(aes(ymin=SpRich-ci, ymax=SpRich+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Bank") +
  ylab("Species Richness") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(-1,33), breaks=c(0,10,20,30), labels=c("0", "10", "20", "30"))

#orbind
ggplot(orbind, aes(x=Bank, y=IndPerM, group=Origin)) + 
  geom_errorbar(aes(ymin=IndPerM-ci, ymax=IndPerM+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Bank") +
  ylab("Number of Individual Plants per m2") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA)) +
  scale_y_continuous(limits=c(0,40), breaks=c(0,10,20,30,40), labels=c("0", "10", "20", "30", "40"))

#orbcov

ggplot(orbcov, aes(x=Bank, y=Cover, group=Origin)) + 
  geom_errorbar(aes(ymin=Cover-ci, ymax=Cover+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(size=5, shape=21, fill="black", position=pd) +
  xlab("Bank") +
  ylab("Herbaceous Cover (%)") +
  theme_bw() + 
  theme(panel.grid.major=element_line(color = NA), panel.grid.minor=element_line(color = NA), 
        legend.justification=c(1,0), legend.position=c(1,0)) +
  scale_y_continuous(limits=c(0,.6), breaks=c(0,.1,.2,.3,.4,.5,.6), labels=c("0", "10", "20", "30", "40", "50", "60"))


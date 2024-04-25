#set the working directory
setwd("C:/Users/ellio/OneDrive - Michigan State University/MSU files/CSS844")
#read in the data
phenos<-read.csv("feke_compiled_periods_class.csv")
#set the character columns to factors to use as variables
phenos$Accession<-as.factor(phenos$Accession)
phenos$Species<-as.factor(phenos$Species)
phenos$Measurement_Date<-as.factor(phenos$Measurement_Date)
#look at the distribution of the errors and period
summary(phenos$Error)
summary(phenos$Period)
#filter by period - greater than 12 and less than 36
cleanphenos<-phenos[which(phenos$Period>=12 & phenos$Period<=36),]
summary(cleanphenos$Period)
#filter the error - <.25
cleanphenos<-cleanphenos[which(cleanphenos$Error<=0.25),]
summary(cleanphenos$Error)
#check distribution of period
hist(cleanphenos$Period)
#how do species differ in their photoperiod?
boxplot(cleanphenos$Period~cleanphenos$Species,las=2)
#let's attempt Tukey's HSD to get letter codes
#set up linear model
mod1<-lm(cleanphenos$Period~cleanphenos$Species)
amod1<-aov(mod1)
TukeyHSD(amod1)
plot(TukeyHSD(amod1))
#suggestion: use agricolae package or similar to create
#letter codes of significant differences on boxplot
#are there significant differences between accessions?
anova(lm(cleanphenos$Period~cleanphenos$Accession))
#is the Period data normally distributed?
#shapiro-wilk test - on residuals
mod1<-lm(cleanphenos$Period~cleanphenos$Accession)
shapiro.test(mod1$residuals)
#not normal!
#should try Kolmogorov-Smirnov test for larger sample
ks.test(mod1$residuals,"pnorm")
#not normal!
#we need to account for other factors
anova(lm(cleanphenos$Period~cleanphenos$Accession+cleanphenos$Measurement_Date))
#look at data
plot(cleanphenos$Period,pch=19,cex=.5,col=cleanphenos$Measurement_Date)
plot(cleanphenos$Period,pch=19,cex=.5,col=cleanphenos$Species)
#Measurement date and species are somewhat confounded
#Maybe one option is to use EDN as a "control" species measured in multiple
#(ideally all) dates to calculate batch effect
#then check whether residuals for fitted accession values are
#normally distributed

#goal: one value for Period per Accession to be used in genetic mapping

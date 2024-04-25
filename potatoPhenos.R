setwd("C:/Users/ellio/OneDrive - Michigan State University/MSU files/CSS844")
getwd()

phenos <- read.csv("feke_compiled_periods_class.csv")
# set the character columns to factors to use as variables
phenos$Accession <- as.factor(phenos$Accession)
phenos$Species <- as.factor(phenos$Species)
phenos$Measurement_Date <- as.factor(phenos$Measurement_Date)
summary(phenos$Error)

#filter by period, greater than 12 and less than 36
cleanphenos <- phenos[which(phenos$Period>=12 & phenos$Period<=36),]
summary(cleanphenos$Period)
#filter by error
cleanphenos <- phenos[which(cleanphenos$Error<=.5),]
#check distribution of Period
hist(cleanphenos$Period, )
#how do species (accession) differ in their photoperiod
boxplot(cleanphenos$Period~cleanphenos$Species, las=2)
#let's try tukey's honest sig. difference test to get letter codes
mod1 <- cleanphenos$Period~cleanphenos$Species
amod1 <- aov(mod1)
TukeyHSD <- (amod1)
#suggestion: use agricolae package or similar
anova(lm(cleanphenos$Period~cleanphenos$Accession))
#Pr(>F) very small so significant evidence to suggest genetic determination
#is the period column distributed normally?
mod1 <- lm(cleanphenos$Period~cleanphenos$Accession)
shapiro.test(mod1$residuals)
#p-value small so data is not distributed normally
#should check normality of data for each accession since it's what
#we'll use for mapping
#we should try Komolgorov-smirnov test b/c larger dataset
ks.test(mod1$residuals, pnorm)
#not normal with this test as well

#we need to account for other factors
anova(lm(cleanphenos$Period~cleanphenos$Accession+cleanphenos$Measurement_Date))

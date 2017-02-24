raw.data <- read.csv("/media/removable/USB Drive/S670/Project 1/pnas.1402786111.sd01 - Archival Study.csv", header = TRUE)[1:92,]

library(ggplot2)

raw.data$Gender_MF <- as.factor(raw.data$Gender_MF)

ggplot(raw.data, aes(x = Gender_MF, y = NDAM)) + geom_boxplot()

NDAM.log <- log10(raw.data$NDAM)

ggplot(raw.data, aes(x = Gender_MF, y = NDAM.log)) + geom_boxplot()
# log transformed boxplot looks much nicer. Maximum values for male/female the same, medians different, but not too different, largest difference seems to be that the first quartile of male NDAM values reaches far lower than female storms.

ggplot(raw.data, aes(x =as.factor(Category), y = NDAM.log)) + geom_boxplot() + facet_wrap(~Gender_MF, ncol = 2)
# when split by gender and category, it is clear that it is hard to make a meaningful comparison between the two genders, as there is a lot of overlap between the ranges of NDAM values, regardless of category.

ggplot(raw.data, aes(x = Gender_MF, y = Category)) + geom_boxplot()

ggplot(raw.data, aes(x=MasFem, y = NDAM.log))+geom_point()

post.1979.data <- raw.data[39:92,]

ggplot(raw.data[39:92,], aes(x = Gender_MF[39:92], y = NDAM.log[39:92])) + geom_boxplot()
ggplot(post.1979.data, aes(x = as.factor(post.1979.data$Gender_MF), y = log10(post.1979.data$NDAM))) + geom_boxplot()
ggplot(post.1979.data, aes(x = as.factor(Gender_MF), y = NDAM)) + geom_boxplot()


# boxplot of log of NDAM data vs gender for hurricanes after 1979.

ggplot(raw.data, aes(sample = log10(NDAM))) + stat_qq() + facet_wrap(~Gender_MF)
# log10 transformed qq plot shows more normal data
ggplot(raw.data, aes(sample = NDAM)) + stat_qq() + facet_wrap(~Gender_MF)
# untransformed qq plot looks terrible!






hdata= read.csv("pnas.1402786111.sd01 - Archival Study.csv")
head(hdata)

myvar<-c("Year","MasFem","MinPressure_before","Minpressure_Updated.2014","Gender_MF","Category","alldeaths","NDAM","ZMinPressure_A")

hudata=hdata[myvar]
head(hudata)

P1=ggplot(hudata,aes(y=log10(NDAM),x=as.factor(Gender_MF)))+geom_boxplot()
P1=P1+stat_summary(fun.y=mean, colour="firebrick4", geom="point")

P1+facet_grid(~Category)+ggtitle("Category wise NDAM")




cor(hudata$MasFem,hudata$alldeaths)
cor(hudata$NDAM,hudata$alldeaths)
pairs(hudata)
table(hudata$Year,hudata$NDAM)

nn=data.frame(Year=hudata$Year,Gender=hudata$Gender_MF,NDAM=hudata$NDAM)
head(nn)

P2<-ggplot(nn,aes(x=Year))+geom_histogram(binwidth=1) + facet_grid(~Gender)

P2+scale_x_continuous(breaks = pretty(nn$Year, n = 15))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Year and gender wise occurrance")


library(ggplot2)
library(gridExtra)
library(tidyr)
library(lattice)
library(dplyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaps)
#1. Database preparation

#1.1 Selection of variables
variables_chosen <- c("communityname:","state:","population:","PopDens:","householdsize:","PctFam2Par:","racepctblack:","racePctHisp:","agePct65up:","perCapInc:","agePct16t24:","pctWSocSec:","pctWInvInc:","PctPopUnderPov:","PctNotHSGrad:","PctUnemployed:","ViolentCrimesPerPop:","nonViolPerPop:")
concat_dropped <- concat_test[variables_chosen]

#1.2 Cleaning the database

is.data.frame(concat_dropped)#test dataframe ok
i=c(3:18) #Select variables to turn into numeric
concat_dropped[ , i] <- apply(concat_dropped[ , i], 2,
                    function(x) as.numeric(as.character(x)))#convert relevant variables in numeric
sapply(concat_dropped, class) #check ok
colnames(concat_dropped)<-gsub(":","",colnames(concat_wo_na))
concat_dropped$crimes=concat_dropped$ViolentCrimesPerPop + concat_dropped$nonViolPerPop #create dependent variable
concat_wo_na= na.omit(concat_dropped)#Drop NA

#1.3 Automatic selection of relevant variables using information criteria
droppers <- c("communityname", "state", "nonViolPerPop","ViolentCrimesPerPop")
subs_sample <- select(concat_wo_na,-all_of(droppers))
subs <- regsubsets(crimes~., data = subs_sample, nvmax = 9, method = "forward")

summary(subs)
subs.sum <- summary(subs)
data.frame(
  Adj.R2 = which.max(subs.sum$adjr2),
  CP = which.min(subs.sum$cp),
  BIC = which.min(subs.sum$bic)
)

#We compare the information criteria when we increase the number of variables that
#We want to keep, the BIC is maximized with 9 variables that we select

final_variables= c("communityname","state","PopDens","householdsize","PctFam2Par","racepctblack","racePctHisp","agePct16t24","PctPopUnderPov","PctNotHSGrad","PctUnemployed","crimes")
dataset=concat_wo_na[final_variables]
is.data.frame(dataset)#test dataframe ok


#2. Linear regression

lm_crimes = lm(nonViolPerPop~ population+PopDens+ PctFam2Par + racepctblack + racePctHisp + agePct65up+ agePct16t24 +perCapInc+ pctWInvInc + PctPopUnderPov + PctBSorMore+ PctUnemployed,data = concat_wo_na)
summary(lm_crimes)

 






























#1.2.2 outliers?

summary(concat_wo_na)#might be some problems on population and popDens

#Boxplots

pct_var <- c("PctFam2Par:","racepctblack:","racePctHisp:","agePct16t24:","pctWSocSec:","pctWInvInc:","PctPopUnderPov:","PctNotHSGrad:","PctEmploy:")
boxplot(concat_wo_na[pct_var])#seems ok


#Histograms for variables
hist(concat_wo_na$`population:`,breaks=c(200))
hist(concat_dropped$`population:`,breaks=c(2000))
concat_wo_na=concat_wo_na[order(concat_wo_na$population, decreasing = TRUE), ]

cor(concat_wo_na[,c(3:17)])

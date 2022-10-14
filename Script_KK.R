library(readxl)
library(dplyr)
library(ggplot2)
library(leaps)
library(MASS)
library(corrplot)
library(vtable)
library(outliers)
library(texreg)
library(estimatr)

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

#2. Descriptive statistics

#Summary statistics

st(dataset,digits = 2,out="latex")

#we checked the minimum and maximum 

#Correlation matrix
correlation=cor(dataset[,c(3:12)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#3. Parametric section - regression

lm_crimes = lm(crimes~ PopDens+householdsize+PctFam2Par+racepctblack+racePctHisp+agePct16t24+PctPopUnderPov+PctNotHSGrad+PctUnemployed,data = dataset)
rlm_crimes = lm_robust(crimes~ PopDens+householdsize+PctFam2Par+racepctblack+racePctHisp+agePct16t24+PctPopUnderPov+PctNotHSGrad+PctUnemployed,data = dataset, se_type="stata")
summary(lm_crimes)
summary(rlm_crimes)

texreg(rlm_crimes)#Latex format
texreg(lm_crimes)

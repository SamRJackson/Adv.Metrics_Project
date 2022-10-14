# Author : Sam JACKSON, Mathis PRETI and KLA KOUADIO
# Date : 14/10/2022
# Subject : M2-ETE Advanced Econometrics - Project Code File

## 1 . LOAD BASELINE PACKAGES
library(stargazer)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaps)
library(mgcv)
library(texreg)
library(MASS)
library(sandwich)
library(randomForest)
library(gbm)
library(glmnet)
library(estimatr)

## 2. LOAD THE DATASET

load("~/Desktop/M2 2022-2023/Advanced Econometrics/Adv.Metrics_Project-main/Data/CrimeData.RData")

## 3. DATA CLEANING AND VARIABLE SELECTION

# 3.1 SELECT WIDE SET OF VARIABLES
variables_chosen <- c("communityname","state","population","PopDens","householdsize","PctFam2Par","racepctblack","racePctHisp","agePct16t24","perCapInc","pctWSocSec","pctWInvInc","PctPopUnderPov","PctNotHSGrad","PctUnemployed","ViolentCrimesPerPop","nonViolPerPop")
colnames(concat_test)<-gsub(":","",colnames(concat_test))# remove semicolons from names
concat_dropped <- concat_test[variables_chosen]


# 3.2 DROP MISSING OBSERVARTIONS
is.data.frame(concat_dropped) #check we have a dataframe
i=c(3:17) #Select variables to turn into numeric
concat_dropped[ , i] <- apply(concat_dropped[ , i], 2,
                              function(x) as.numeric(as.character(x)))#convert relevant variables in numeric
sapply(concat_dropped, class) #check we have succeeded
concat_dropped$crimes=concat_dropped$ViolentCrimesPerPop + concat_dropped$nonViolPerPop #create dependent variable
concat_wo_na= na.omit(concat_dropped)#Drop NA statistics on drops


# 3.3 USE SUBSET SELECTION TO CHOOSE COVARIATES

drops <- c("communityname", "state", "nonViolPerPop","ViolentCrimesPerPop")
subselect_sample <- dplyr::select(concat_wo_na,-all_of(drops))
subselect_res <- regsubsets(crimes~., data = subselect_sample, nvmax = 9, method = "forward")

summary(subselect_res)
subselect_sums <- summary(subselect_res)
data.frame(
  Adj.R2 = which.max(subselect_sums$adjr2),
  CP = which.min(subselect_sums$cp),
  BIC = which.min(subselect_sums$bic)
)

final_variables= c("communityname","state","PopDens","householdsize","PctFam2Par","racepctblack","racePctHisp","agePct16t24","PctPopUnderPov","PctNotHSGrad","PctUnemployed","crimes")
dataset=concat_wo_na[final_variables]
is.data.frame(dataset) # check we have a dataframe

# 3.4 SUMMARY STATTSTICS ON OUR DATASET

st(dataset,digits = 2,out="latex")

# 3.5 CORRELATION MATRIX
correlation=cor(dataset[,c(3:12)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
## 4. NAIVE LINEAR REGRESSION MODEL

rlm_crimes = lm_robust(crimes~ PopDens+householdsize+PctFam2Par+racepctblack+racePctHisp+agePct16t24+PctPopUnderPov+PctNotHSGrad+PctUnemployed,data = dataset, se_type="stata")
summary(rlm_crimes)
texreg(rlm_crimes)#Latex format



## 5. THE GENERAL ADDITIVE MODEL

# 5.1 FIRST ESTIMATION
attach(dataset)
Gam <- gam((crimes~s(PopDens)+s(householdsize)+s(PctFam2Par)+s(racepctblack)+s(racePctHisp)+s(agePct16t24)+s(PctPopUnderPov)+s(PctNotHSGrad)+s(PctUnemployed))) 
#gamtabs(Gam) 

summary(Gam)
par(mfrow=c(3,3)) #TO have the 3 graphs side by side
plot(Gam, select=1, shade=T, shade.col="lightblue", rug=T, ylim=c(-5000,1000))
plot(Gam, select=2, shade=T, shade.col="lightblue", rug=T, ylim=c(-1000,7000))
plot(Gam, select=3, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,15000))
plot(Gam, select=4, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam, select=5, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,8000))
plot(Gam, select=6, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam, select=7, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000) )
plot(Gam, select=8, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam, select=9, shade=T, shade.col="lightblue", rug=T, ylim=c(-5000,500))

# 5.2 SECOND ESTIMATION

Gam2 <- gam((crimes~s(PopDens,sp=0.01)+s(householdsize)+s(PctFam2Par)+s(racepctblack)+s(racePctHisp)+s(agePct16t24)+s(PctPopUnderPov)+s(PctNotHSGrad)+s(PctUnemployed, sp=0.05)),method="REML")
#gamtabs(Gam2) 

summary(Gam2)
plot(Gam2, select=1, shade=T, shade.col="lightblue", xlim=c(0,20000), ylim=c(-5000,1000))
plot(Gam2, select=2, shade=T, shade.col="lightblue", rug=T, ylim=c(-1000,7000))
plot(Gam2, select=3, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,15000))
plot(Gam2, select=4, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam2, select=5, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,8000))
plot(Gam2, select=6, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam2, select=7, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam2, select=8, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam2, select=9, shade=T, shade.col="lightblue", rug=T, ylim=c(-5000,500)) 
vis.gam(Gam2, view=c("PctFam2Par","householdsize"), phi=30, theta=40)

# 5.3 THIRD AND FINAL ESTIMATION
Gam3 <- gam((crimes~s(PopDens)+s(racepctblack)+s(PctPopUnderPov)+s(PctNotHSGrad)+s(agePct16t24,sp=1)+PctUnemployed+racePctHisp+s(PctFam2Par,householdsize)), method="REML")
#gamtabs(Gam3) 

summary(Gam3)
par(mfrow=c(2,3))
plot(Gam3, select=1, shade=T, shade.col="lightblue", xlim=c(0,20000), ylim=c(-5000,1000))
plot(Gam3, select=2, shade=T, shade.col="lightblue", rug=T, ylim=c(-3000,4000))
plot(Gam3, select=3, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,15000))
plot(Gam3, select=4, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam3, select=5, shade=T, shade.col="lightblue", rug=T, ylim=c(-2000,2000))
plot(Gam3, select=6, shade=T, shade.col="lightblue", rug=T, ylim=c(1,5))

par(mfrow=c(2,1))
vis.gam(Gam3, view=c("PctFam2Par","householdsize"), phi=30, theta=40)

# 5.4 CHECKING CONVERGENCE AND RESULTS OF OUR FINAL GAM
par(mfrow=c(2,2))
gam.check(Gam3)

## 6. REVISED PIECEWISE LINEAR REGRESSION MODEL

# 6.1 CREATING VARIABLES FOR KNOTS
HS_dummy <- rep(0,length(dataset$crimes))
HS_dummy[dataset$PctNotHSGrad>12] =1
dataset$HS_diff <- dataset$PctNotHSGrad - 12
dataset$HS_dummy <- HS_dummy
dataset$HS_piecewise <- dataset$HS_diff * dataset$HS_dummy


Age_dummy <- rep(0,length(dataset$crimes))
Age_dummy[dataset$agePct16t24>13] =1
dataset$Age_diff <- dataset$agePct16t24 - 13
dataset$Age_dummy <- Age_dummy
dataset$Age_piecewise <- dataset$Age_diff * dataset$Age_dummy

Pov_dummy <- rep(0,length(dataset$crimes))
Pov_dummy[dataset$PctPopUnderPov>20] =1
dataset$Pov_diff <- dataset$PctPopUnderPov - 20
dataset$Pov_dummy <- Pov_dummy
dataset$Pov_piecewise <- dataset$Pov_diff * dataset$Pov_dummy

dataset$racepctblack_sqr <- (dataset$racepctblack)^2

# 6.2 FITTING THE PIECEWISE LINEAR MODEL
fit.lm2 <- lm_robust(crimes~ PopDens + householdsize + racepctblack + racepctblack_sqr + racePctHisp + PctUnemployed + PctFam2Par + PctNotHSGrad + HS_piecewise + PctPopUnderPov + Pov_piecewise + agePct16t24 + Age_piecewise, data = dataset, se_type = "stata")
texreg(fit.lm2)
# 6.3

# COMPARATIVE PERFORMANCE TEST OF THE 3 MODEL SPECIFICATIONS

charcs <- c("communityname","state")

nobs = nrow(dataset)
nfold = 10
Kfold=cut(seq(1,nobs),breaks=nfold,labels=FALSE)
mse.test = matrix(0,nfold,3)

Xcol = colnames(dataset)[-10]
Xsqr = paste0("I(",Xcol,"^2)",collapse = "+")
Xcub = paste0("I(",Xcol,"^3)",collapse = "+")

fmla = paste0("crimes~(.)^2+", Xsqr,"+",Xcub)

#X = model.matrix(as.formula(fmla),data = dataset)[,-1]
#y = dataset[,10]

mysample = sample(1:nobs)
for (i in 1:nfold){
  cat("K-fold loop: ",i,"\r")
  test = mysample[which(Kfold == i)]
  train = mysample[which(Kfold != i)]
  
  fit.lm <- estimatr::lm_robust(crimes~PopDens+householdsize+PctFam2Par+racepctblack+racePctHisp+agePct16t24+PctPopUnderPov+PctNotHSGrad+PctUnemployed,data = dataset,subset=train, se_type = "stata" )
  fit.gam <- gam(crimes~s(PopDens)+s(racepctblack)+s(PctPopUnderPov)+s(PctNotHSGrad)+s(agePct16t24,sp=1)+PctUnemployed+racePctHisp+s(PctFam2Par,householdsize), method="REML",data = dataset,subset = train)
  fit.lm2 <- estimatr::lm_robust(crimes~ PopDens + householdsize + racepctblack + racepctblack_sqr + racePctHisp + PctUnemployed + PctFam2Par + PctNotHSGrad + HS_piecewise + PctPopUnderPov + Pov_piecewise + agePct16t24 + Age_piecewise, data = dataset, subset = train, se_type = "stata")  
  
  mse.test[i,1] = mean((dataset$crimes -predict(fit.lm,dataset))[-train]^2)
  mse.test[i,2] = mean((dataset$crimes - predict(fit.gam,dataset))[-train]^2)
  mse.test[i,3] = mean((dataset$crimes -predict(fit.lm2,dataset))[-train]^2)
  
}
mse = colMeans(mse.test)
round(mse,digits = 2)



#CREATING SECOND LINEAR MODEL

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

test <- lm(crimes~ PopDens + householdsize + racepctblack + racepctblack_sqr + racePctHisp + PctUnemployed + PctFam2Par + PctNotHSGrad + HS_piecewise + PctPopUnderPov + Pov_piecewise + agePct16t24 + Age_piecewise, data = dataset)

summary(test)

#TESTING PERFORMANCE OF ALL 3 MODELS

library(MASS)
library(randomForest)
library(gbm)
library(glmnet)
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
  
  fit.lm <- lm(crimes~PopDens+householdsize+PctFam2Par+racepctblack+racePctHisp+agePct16t24+PctPopUnderPov+PctNotHSGrad+PctUnemployed
               ,data = dataset,subset=train)
  fit.gam <- gam(crimes~s(PopDens)+s(racepctblack)+s(PctPopUnderPov)+s(PctNotHSGrad)+s(agePct16t24,sp=1)+PctUnemployed+racePctHisp+s(PctFam2Par,householdsize), method="REML",data = dataset,subset = train)
  fit.lm2 <- lm(crimes~ PopDens + householdsize + racepctblack + racepctblack_sqr + racePctHisp + PctUnemployed + PctFam2Par + PctNotHSGrad + HS_piecewise + PctPopUnderPov + Pov_piecewise + agePct16t24 + Age_piecewise, data = dataset,subset=train)
  
  
  mse.test[i,1] = mean((dataset$crimes -predict(fit.lm,dataset))[-train]^2)
  mse.test[i,2] = mean((dataset$crimes - predict(fit.gam,dataset))[-train]^2)
  mse.test[i,3] = mean((dataset$crimes -predict(fit.lm2,dataset))[-train]^2)
  
}
mse = colMeans(mse.test)
round(mse,digits = 2)
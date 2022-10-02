library(readxl)
library(dplyr)

#1. Database preparation

#1.1 Selection of variables
variables_chosen <- c("communityname:","state:","population:","PopDens:","householdsize:","PctFam2Par:","racepctblack:","racePctHisp:","agePct16t24:","perCapInc:","pctWSocSec:","pctWInvInc:","PctPopUnderPov:","PctNotHSGrad:","PctEmploy:","PolicPerPop:","PolicBudgPerPop:","ViolentCrimesPerPop:","nonViolPerPop:")
concat_dropped <- concat_test[variables_chosen]

#1.2 Cleaning of the database

#1.2.1 Keep complete observations 
is.data.frame(concat_dropped)#test dataframe ok
i=c(3:19) #Select variables to turn into numeric
concat_dropped[ , i] <- apply(concat_dropped[ , i], 2,
                    function(x) as.numeric(as.character(x)))#convert relevant variables in numeric
sapply(concat_dropped, class) #check ok
concat_wo_na= na.omit(concat_dropped)#Drop NA
concat_wi_na=concat_dropped[!complete.cases(concat_dropped),]#keep NA for statistics on drops
colnames(concat_wo_na)<-gsub(":","",colnames(concat_wo_na))
explanatory_variables <- c("population","PopDens","householdsize","PctFam2Par","racepctblack","racePctHisp","agePct16t24","perCapInc","pctWSocSec","pctWInvInc","PctPopUnderPov","PctNotHSGrad","PctEmploy","PolicPerPop","PolicBudgPerPop")
colnames(concat_wo_na)

lm_Violent = lm(ViolentCrimesPerPop~ PopDens + householdsize + PctFam2Par + racepctblack + racePctHisp + agePct16t24 + perCapInc + pctWSocSec + pctWInvInc + PctPopUnderPov + PctNotHSGrad + PctEmploy + PolicPerPop + PolicBudgPerPop,data = concat_wo_na)
summary(lm_Violent)

lm_Violent_wi_na = lm(ViolentCrimesPerPop~ PopDens + householdsize + PctFam2Par + racepctblack + racePctHisp + agePct16t24 + perCapInc + pctWSocSec + pctWInvInc + PctPopUnderPov + PctNotHSGrad + PctEmploy,data = concat_wo_na)
summary(lm_Violent_wi_na)

lm_Violent[1]
lm_Violent_wi_na[1]

# Statistically significant variables accross the two regressions : 
# PctFam2PAr, racepctblack, racePctHisp, pctWInvInc,PctNotHSGrad

#1.2.2 outliers?



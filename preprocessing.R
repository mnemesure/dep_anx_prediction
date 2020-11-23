# PREPROCESSING

library(haven)
data <- read_sav("data.sav")
predictorvars=c("Gruppenart","sibar1bt0","sibar1at0","Indikation","schulab","Behandlungsdauer","spe_sum_t0","PHQ_Dept0","PHQSomT0","PHQStressT0","GAD7T0","KSK12t0","PSK12t0","AV_n1_t0","AV_n2_t0","AV_n3_t0","AV_n4_t0","AV_n5_t0","AV_n6_t0","AV_n7_t0","AV_n8_t0","AV_n9_t0","AV_n10_t0","AV_n11_t0","sibar_risk","sibar_risk_di","sibar5at1","leistft1","rentet1","sibar2t1","sibar5t1","spe_sum_t1","PHQ_Dept1","PHQSomT1","PHQStressT1","GAD7T1","KSK12t1","PSK12t1","AV_n1_t1","AV_n2_t1","AV_n3_t1","AV_n4_t1","AV_n5_t1","AV_n6_t1","AV_n7_t1","AV_n8_t1","AV_n9_t1","AV_n10_t1","AV_n11_t1","sibar_risk_t1","sibar_risk_di_t1")

data$depchange=data$GAD7T0-data$GAD7T3

train=data[!is.na(data$depchange)&data$Imputation_==1,c("depchange",predictorvars)]
for(i in 2:10){
  tempimp=data[!is.na(data$depchange)&data$Imputation_==i,c("depchange",predictorvars)]
  train[train!=tempimp]=NA
}
train=data.matrix(train)
train=train[,c("depchange",predictorvars)]

varlabels <- read.csv("varlabels.csv", header=TRUE)
varlabels2=as.character(varlabels[varlabels$X%in%predictorvars,"x"])
varlabels3=gsub(" ","_",gsub("[[:punct:]]", "", varlabels2))
varlabels3

set.seed(12)
library(missForest)
imputeddata=missForest(train)

tempimputed=a=imputeddata$ximp
tempimputed2=data.matrix(tempimputed)

#write.csv(tempimputed2, "nick_rr_anx.csv")
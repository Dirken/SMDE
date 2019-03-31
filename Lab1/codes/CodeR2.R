# Author: Ricard Meyerhofer Parra
# Subject: SMDE
# Professor: Pau Fonseca
library(FactoMineR)
library(Rcmdr)

#Generate three populations that follow your specific distribution, but now change one of the parameters (change it following your criteria). 
#As an example, 
#the first is a population that follows a normal distribution with a parameter =0, the second with =10, and the third with =0.
#We want to analyze using an ANOVA if these three populations are different (or not) depending on the parameter selected.
#Analyze and explain the results obtained. Justify your answers.

Norm_v1=rnorm(200, mean=0, sd=1)
Norm_v2=rnorm(200, mean=10, sd=1)
Norm_v3=rnorm(200, mean=0, sd=1)

Norm_v1n=data.frame(x1=Norm_v1, x2="v1")
Norm_v2n=data.frame(x1=Norm_v2, x2="v2")
Norm_v3n=data.frame(x1=Norm_v3, x2="v3")
#intall.packages(Rcmndr)
library(Rcmdr)
data=mergeRows(Norm_v1n, Norm_v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), Norm_v3n, common.only=FALSE)

AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)
Boxplot(x1~x2, data=data, id.method="y")

#The observations within each sample must be independent.
#Durbin Watson 
library("lmtest")
dwtest(AnovaModel.1, alternative ="two.sided")
#The populations from which the samples are selected must be normal.
#Shapiro test
shapiro.test(residuals(AnovaModel.1))
#The populations from which the samples are selected must have equal variances (homogeneity of variance)
#Breusch Pagan test
lmtest::bptest(AnovaModel.1)



#On the dataset contained on FactoMiner package named 
#decathlon, we want to analyze if both competitions achieve different results in the different disciplines analyzed. 

data(decathlon, package="FactoMineR")
#dataset <- decathlon
#index <- dataset$Competition == "Decastar"
#index2 <- dataset$Competition == "OlympicG"
#dataset[index,]
#dataset[index2,]

#100m
data = data.frame(x1=decathlon$`100m`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is less than 0.05, it means that we reject null hypothesis, so these distributions are different.


#Long jump
data = data.frame(x1=decathlon$Long.jump, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
Boxplot(x1~x2, data=data, id.method="y")
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.

#Shot put
data = data.frame(x1=decathlon$Shot.put, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#High jump
data = data.frame(x1=decathlon$High.jump, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#400m
data = data.frame(x1=decathlon$`400m`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#100m hurdle
data = data.frame(x1=decathlon$`110m.hurdle`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#Discus
data = data.frame(x1=decathlon$`Discus`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#Pole.vault
data = data.frame(x1=decathlon$`Pole.vault`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.


#Javeline
data = data.frame(x1=decathlon$`Javeline`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.

#1500m
data = data.frame(x1=decathlon$`1500m`, x2=decathlon$Competition)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is more than 0.05, it means that we accept null hypothesis, so these distributions are equal.

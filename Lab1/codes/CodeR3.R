
library(Rcmdr)
data(decathlon, package="FactoMineR")

train <- subset(decathlon, Competition == "Decastar")
test <- subset(decathlon, Competition == "OlympicG")
train <- na.omit(train)
RegModel.1 <- lm(`1500m` ~ `100m` + `Long.jump` + `Shot.put` + `High.jump` + `400m` + 
                  `110m.hurdle` + `Discus` + `Pole.vault` + `Javeline` +`Rank`+ `Points`,
                data=train)

summary(RegModel.1)

# Linearity
summary(RegModel.1)
# Independence of residuals
dwtest(RegModel.1, alternative ="two.sided") 
# Normality 
shapiro.test(residuals(RegModel.1)) 
# Homoscedasticity 
lmtest::bptest(RegModel.1) 


prediction <- as.data.frame(predict(RegModel.1, newdata=test, interval="prediction"))
prediction$difference <- sapply((test$`1500m` <= prediction$upr) & (test$`1500m` >= prediction$lwr), ifelse, 'yes', 'no')
prediction

hitRate <- sum(prediction$difference == "yes")/(sum(prediction$difference == "no")+sum(prediction$difference == "yes"))
hitRate


#Now let's be a bit more smart:


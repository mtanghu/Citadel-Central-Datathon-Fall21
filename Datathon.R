---
title: "R Notebook"
output: html_notebook
---


```{r}
#data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Age.csv")
#data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Gender.csv")
#data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Income.csv")
data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Edu.csv")
data$Break_Out=factor(data$Break_Out)
#par(mfrow=c(1,2))
plot(Data_value~Break_Out,data,xlab="Edu",ylab="Percentage of Current Smoker")
stripchart(Data_value~Break_Out,data,vertical=TRUE,method="stack",xlab="Age",ylab="Percentage of Current Smoker")
```

```{r}
lm = lm(log(Data_value)~Break_Out,data)
#lm = lm(Data_value~Break_Out-1,data)
summary(lm)
```
```{r}
round(coef(lm),1)
```
Although Here the residuals are normal with some outliers and so we can go ahead with the inference without much concern.

```{r}
anova(lm)
par(mfrow=c(1,2))
qqnorm(residuals(lm))
qqline(residuals(lm))
plot(jitter(fitted(lm)),residuals(lm),xlab="Fitted",ylab="Residuals")+abline(h=0)
```
Since there exists 
```{r}
#med = with(data,tapply(Data_value,Break_Out,median))
#ar = with(data,abs(Data_value-med[Break_Out]))
#anova(lm(ar~Break_Out,data))
# log
med = with(data,tapply(log(Data_value),Break_Out,median))
ar = with(data,abs(log(Data_value)-med[Break_Out]))
anova(lm(ar~Break_Out,data))
```

```{r}
library(PMCMRplus)
vartest=anova(lm(ar~Break_Out,data))
if(vartest$`Pr(>F)`[1]<0.05){
  summary(tamhaneT2Test(log(data$Data_value),data$Break_Out))
}else{
  print(tci <- TukeyHSD(aov(Data_value~Break_Out,data)))
  plot(tci)
}
  
```
```{r}
require(MASS)
lm = lm(Data_value~Break_Out,data)
boxcox(lm,plotit=T,lambda=seq(-0.08,0.23,by=0.01))
```
```{r}
data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/FormeronYear.csv")
data$Year=factor(data$Year)
par(mfrow=c(1,2))
plot(Data_value~Year,data,xlab="Year",ylab="Smoke every day")
stripchart(Data_value~Year,data,vertical=TRUE,method="stack",xlab="Year",ylab="Smoke every day")
lm = lm(Data_value~Year,data)
summary(lm)
anova(lm)
par(mfrow=c(1,2))
qqnorm(residuals(lm))
qqline(residuals(lm))
plot(jitter(fitted(lm)),residuals(lm),xlab="Fitted",ylab="Residuals")+abline(h=0)
lmnull=lm(Data_value~1,data)
anova(lmnull,lm)
```
```{r}
data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2020Age.csv")
#data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2019Age.csv")
#data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Age.csv")
data$Break_Out=factor(data$Break_Out)
par(mfrow=c(1,2))
plot(Data_value~Break_Out,data,xlab="Age",ylab="Percentage of Current Smoker")
stripchart(Data_value~Break_Out,data,vertical=TRUE,method="stack",xlab="Age",ylab="Percentage of Current Smoker")
print("----------ANOVA with intercept----------")
print("if P(>F)<0.05, there is indeed a difference in the levels.")
lm = lm(Data_value~Break_Out,data)
#summary(lm)
anova(lm)
print("----------Fited model without intercept----------")
lmodi = lm(Data_value~Break_Out-1,data)
summary(lmodi)
print("----------Fited model without intercept ANOVA----------")
lmnull=lm(Data_value~1,data)
anova(lmnull,lmodi)
qqnorm(residuals(lm))
qqline(residuals(lm))
plot(jitter(fitted(lm)),residuals(lm),xlab="Fitted",ylab="Residuals")+abline(h=0)
print("----------Levene's Test----------")
print("if P(>F)>0.05, there is no evidence of non-constant variance.")
med = with(data,tapply(Data_value,Break_Out,median))
ar = with(data,abs(Data_value-med[Break_Out]))
anova(lm(ar~Break_Out,data))
if(vartest$`Pr(>F)`[1]<0.05){
    print("----------Transformation----------")
    require(MASS)
    boxcox(lm,plotit=T,lambda=seq(0.05,0.5,by=0.01))
    lmtr = lm((4*(Data_value^(0.25)-1))~Break_Out,data)
    summary(lmtr)
    par(mfrow=c(1,2))
    qqnorm(residuals(lmtr))
    qqline(residuals(lmtr))
    plot(jitter(fitted(lmtr)),residuals(lmtr),xlab="Fitted",ylab="Residuals")+abline(h=0)
    print("----------Levene's Test with Transformation----------")
    print("if P(>F)>0.05, there is no evidence of non-constant variance.")
    medtr = with(data,tapply((4*(Data_value^(0.25)-1)),Break_Out,median))
    artr = with(data,abs((4*(Data_value^(0.25)-1))-med[Break_Out]))
    anova(lm(artr~Break_Out,data))
}
print("----------Comparison Test----------")
library(PMCMRplus)
vartest=anova(lm(ar~Break_Out,data))
if(vartest$`Pr(>F)`[1]<0.05){
    print("----------Non-constant Var----------")
    summary(tamhaneT2Test(log(data$Data_value),data$Break_Out))
}else{
    print("----------Constant Var----------")
    print(tci <- TukeyHSD(aov(Data_value~Break_Out,data)))
    plot(tci)
}
```

```{r}
#data18 = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2018Edu.csv")
#data19 = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/isCurrent2019Edu.csv")
data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/FormeronYear.csv")
#data18$Break_Out=factor(data18$Break_Out)
#data19$Break_Out=factor(data19$Break_Out)
data$Year=factor(data$Break_Out)

par(mfrow=c(1,3))
plot(Data_value~Break_Out,data,xlab="Edu(2020)",ylab="Percentage of Current Smoker")
plot(Data_value~Break_Out,data19,xlab="Edu(2019)",ylab="Percentage of Current Smoker")
plot(Data_value~Break_Out,data18,xlab="Edu(2018)",ylab="Percentage of Current Smoker")
#stripchart(Data_value~Break_Out,data,vertical=TRUE,method="stack",xlab="Age",ylab="Percentage of Current Smoker")
print("----------ANOVA with intercept----------")
print("if P(>F)<0.05, there is indeed a difference in the levels.")
lm = lm(Data_value~Break_Out,data)
#summary(lm)
anova(lm)
print("----------Fited model without intercept----------")
lmodi = lm(Data_value~Break_Out-1,data)
summary(lmodi)
print("----------Fited model without intercept ANOVA----------")
lmnull=lm(Data_value~1,data)
anova(lmnull,lmodi)
qqnorm(residuals(lm))
qqline(residuals(lm))
plot(jitter(fitted(lm)),residuals(lm),xlab="Fitted",ylab="Residuals")+abline(h=0)
print("----------Levene's Test----------")
print("if P(>F)>0.05, there is no evidence of non-constant variance.")
med = with(data,tapply(Data_value,Break_Out,median))
ar = with(data,abs(Data_value-med[Break_Out]))
anova(lm(ar~Break_Out,data))

print("----------Transformation----------")
require(MASS)
boxcox(lm,plotit=T,lambda=seq(1.5,3.3,by=0.01))
lmtr = lm(1/2.25*(Data_value^(2.25)-1)~Break_Out,data)
summary(lmtr)
par(mfrow=c(1,2))
qqnorm(residuals(lmtr))
qqline(residuals(lmtr))
plot(jitter(fitted(lmtr)),residuals(lmtr),xlab="Fitted",ylab="Residuals")+abline(h=0)
print("----------Levene's Test with Transformation----------")
print("if P(>F)>0.05, there is no evidence of non-constant variance.")
medtr = with(data,tapply(1/2.25*(Data_value^(2.25)-1),Break_Out,median))
artr = with(data,abs(1/2.25*(Data_value^(2.25)-1)-medtr[Break_Out]))
anova(lm(artr~Break_Out,data))

print("----------Comparison Test----------")
library(PMCMRplus)
vartest=anova(lm(ar~Break_Out,data))
if(vartest$`Pr(>F)`[1]<0.05){
  print("----------Non-constant Var----------")
  summary(tamhaneT2Test(data$Data_value,data$Break_Out))
}else{
  print("----------Constant Var----------")
  print(tci <- TukeyHSD(aov(Data_value~Break_Out,data)))
  plot(tci)
  print(tci <- TukeyHSD(aov(1/2.25*(Data_value^(2.25)-1)~Break_Out,data)))
  plot(tci)
  
  tci20 <- TukeyHSD(aov(Data_value~Break_Out,data))
  tci19 <- TukeyHSD(aov(Data_value~Break_Out,data19))
  tci18 <- TukeyHSD(aov(Data_value~Break_Out,data18))
  par(mfrow=c(1,3))
  plot(tci20)
  plot(tci19)
  plot(tci18)
}
```

```{r}
data2 = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/NeverSmokedonYear.csv")
data3 = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/SmokeEverydayonYear.csv")
data = read.csv("C:/Users/Jun/Desktop/Courses/Datathon/FormeronYear.csv")
data$Year=factor(data$Year)
data2$Year=factor(data2$Year)
data3$Year=factor(data3$Year)
par(mfrow=c(1,3))
plot(Data_value~Year,data2,xlab="Year",ylab="Never Smoked")
plot(Data_value~Year,data,xlab="Year",ylab="Former")
plot(Data_value~Year,data3,xlab="Year",ylab="Smoke every day")
#stripchart(Data_value~Year,data,vertical=TRUE,method="stack",xlab="Year",ylab="Smoke every day")

lm = lm(Data_value~Year,data)
summary(lm)

anova(lm)
par(mfrow=c(1,2))
qqnorm(residuals(lm))
qqline(residuals(lm))
plot(jitter(fitted(lm)),residuals(lm),xlab="Fitted",ylab="Residuals")+abline(h=0)

```
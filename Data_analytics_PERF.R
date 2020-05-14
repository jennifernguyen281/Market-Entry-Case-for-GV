library(readxl)
data <- read_excel("Gyrfalcon Data Bank_Spring 2020(1).xlsx")
str(data)
official_data <- data[,-c(1,2,4:20)]
#pairs(official_data)


library(leaps)
lm_best <- regsubsets(PERF ~., data = official_data, nvmax = 17)
best.summary <- summary(lm_best)

par(mfrow =c(2,2))
plot(best.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
     type="l")

plot(best.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
which.min (best.summary$cp )
points (10, best.summary$cp [10], col ="red",cex =2, pch =20)
which.min (best.summary$bic)
plot(best.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",
     type="l")
points (6, best.summary$bic [6], col =" red",cex =2, pch =20)

plot(best.summary$adjr2 ,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
coef(lm_best,5) 

model_2 <- lm(PERF~`I-LAND`+`I-LOGAGLAND`  +  `I-CROPX` + `I-AGVAL-PC`  +  `I-GDPCAP`,
              data = official_data)
summary(model_2)

model_3 <- lm(PERF~ `I-LOGAGLAND` + `I-AGVAL-PC`  +  `I-GDPCAP`,
              data = official_data)

# I-LOGAGLAND is the most important factor for PERF

#summary(lm(PERF ~ `I-LOGAGLAND`, data = official_data))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     6.8190     0.1250   54.54   <2e-16 ***
  #`I-LOGAGLAND`   7.8299     0.2009   38.98   <2e-16 ***
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.2784 on 38 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.975 
#F-statistic:  1519 on 1 and 38 DF,  p-value: < 2.2e-16




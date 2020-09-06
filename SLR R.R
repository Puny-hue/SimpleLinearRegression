#1) carlories_consumed -> predict weight gained using calories consumed 

cal.wt<- calories_consumed
View(cal.wt)
summary(cal.wt)
attach(cal.wt)
colnames(cal.wt)<-c("weight_gained","calories_consumed")
colnames(cal.wt)
library(lattice)

# Graphical Representation
# plot for calories consumed 
dotplot(`Calories Consumed`, main="Dot plot of calories consumed")
boxplot(calories_consumed, col="pink")
hist(`Calories Consumed`)
qqnorm(`Calories Consumed`)
qqline(`Calories Consumed`)

#plots for weight gain
dotplot(`Weight gained (grams)`, main="dot plot of weight gain ")
boxplot(`Weight gained (grams)`, col="lightgreen", horizontal = FALSE)

hist(`Weight gained (grams)`)
qqnorm(`Weight gained (grams)`)
qqline(`Weight gained (grams)`)

# Scatter Plot(between calories consumed and weight gained)
plot(`Calories Consumed`,`Weight gained (grams)`,main="Scatter Plot", col="dodgerblue4",
     col.main="dodgerblue4", col.lab="dodgerblue4", xlab="calories consumed", ylab="weight gained",pch=20)
#correlation (between calories consumed and weight gained)
cor(`Calories Consumed`,`Weight gained (grams)`)#0.946991

#Regression
reg_cal.wt <- lm(`Weight gained (grams)`~`Calories Consumed`, data = calories_consumed)# Y~X #Linear Model
summary(reg_cal.wt)# R-Square =  0.8968
class(reg_cal.wt)
str(reg_cal.wt)


reg_cal.wt$coefficients
reg_cal.wt$residuals


# Regression line
calories_consumed.pred = -625.7523557+0.4201566*`Weight gained (grams)`
-625.7523557+0.4201566*3400
900-802.7801#(error= actual-predicted )
## 97.2199
#Root Mean square unit
sqrt(sum(reg_cal.wt$residuals^2)/nrow(cal.wt))
#RMSE= 103.3025

pred <- predict(reg_cal.wt)
pred
cor(pred, `Weight gained (grams)`)# 0.946991 correlation between input (predicted value and output variable)
plot(pred, `Weight gained (grams)`)

#Visualiztion 
 
install.packages("ggplot")
library(ggplot2)
ggplot(data=cal.wt,aes(x=calories_consumed, y=`Weight gained (grams)`))+geom_point(color ='navyblue')+
  geom_line(color='maroon',data = cal.wt,aes(x=calories_consumed,y=pred))

#doing trassformation of input variable

#Square root Trasformation
cor(`Weight gained (grams)`, sqrt(`Calories Consumed`))#0.9255962

# transform the variables to check whether the predict value is better
cal.wt_pred_sqrt<- lm(`Weight gained (grams)`~sqrt(`Calories Consumed`), data = cal.wt)
summary(cal.wt_pred_sqrt)# R-Square = 0.8567

plot(sqrt(`Calories Consumed`), `Weight gained (grams)`)
pred_sqrt<- predict(cal.wt_pred_sqrt)

# Root Means Value
sqrt(sum(cal.wt_pred_sqrt$residuals^2)/nrow(cal.wt))## RMSE VALUE = 121.7122


ggplot(data = cal.wt, aes(x = sqrt(calories_consumed), y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=sqrt(calories_consumed), y=pred_sqrt))

cal.wt_pred_sqrt$residuals
cal.wt_pred_sqrt$coefficients

#Logarithmic transformation
 
hist(log(YearsExperience))
skewness(log(YearsExperience))
cor(weight_gained,log(calories_consumed)) # 0.89
plot(log(calories_consumed),`Weight gained (grams)` ) 

reg_log <-lm(weight_gained~log(calories_consumed), data=cal.wt)
summary(reg_log)  #R-square = 0.8077
reg_log$coefficients
reg_log$residuals
pred_log <-  predict(reg_log)
sqrt(sum(reg_log$residuals^2)/nrow(cal.wt)) ## RMSE = 141.005
pred_log <- predict(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = cal.wt, aes(x = log(calories_consumed) , y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=log(calories_consumed), y=pred_log))

# Exponential Model
Salary <- Salary_Data_S_
hist(log(Salary))
boxplot(log(Salary))

skewness(Salary)
# x = sorting_time and y = log(delivery_time)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary)) # -0.93

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.93

exp(reg_exp$residuals)
exp(reg_exp$coefficients)
sqrt(mean(reg_exp$residuals^2)) # RMSE
sqrt(sum(reg_exp$residuals^2)/nrow(salary_data))   #RMSE = 0.094


logat <- predict(reg_exp) # predicted values
at <- exp(logat)

error = Salary - at
error

sqrt(sum(error^2)/nrow(salary_data))  #  RMSE =  7213.235

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = salary_data, aes(x = YearsExperience, y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=logat))

# Polynomial model with 2 degree (quadratic model)


plot(calories_consumed*calories_consumed, weight_gained)

cor(calories_consumed*calories_consumed, weight_gained) # 0.9710
hist(log(weight_gained))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(weight_gained) ~ calories_consumed + I(calories_consumed*calories_consumed))

summary(reg2degree) #0.98

logpol <- predict(reg2degree)
expy <- exp(logpol)
reg2degree$coefficients
err = weight_gained - expy

sqrt(sum(err^2)/nrow(cal.wt))  #SE # 117.4145

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = cal.wt, aes(x = calories_consumed + I(calories_consumed^2), y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=calories_consumed+I(calories_consumed^2), y=expy))



#2.) Delivery_time -> Predict delivery time using sorting time 
dvl.st <-  read.csv("C:/EXCELR/ASSIGNMENTS/SimpleLinearRegression/delivery_time.csv")
View(dvl.st)
colnames(dvl.st) <- c("delivery_time","sorting_time")
attach(dvl.st)

#EDA 
#Graphical Representation

# for delivery time

hist(delivery_time)
boxplot(delivery_time)
qqnorm(delivery_time)
qqline(delivery_time)
dotplot(delivery_time, main="Dot Plot of delivery time")
skewness(delivery_time)
kurtosis(delivery_time)

# for sorting time
hist(sorting_time)
boxplot(sorting_time)
qqnorm(sorting_time)
qqline(sorting_time)
dotplot(sorting_time, main="Dot Plot of sorting time")
skewness(sorting_time)
kurtosis(sorting_time)

# Scatter - Plot
?plot
plot(sorting_time,delivery_time, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="sorting time", 
     ylab="delivery time", pch=20) 

# Co-relation Analysis
cor(sorting_time,delivery_time) # 0.82
#ggplot(dvl.st,aes(x=sorting_time,y= delivery_time ), color="steelblue")

#Regression
reg_dvl.st <- lm(delivery_time~sorting_time, data = dvl.st) 
summary(reg_dvl.st) # R2 = 0.68
pred_dvl.st <- predict(reg_dvl.st) 

reg_dvl.st$residuals

sum(reg_dvl.st$residuals)

mean(reg_dvl.st$residuals)


# Root Mean Square Error
sqrt(sum(reg_dvl.st$residuals^2)/nrow(dvl.st))  #RMSE = 2.79

sqrt(mean(reg_dvl.st$residuals^2))

confint(reg_dvl.st,level=0.95)
predict(reg_dvl.st,interval="predict")

# ggplot for adding regresion line for data


ggplot(data = dvl.st, aes(x = sorting_time, y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time, y=pred_dvl.st))

#Transformation
# Square root transformation
cor(sqrt(sorting_time), delivery_time) #0.83 

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(delivery_time~sqrt(sorting_time), data=dvl.st)
summary(reg_sqrt) # R-square = 0.6958

plot(sqrt(sorting_time),delivery_time) 

# Root mean Square value

sqrt(sum(reg_sqrt$residuals^2)/nrow(dvl.st)) # = 0.479## RMSE 
pred_sqrt <- predict(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
reg_sqrt$ coefficients
ggplot(data = dvl.st, aes(x = sqrt(sorting_time), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sqrt(sorting_time), y=pred_sqrt))

# Logarithmic transformation
cor(delivery_time,log(sorting_time)) # 0.83
plot(delivery_time,log(sorting_time))

reg_log <-lm(delivery_time~log(sorting_time), data=dvl.st)
summary(reg_log)  #R-square = 0.69
reg_log$coefficients
sqrt(sum(reg_log$residuals^2)/nrow(dvl.st)) ## RMSE = 2.7331

pred_log <- predict(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = dvl.st, aes(x = log(sorting_time), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=log(sorting_time), y=pred_log))

# Exponential Model
hist(log(delivery_time))
boxplot(log(delivery_time))

# x = sorting_time and y = log(delivery_time)

plot(sorting_time, log(delivery_time))

cor(sorting_time, log(delivery_time)) # 0.84

reg_exp <- lm(log(delivery_time) ~ sorting_time)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.71

reg_exp$residuals
reg_exp$coefficients
pred_ex <- predict(reg_exp)
at <- exp(pred_ex)
error = delivery_time -at

sqrt(sum(error^2)/nrow(dvl.st))  #RMSE = 2.94




confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = dvl.st, aes(x = sorting_time, y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time, y=at))

# Polynomial model with 2 degree (quadratic model)


plot(sorting_time*sorting_time, delivery_time)

cor(sorting_time*sorting_time, delivery_time) # 0.79

plot(sorting_time*sorting_time, log(delivery_time))

cor(sorting_time, log(delivery_time)) #0.84
cor(sorting_time*sorting_time, log(delivery_time))0.78

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(delivery_time) ~ sorting_time + I(sorting_time*sorting_time))

summary(reg2degree) #0.76

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time - expy

sqrt(sum(err^2)/nrow(dvl.st))  #RMSE # 2.79

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = dvl.st, aes(x = sorting_time + I(sorting_time^2), y = log(delivery_time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time+I(sorting_time^2), y=logpol))

#  Polynomial model with 3 degree

reg3degree<-lm(log(delivery_time)~sorting_time + I(delivery_time*sorting_time) + I(sorting_time*sorting_time*sorting_time))

summary(reg3degree) R- Square =  0.9276
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = dvl.st, aes(x = sorting_time + I(sorting_time^2) + I(sorting_time^3), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time+I(sorting_time^2)+I(sorting_time^3), y=expy3))


# 3.) Emp_data -> Build a prediction model for Churn_out_rate

emp_data <- read.csv("C:/EXCELR/ASSIGNMENTS/SimpleLinearRegression/emp_data.csv")
View(emp_data)
attach(emp_data)
colnames(emp_data)
#EDA
library(moments)
library(ggplot2)
# For Salary Hike
hist(Salary_hike)
boxplot(Salary_hike)
qqnorm(Salary_hike)
qqline(Salary_hike)
skewness(Salary_hike)
kurtosis(Salary_hike)

# For Churn out rate
hist(Churn_out_rate)
boxplot(Churn_out_rate)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
skewness(Churn_out_rate)
kurtosis(Churn_out_rate)

#  Scatter Plot
?plot
plot(Salary_hike,Churn_out_rate, xlab="salary_hike",ylab="churn_out_rate")
# Co-relation
cor(Salary_hike,Churn_out_rate) # - 0.911
# Regression 

reg_sal_churn <- lm(Churn_out_rate~ Salary_hike,data =emp_data )
summary(reg_sal_churn) # R square = 0.83

#Root mean Square error
sqrt(sum(reg_sal_churn$residuals^2)/nrow(emp_data)) # = 3.99## RMSE 
pred_sal_churn <- predict(reg_sal_churn) 

reg_sal_churn$residuals
reg_sal_churn$coefficients
sum(reg_sal_churn$residuals) 

mean(reg_sal_churn$residuals)


confint(reg_sal_churn,level=0.95)
predict(reg_sal_churn,interval="predict")

# ggplot for adding regresion line for data

library(ggplot2)
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred_sal_churn))

#Transformation
# Square root transformation

cor(sqrt(Salary_hike), Churn_out_rate) #-0.91 

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike), data=emp_data)
summary(reg_sqrt) # R-square = 0.84

plot(sqrt(Salary_hike),Churn_out_rate) 

# Root mean Square value

sqrt(sum(reg_sqrt$residuals^2)/nrow(emp_data)) # = 3.89## RMSE 
pred_sqrt <- predict(reg_sqrt)
pred_sqrt2 <- pred_sqrt * pred_sqrt
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

ggplot(data = emp_data, aes(x = sqrt(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sqrt(Salary_hike), y=pred_sqrt))
reg_sqrt$coefficients
reg_sqrt$residuals

# Logarithmic transformation
hist(log(Salary_hike))
skewness(log(Salary_hike))
cor(Churn_out_rate,log(Salary_hike)) # -0.92
plot(log(Salary_hike),Churn_out_rate )

reg_log <-lm(Churn_out_rate~log(Salary_hike), data=emp_data)
summary(reg_log)  #R-square = 0.84
reg_log$coefficients
sqrt(sum(reg_log$residuals^2)/nrow(emp_data)) ## RMSE = 3.7

pred_log <- predict(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = emp_data, aes(x = log(Salary_hike) , y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=log(Salary_hike), y=pred_log))

# Exponential Model
library(moments)
hist(log(Churn_out_rate))
boxplot(log(Churn_out_rate))
skewness(Churn_out_rate)
# x = sorting_time and y = log(delivery_time)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate)) # -0.93

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.87

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2)) # RMSE
sqrt(sum(reg_exp$residuals^2)/nrow(emp_data))   #RMSE = 0.04


logat <- predict(reg_exp) # predicted values
at <- exp(logat)

error = delivery_time - at
error

sqrt(sum(error^2)/nrow(dvl.st))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=at))

# Polynomial model with 2 degree (quadratic model)


plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate) # -0.901


# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree) #0.98

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time - expy

sqrt(sum(reg2degree$residuals^2)/nrow(emp_data))  #RMSE # 0.0167

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Churn_out_rate^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=expy))


#4) Salary_data -> Build a prediction model for Salary

salary_data <- read.csv("C:/EXCELR/ASSIGNMENTS/SimpleLinearRegression/Salary_Data.csv")
View(salary_data)
attach(salary_data)

#EDA
#Graphical Representation
library(moments)
library(ggplot2)

# For YearsExperience
hist(YearsExperience)
boxplot(YearsExperience)
qqnorm(YearsExperience)
qqline(YearsExperience)
skewness(YearsExperience)
kurtosis(YearsExperience)

# For Salary
hist(Salary)
boxplot(Salary)
qqnorm(Salary)
qqline(Salary)
skewness(Salary)
kurtosis(Salary)

#  Scatter Plot
?plot

plot(YearsExperience,Salary, xlab="YearsExperience",ylab="Salary")
# Co-relation
cor(YearsExperience,Salary) # 0.978

# Regression 
reg_Ye_sal <- lm(Salary~ YearsExperience,data =salary_data )
summary(reg_Ye_sal) # R square = 0.957

#Root mean Square error
sqrt(sum(reg_Ye_sal$residuals^2)/nrow(salary_data)) # = 5592.044## RMSE 
pred_Ye_Sal <- predict(reg_Ye_sal) 

reg_Ye_sal$residuals
reg_Ye_sal$coefficients
sum(reg_Ye_sal$residuals) 

mean(reg_Ye_sal$residuals)


confint(reg_Ye_sal,level=0.95)
predict(reg_Ye_sal,interval="predict")

# ggplot for adding regresion line for data

library(ggplot2)
ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=pred_Ye_Sal))

#Transformation
# Square root transformation

cor(sqrt(YearsExperience), Salary) #-0.964

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Salary~sqrt(YearsExperience), data=salary_data)
summary(reg_sqrt) # R-square = 0.93

plot(sqrt(YearsExperience),Salary) 

# Root mean Square value

sqrt(sum(reg_sqrt$residuals^2)/nrow(salary_data)) # = 7080.096## RMSE 
pred_sqrt <- predict(reg_sqrt)
pred_sqrt2 <- pred_sqrt * pred_sqrt
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

ggplot(data = salary_data, aes(x = sqrt(YearsExperience), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=sqrt(YearsExperience), y=pred_sqrt))
reg_sqrt$coefficients
reg_sqrt$residuals

# Logarithmic transformation
hist(log(YearsExperience))
skewness(log(YearsExperience))
cor(Salary,log(YearsExperience)) # -0.92
plot(log(YearsExperience),Salary )

reg_log <-lm(Salary~log(YearsExperience), data=salary_data)

summary(reg_log)  #R-square = 0.85
reg_log$coefficients
sqrt(sum(reg_log$residuals^2)/nrow(salary_data)) ## RMSE = 10302.89

pred_log <- predict(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = salary_data, aes(x = log(YearsExperience) , y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=log(YearsExperience), y=pred_log))

# Exponential Model
library(moments)
hist(log(Salary))
boxplot(log(Salary))
skewness(Salary)
# x = sorting_time and y = log(delivery_time)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary)) # -0.93

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.93

exp(reg_exp$residuals)
exp(reg_exp$coefficients)
sqrt(mean(reg_exp$residuals^2)) # RMSE
sqrt(sum(reg_exp$residuals^2)/nrow(salary_data))   #RMSE = 0.094


logat <- predict(reg_exp) # predicted values
at <- exp(logat)

error = Salary - at
error

sqrt(sum(error^2)/nrow(salary_data))  #  RMSE =  7213.235

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = salary_data, aes(x = YearsExperience, y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=logat))

# Polynomial model with 2 degree (quadratic model)


plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate) # -0.901


# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree) #0.98

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time - expy

sqrt(sum(reg2degree$residuals^2)/nrow(emp_data))  #RMSE # 0.0167

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Churn_out_rate^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=expy))
##Regression-Line :-log (salary) = 10.5074019 + 0.1254529(YearsExperience) 






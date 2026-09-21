#Task 1: Linear Probability Model

data <- read.csv("Loan.csv", header = TRUE)
head(data,5)
#Check the structure of the dataset using str function
str(data)

#1. Import the data into R. Convert “Education” into factor (you can use as.factor() function).
data$Education <- as.factor(data$Education)
str(data)
# Education: Factor w/ 3 levels "1","2","3": 1 1 1 2 2 2 2 3 2 3

#2. Run a linear probability model by regressing Loan on all the other variables (Income, Family,
#CCAvg, Education). Show the summary of the regression results. How do you interpret the
#coefficients in front of the two Education variables?

linear_prob_model <- lm(Loan~., data=data)
summary(linear_prob_model)
#Education2   1.517e-01  8.473e-03  17.908  < 2e-16 ***
#Education3   1.605e-01  8.229e-03  19.511  < 2e-16 ***
#Two Education variables are significantly important for the model and their coefficients 
#have p value less than 0.05

#3. What does the fitted 𝑦̂ (𝑦̂ = 𝑋𝛽̂ ) from a linear probability model mean? Are there any
#customers with the fitted 𝑦̂ being greater than 1 or less than 0 in this data set? Show some
#of those customers.

#fitted y hat (predicted probability of success)
#from a linear probability model means that some probability values may fall outside of 
#0 to 1 range and may be invalid.

head(sort(predict(linear_prob_model)),10) #will give use first 10 predicted values
#in this case these values are less than 0

tail(sort(predict(linear_prob_model)),10) #last 10 predicted values, values are less than 1

#Task 2: Logit Model

#4. Next, run a Logit model of Loan on the same set of independent variables (Income, Family,
#CCAvg, Education) using the glm() function. Show the summary of the estimation results.

logit_model <- glm(Loan~.,family=binomial(link=logit),data=data)
summary(logit_model)
#AIC: 1346.8

#Create a vector of all 1 for all observations - initial prediction of outcome for all observations
yhat <- rep(1,nrow(data))
#pred(logit_model, type="response" - predicted probability of success 
#mean(data$Loan) - used to calculate threshold of success, default one 0.5
#we are comparing vector values to scalar values to get true or false results
yhat[ predict(logit_model, type="response") < mean(data$Loan) ] <-0
#customers with predicted probability of success whose predicted results are higher than threshold, 
#are those who likely to get the loan.


#5.Create the confusion matrix and calculate the Percent Correctly Predicted (PCP), 
#both at the overall level and for each possible outcome separately
#(i.e., PCP for 𝑦 = 1 and 𝑦 = 0,respectively). 
#As we discussed in class, use the fraction of “success” in the original data as 
#the threshold in predicting the binary outcomes.

#create confusion matrix
confusion <- table(yhat, data$Loan)
confusion

#calculate the Percent Correctly Predicted (PCP)
sum(diag(confusion)) /sum(confusion)
#0.884

#𝑦 = 0
confusion[1,1] / sum(confusion[,1]) #0.885177
#𝑦 = 1
confusion[2,2] / sum(confusion[,2]) #0.8729167

#6.Calculate the predicted probability of success according to the Logit model estimation
#results. Evaluate the probability at such values of the X variables: {Income, Family, CCAvg}
#equal their mean values in the original data and Education=”2”.
#• First calculate the probability using the defining formula as explained in class. You may
#do the calculation in R, but be explicit about the exact formula used and substitute in
#the exact numbers.
#• Then use the predict(…, type=”response”) function to obtain the predicted probability
#of success. The value should be the same as your own calculation result above

##Evaluate the probability at such values of the X variables: {Income, Family, CCAvg}
#equal their mean values in the original data and Education=”2”
#get coefficient estimated results
coef(logit_model)
# (Intercept)       Income       Family        CCAvg   Education2   Education3 
# -13.17783285   0.05979075   0.58707882   0.16267911   3.91060897   3.93317273 

#calculate mean of numeric column values (Income, Family, CCAvg)
colMeans(data[,2:4])
#   Income    Family     CCAvg 
# 73.774200  2.396400  1.937938 

#coefficient estimated results * mean of numeric column values (Income, Family, CCAvg)
#coef(logit_model) * colMeans(data[,2:4]) + Education2

exp(-13.17783285+0.05979075*73.774200+0.58707882*2.396400+0.16267911*1.937938+3.91060897)/(1+exp(-13.17783285+0.05979075*73.774200+0.58707882*2.396400+0.16267911*1.937938+3.91060897))
# 0.04172352 - predicted probability of success

xvalues <- data.frame(Income=73.774200,Family=2.396400,CCAvg=1.937938,Education="2")

#Then use the predict(…, type=”response”) function to obtain the predicted probability
#of success. The value should be the same as your own calculation result above
predict(logit_model,newdata=xvalues,type="response")
#0.04172353 - predicted probability of success


#7. Co-list and compare the coefficients from the linear probability model and the Logit model.
#Are the coefficients from the two models directly comparable? Why?

#compare beta coefficients
coeff_linear_prob_model <- coef(linear_prob_model)
coeff_logit_model <- coef(logit_model)

models_combined = cbind(coeff_linear_prob_model,coeff_logit_model)
models_combined
#beta coefficients are not comparable accross the models

#8. Calculate the partial effects of all independent variables based on the Logit model. Again,
#evaluate the probability at such values of the independent variables: {Income, Family,
#CCAvg} equal their mean values in the original data and Education=”2”. Also calculate the
#partial effects based on the linear probability model (Hint: simply the 𝛽 coefficients). 
#Co-list and compare the partial effects from the two models. 
#Are they comparable?

xvalues <- data.frame(Income=73.774200,Family=2.396400,CCAvg=1.937938,Education="2")

#calculate linear combination of x and beta
xb<- predict(logit_model,newdata=xvalues)

#calculate partial effects of logit model using dlogis for pdf(probability density function)
#coef function returns beta coefficient estimates and exclude intercept [-1]
par_eff_logit <- dlogis(xb) * coef(logit_model)[-1]

#calculate partial effects of linear probability model, exclude intercept
part_eff_linear <- coef(linear_prob_model)[-1]
#in linear model beta coefficients are partial effects

#Co-list and compare the partial effects from the two models
cbind(par_eff_logit, part_eff_linear)

#partial effects are pretty close to each other and directly comparable
#are directly comparable because the partial effects from both models quantify 
#the impact of a unit change in the independent variable on the probability of success."

data <- read.csv("Mobile_data_usage.csv", header = TRUE)
str(data)
#1. Our main focus is to investigate how users’ daily data usage amount DataUse changes with
#the remaining quota Quota when controlling the number of days left Days. First, create a
#scatter plot to visualize the data. Set DataUse as the y-axis and Quota as the x-axis. Save the
#plot from the R output below. What pattern in the data requires special treatments beyond
#the usual linear regression?
install.packages("censReg")
library(censReg)
#create a scatter plot to visualize the data. Set DataUse as the y-axis and Quota as the x-axis
plot(data$Quota, data$DataUse)
#much data is censored at 0 of Data usage

#2. Estimate a linear model by simply regressing DataUse on Quota and Days. Use the lm()
#function and display the R output for the summary of the regression results.
lm_model <- lm(DataUse~Quota+Days, data=data)
summary(lm_model)

#3. Next, estimate the following Tobit model:
#𝐷𝑎𝑡𝑎𝑈𝑠𝑒𝑖𝑡∗ = 𝛽0 + 𝛽1 ∙ 𝑄𝑢𝑜𝑡𝑎𝑖𝑡 + 𝛽2 ∙ 𝐷𝑎𝑦𝑠𝑖𝑡 + 𝜀𝑖𝑡
#𝐷𝑎𝑡𝑎𝑈𝑠𝑒𝑖𝑡 = {𝐷𝑎𝑡𝑎𝑈𝑠𝑒𝑖𝑡
 # ∗ if 𝐷𝑎𝑡𝑎𝑈𝑠𝑒𝑖𝑡∗ ≥ 0
  #0 if 𝐷𝑎𝑡𝑎𝑈𝑠𝑒𝑖𝑡∗ < 0
#Use the censReg() function included in the censReg package. Include all the R output for the
#summary of the regression results.

#Tobit model
tobit_model <- censReg(DataUse~Quota+Days, data=data)
summary(tobit_model)

beta <- coef(tobit_model)[1:3]
sigma <- exp(coef(tobit_model)[4]) #last coeff is logSigma
sigma
#logSigma 
#4.45807

#4. List together and compare the estimates of the linear model and the Tobit model. (Note:
#only include the first three elements: intercept, Quota, Day.) How does the coefficient for
#Quota from the Tobit model compare to that from the linear model? Why is it greater/less?
coef_lm <- coef(lm_model)
#beta <- coef(tobit_model)[1:3]
models_combined <- cbind(coef_lm, beta)
models_combined

#Linear Model (lm): "For every 1-unit increase in Quota,
#the observed DataUse increases by 0.0079 units across the entire sample."
#(This is a dampened average because it blends zero-users with active users).

#Tobit Model: "For every 1-unit increase in Quota,
#the latent propensity to use data increases by 0.0090 units." 
#(This reflects the true, uninhibited marginal effect on users who are actively consuming data).
# Tobit model is specifically built to handle censored data. 
#It understands that a "0" in DataUse doesn't just mean zero; 
#it represents a threshold where the user's underlying desire 
#or propensity to use data could be zero or even "conceptually negative

#DataUse cannot fall below zero—it is censored at 0. 
#Because of this structural floor, a standard linear model 
#understates the true impact of predictors.

#5. Compute the marginal effects of Quota based on the Tobit model at two different X values:
#(i) Quota = 10, Days = the mean of Days in the sample;
#(ii) Quota = 2,000, Days = the mean of Days in the sample.
#Use the margEff() function in the censReg package.
#(Hint: don’t forget to include 1 in the xValues argument to account for the intercept.) 
#Compare the marginal effects with that of the linear model. 
#(Note that the marginal effects of a linear model simply equal the coefficients.) 
#What conclusions and implications can you draw? What do these marginal effect values mean?
#How and why do the marginal effects vary as the explanatory variables change?

days_mean <- mean(data$Days)

##(i) Quota = 10, Days = the mean of Days in the sample

quota_1 <- 10
quota_2 <- 2000

margEff(tobit_model, xValues = c(1, quota_1, days_mean))
#Quota         Days 
#0.004917625 -0.006517316

#(ii) Quota = 2,000, Days = the mean of Days in the sample
margEff(tobit_model, xValues = c(1, quota_2, days_mean))

#Quota         Days 
#0.009013811 -0.011945981 

coef_lm
#(Intercept)        Quota         Days 
# 1.363301864  0.007976548 -0.012323914 

#At a low baseline (Quota = 10): The marginal effect is 0.0049. If a user with a tiny quota receives an extra unit of data allowance, 
#their actual consumption barely moves.

#The marginal effects tell you how much a user's actual, real-world data consumption changes when you modify their Quota by one unit, 
#depending entirely on where they start
#At a low baseline (Quota = 10): The marginal effect is 0.0049. If a user with a tiny quota receives an extra unit of data allowance, 
#their actual consumption barely moves.

#At a high baseline (Quota = 2,000): The marginal effect jumps to 0.0090. 
#For an active user with a large quota, giving them one more unit yields the "true," 
#uninhibited increase in consumption because they face no friction from a zero-use floor

#If you rely on the linear model (lm), 
#you will believe that every user increases data consumption by 0.0079 per unit of quota.


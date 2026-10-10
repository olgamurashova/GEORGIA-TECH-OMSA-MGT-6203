data = read.csv("Bank_Attrition.csv", header=TRUE)
str(data)
#1. Estimate a proportional hazard model with the Weibull distribution using the survreg()
#function. Properly specify the dependent variable ChurnTime with the Censored indicators,
#and include all the other variables (except CustomerID) as the explanatory variables (i.e., 9 in total).
install.packages('survival')
library(survival)

survival_model <- survreg(Surv(ChurnTime, 1-Censored)~ Age + Income + HomeVal + Tenure + DirectDeposit + Loan + NumAccounts + Dist + MktShare, data=data, dist="weibull")
summary(survival_model)

#2.As we explained in class, the reported estimates from the survreg() function is not the 𝛽
#estimates directly (but the 𝛿 estimates instead), and the “scale” estimate is not directly the
#shape parameter 𝑎 in the Weibull model either. Transform them to the 𝛽 estimates and the
#shape parameter 𝑎 as we demonstrated in class. Interpret both the reported 𝛿 estimates
#and the transformed 𝛽 estimates. For example, what do the two types of coefficients before
#Income mean?

a <- 1/survival_model$scale
# a = 2.255127

beta <- -coef(survival_model) * a

#3.Plot the hazard function based on the estimation results, evaluated at the mean values of
#the explanatory variables from the data sample. Is it positively or negatively duration
#dependent? What does it mean? Also, how would you interpret the hazard rates? Try
#interpreting the meaning of a particular point on the plot.

xbar <- colMeans(data[,2:10])
xbeta <- crossprod(c(1, xbar), beta)
curve(exp(c(xbeta))*a*x^(a-1), xlim=c(0,20), xlab = "time", ylab = "hazard")
#The model is positively duration dependent because the shape parameter
#\(a\) (alpha) is greater than 1 (\(a = 2.255127 > 1\))

#The baseline hazard increases over time. For your business context, this means that the longer a customer stays with the company,
#the more likely they are to churn at any given moment. 

#4.Plot the density function of the duration based on the estimation results, evaluated at the
#mean values of the explanatory variables from the data sample. To help you better
#understand what this density function means, add the histogram of the ChurnTime variable
#from the original data (for those uncensored observations) to the same plot (plotting the
#density rather than the count for the y-axis). They should be on the same scale and roughly
#follow the same pattern. (You may set “breaks=50” for the hist() function.)

curve(dweibull(x, shape=a, scale=exp(-xbeta/a)), xlab="Churn Time", ylab="density", xlim=c(0,20), ylim=c(0,0.12))
hist(data$ChurnTime[data$Censored==0], breaks=50, freq=FALSE, add=TRUE, col=NULL)

#The peak of that bell-shaped density curve (and the tallest bars of your histogram) right between 5 and 9 time units represents the period 
#where the highest volume of customer churn occurs.
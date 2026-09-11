# Set working directory 
setwd("~/GT HOMEWORK/Data Analytics Business - MGT-6203/HOMEWORK_1")

#Load and inspect data
data <- read.csv("UsedCars.csv", header = TRUE)
head(data, 10)
attach(data)
#1. Import that data into R. Run a linear regression of Price on all the available explanatory
#variables (i.e., Age, KM, HP, Metallic, Automatic, CC, Doors, Gears, Weight). Use the
#summary() function to show the regression results. 

lm.res <- lm(Price~Age+KM+HP+Metallic+Automatic+CC+Doors+Gears+Weight, data = data)
lm.res
lm.sum <- summary(lm.res)
names(lm.res)
lm.res$residuals
lm.sum$coefficients

#2. Calculate the fitted values of the response variable, and calculate the residuals. Co-list the
#original 𝑦 values, fitted 𝑦̂ values, and the residuals together for the first 10 observations.
#Check if the residuals equal 𝑦 − 𝑦^

#Calculate predicted variable as the fitted values of the response variable
yhat = fitted(lm.res)
yhat

#calculate residuals (residuals are the difference between the actual observation and predicted value)
uhat = resid(lm.res)
uhat

#Co-list the original 𝑦 values, fitted 𝑦̂ values, and the residuals together for the first 10 observation
cbind(Age, KM, HP, Metallic, Automatic, CC, Doors, Gears, Weight, Price, yhat,uhat)[1:10,]

#3.Re-produce the t-statistics for all 𝛽̂𝑗, using the defining formula 𝒕(𝜷̂𝒋) = 𝜷̂𝒋
#𝒔𝒆(𝜷̂𝒋). Co-list your calculated t-statistics along with the t-statistics generated from summary()
#of the regression results. They should be exactly the same

#reproduce t-values which are Estimat / Std. Error , can be retrieved from lm.sum$coefficients
bhat <- lm.sum$coefficients[,1]
std <- lm.sum$coefficients[,2]
tstat <- bhat / std

cbind(lm.sum$coefficients[,3], tstat)

#4. Determine the critical value (or cutoff) of the t-statistic for a 𝛽 estimate to be considered as
#significant at 95% confidence level. You need to first determine the degree of freedom of
#your model (Hint: you can simply retrieve the value of df.residual from the regression result.) 
#Then you need to find the corresponding percentile of the t distribution (with tha degree of freedom). 
#(Hint: use qt() function to find a certain percentile of a t distribution

#find t cutoffs
#n <- nobs(lm.res)
#k <- nrow(lm.sum$coefficients) -1
#degree of freedom: df <- n-k-1

#degree of freedom
df <- lm.res$df.residual

#find the corresponding percentile of the t distribution
alpha <- 1 - 0.95
qt(1-alpha/2,df)
#t-cutoff = 1.961858, if t-cutoff is less than t-value of a variable,it means the variable is significant

#t distribution converts to standard normal distribution as degree of freedom increases
qnorm(1-alpha/2)

#5.Calculate the p-value for each 𝛽̂𝑗 using the defining formula 𝒑 = 𝟐 ∙ 𝐏𝐫(𝒕 < −|𝒕𝒔𝒕𝒂𝒕|).
#(Hint: use pt() function for the cdf of t distribution.) Co-list your calculated p-values along
#with the p-values generated from summary() of the regression results. They should be exactly the same.

#Reproduce p-values

pval <- 2 * pt(-abs(tstat), df)
cbind(pval, lm.sum$coefficients[,4])

#6. Which explanatory variables have significant effects on the outcome, that is, which 𝛽
#estimates are significantly different from zero? You can find the answers either by
#comparing the t-statistics (obtained in Step 3) to the critical values (obtained in Step 4) or by
#comparing the p-values (obtained in Step 5) to (1-confidence level), as we discussed in class.
#The conclusions should be the same. (Note: use 95% confidence level.

#p-value = 0.05, any variable with p value greater than 0.05 is insignificant
lm.sum$coefficients
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.883e+03  1.505e+03  -1.916 0.055649 .  
#Age         -1.299e+02  2.610e+00 -49.782  < 2e-16 ***
#KM          -1.465e-02  1.441e-03 -10.162  < 2e-16 ***
#HP           2.540e+01  3.376e+00   7.524 1.01e-13 ***
#Metallic    -2.281e+01  7.436e+01  -0.307 0.759059    
#Automatic    4.991e+02  1.497e+02   3.334 0.000881 ***
#CC          -5.259e-03  8.499e-02  -0.062 0.950667    
#Doors        1.315e+01  4.086e+01   0.322 0.747638    
#Gears        6.281e+02  1.927e+02   3.259 0.001148 ** 
#Weight       1.504e+01  1.300e+00  11.567  < 2e-16 ***
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#7. Calculate the R-squared of the regression you have performed, using the defining formula
#𝑹𝟐 = 𝑬𝑺𝑺/𝑻𝑺𝑺. Compare your calculated value with the R-squared value calculated by the routine. 
#(Hint: you can retrieve r.squared from the summary() output.) Again, they should be the same. 
#can be reproduced as follows: 
lm.sum$r.squared
[1] 0.8648677
var(yhat) / var(Price)
[1] 0.8648677
1-var(uhat) / var(Price)
[1] 0.8648677

#8. Install and load the package “car”. Use the vif() function included in the package to
#calculate the variance inflation factor (VIF) for the 𝛽 estimators. Examine the VIF values
#and discuss if there is any sign of multicollinearity among independent variables

install.packages("car")
library(car)
#VIF Variance Inflation Factor
vif(lm.res)
#VIFs should always be closer to 1, the smaller the better
Age        KM        HP        Metallic Automatic    CC     Doors     Gears    Weight 
1.914772  1.586299  1.548521  1.017014  1.101810  1.104307  1.269749  1.129618  2.096541
#1 < VIF < 5: Low to moderate correlation. 
#This is generally considered perfectly acceptable in most applied work and requires no action
#1 < VIF < 5: Low to moderate correlation. This is generally considered perfectly acceptable in most applied work and requires no action

#Perfectly Independent (~1.0 to 1.2): Metallic (1.01), Automatic (1.10), CC (1.10), Gears (1.12), 
#and Doors (1.26) have almost no correlation with the other variables. 
#Their standard errors are not inflated at all.Low/Acceptable Correlation (~1.5 to 2.1): 
#HP (1.54), KM (1.58), Age (1.91), and Weight (2.09) show slight correlation, which is completely normal and healthy for real-world car data
#(for example, older cars naturally tend to have higher kilometers).


#9. Re-produce the VIF for the coefficient of Weight (which has the largest VIF value), following
#these two steps:
 # i. Regress Weight on all the other independent variables, and obtain the R-squared
#ii. Calculate the VIF using the defining formula 𝑽𝑰𝑭(𝜷̂𝒋) = 𝟏−𝑹𝒋

lm.res2 <- lm(Weight~Age+KM+HP+Metallic+Automatic+CC+Doors+Gears, data=data)
r2.weight <- summary(lm.res2)$r.squared
1/(1 - r2.weight)
# vfi = 2.096541 which matches our VIF calculation

#10. Remove from the model the independent variables which are NOT significant according to
#your conclusion in Step 6. Run a new linear regression of Price on the remaining
#independent variables. Use the summary() function to show the regression results.

#remove Metallic, CC, Doors variables as they are not significant
lm.res3 <- lm(Price~Age+KM+HP+Automatic+Gears+Weight, data = data)
lm.sum3 <- summary(lm.res3)

#11. Retrieve and compare the R-squared and Adjusted R-Squared from the two models (the full
#regression with all independent variables in Step 1 versus the new model with only the
#independent variables that are significant in Step 10). Discuss your findings with regard to
#the relative magnitudes of the R-squared and the Adjusted R-Squared from the two models and what they imply.

lm.sum3$r.squared
0.8648474
lm.sum3$adj.r.squared
0.8642022

lm.sum$r.squared
0.8648677
lm.sum$adj.r.squared
0.8638979

#Adjusted R-Squared penalizes the number of variables
#and provides a more reliable metric for comparing models with different numbers of predictors
#When comparing your two models, lm.sum3 is the better choice because it achieves virtually
#the same explanatory power while using fewer variables. Adjusted R-squared score is higher. 

#12. Use the results from the model with a better fit to discuss the effects of certain
#independent variables on the dependent variable: Holding everything else equal, how much
#the sales price would decrease if a car were one year older? What if a car accumulated
#10,000 more kilometers?

The regression coefficient for Age is approximately (-129.9). 
This means that for every single month the car ages, its value drops by €129.90. 
12 x €129.90, the sales price would decrease by approximately €1,558.80.

The regression coefficient for KM is approximately (-0.01463).
10,000 X 0.01463, If a car accumulated 10,000 more kilometers, 
holding everything else equal, the sales price would decrease by approximately €146.30 
Time (Age) decays the car's value much faster than Usage (KM).

detach(data)
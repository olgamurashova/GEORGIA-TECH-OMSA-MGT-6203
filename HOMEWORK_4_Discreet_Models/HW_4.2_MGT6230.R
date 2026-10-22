#First run a simple OLS and report the estimation results.
data <- read.csv("Education_data1.csv")
data <- na.omit(data[, c("wage", "educ", "exper", "nearc4", "fatheduc", "motheduc")])
str(data)

install.packages('AER')
library(AER)

hist(data$wage, breaks = 50) #distribution is right-skewed
hist(log(data$wage), breaks = 50) #using log makes data distribution more symmetrical

ols.res <-lm(log(wage)~educ+exper+I(exper^2), data=data)
summary(ols.res)
#educ 0.0897809 : one year of education could increase an individual's salary by 9%

#3. Let’s use nearc4 as the instrumental variable. We will first perform the two-stage least
#square manually. First, perform the first-stage regression. Is the endogenous variable 𝑒𝑑𝑢𝑐
#is correlated with the proposed IV nearc4?

stage1 <- lm(educ~nearc4+fatheduc+motheduc+exper+I(exper^2), data=data)
summary(stage1)

#nearc4 0.322109 indicates a presence of correlation

#4. Then perform the second-stage regression, and obtain the 2SLS estimation results.
stage2 <- lm(log(wage)~fitted(stage1)+exper+I(exper^2), data=data)
summary(stage2)

#fitted(stage1)  0.1561088, includes monther's and father's education, 
#and shows that one extra year of education could increase wage by 15%

#5.Next, use the ivreg() function in the AER package, and verify that all coefficient estimates
#are the same as your manual 2SLS results.

TSLS.res <- ivreg(log(wage)~educ+exper+I(exper^2)|nearc4+fatheduc+motheduc+exper+I(exper^2), data=data)
summary(TSLS.res)

#6. Now compare the estimated coefficients from the OLS and the 2SLS. How did OLS bias the
#estimated return on education? Is the direction somewhat counterintuitive? Can you come
#up with some reasoning to justify the finding?

cbind(coef(ols.res), coef(TSLS.res))

#educ in ols model, shows that the model underestimates effect of educ on wage
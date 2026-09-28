data = read.csv("Forum_Posts.csv", header=TRUE)
str(data)

#Task 1: Poisson Regression Model

#1. Estimate a Poisson regression model using this data set by regressing posts on all the other
#explanatory variables. Use the glm() function and produce the summary of the estimation
#results.
poisson_model<- glm(posts~., family=poisson, data=data)
summary(poisson_model)

#2. Examine the potential overdispersion by computing the estimate of 𝜎2 as we introduced in
#class. Compare the estimated 𝜎̂ 2 to 1, and state your conclusion regarding whether the data
#are over-dispersed or not.

#YHAT - predicted value
y <- data$posts
yhat <- predict(poisson_model,type="response")
sum((y-yhat)^2/yhat) / (nrow(data) - length(coef(poisson_model)))
#5.168645, q2 is over 1, and is overdispersed

#Task 2: Negative Binomial Model
#3. Next, estimate a negative binomial model using this data set in a similar fashion, that is, by
#regressing posts on all the other explanatory variables. You need to first install the MASS
#package, and then use the glm.nb() function to do the estimation.

install.packages('MASS')
library(MASS)

neg_binomial_model <- glm.nb(posts~., data=data)
summary(neg_binomial_model)

#Only 1 independent variable remains statistically significant: 
#Only totalPosts is significant at the 95% confidence level (\(p = 0.00323 < 0.05\)).
#Variables that lost significance: Both readingRate (\(p = 0.271\)) and wknd (\(p = 0.312\)) 
#were significant in Poisson model but are no longer significant here. 
#This is a classic symptom of overdispersion—the Poisson model was 
#overconfident due to underestimated standard errors.

#4. How would you interpret the estimated 𝜃̂ ? What does the value imply about the potential
#overdispersion?
#Theta:  0.11547 is very small which is a sign of overdispersion

#5. Compute and compare the AIC and BIC of both the Poisson regression model and the
#negative binomial model. Which model fits the data better

cbind(AIC(poisson_model), AIC(neg_binomial_model))
cbind(BIC(poisson_model), BIC(neg_binomial_model))

#AIC    [,1]     [,2]
# [1,] 25458.83 16309.15

#BIC
#      [,1]     [,2]
#[1,] 25495.8 16353.51

#The Negative Binomial model fits the data better as it has lower values

#6. Based on the model estimation results, predict the probability of any given number of posts
#using the Poisson regression model and the negative binomial model, respectively. Create a
#vector of 0:20 as k, and calculate the predicted probability of observing k number of posts
#based on the Poisson regression model and the negative binomial model (evaluated at the
#mean of the explanatory variables in the sample) as p1 and p2, respectively. Plot p1 and p2
#against k on the same plot, as we demonstrated in class. Discuss how and why the
#distributions of the Poisson model and the negative binomial model differ.

xbar <- colMeans(data)[2:5] #exclude 1 column as it is predicted y

#calculate linear combination of xB
x_pois <- crossprod(coef(poisson_model), c(1,xbar))
x_neg_binom <- crossprod(coef(neg_binomial_model), c(1,xbar))

#Create a vector of 0:20 as k
k <- 0:20 #vector of post counts 

#probability density; predicted probabilities of observing k number of posts
p1 <- dpois(k,exp(x_pois)) #poisson model

p2 <- dnbinom(k,size=neg_binomial_model$theta, mu=exp(x_neg_binom)) #negative binomial model

plot(k, p2, pch=16, xlab="Posts", ylab='Prob')

points(k,p1)

legend("topright", c("Poisson", "Negative Binomial"), pch=c(1,16))

#the negative binomial probability shows that a probability for a user 
#not to post anything is over 80%, while the poisson prob is around 70%
#The negative binomial model has more variance in the count variable values than the Poisson model because the negative binomial model places
#more probability density on larger count values (e.g., 3, 4, and 5)



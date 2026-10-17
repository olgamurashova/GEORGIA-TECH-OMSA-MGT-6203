install.packages('mlogit')
library(mlogit)
#1. Properly import the data using the mlogit.data() function. 
#Specify the right data format (i.e.,#long) and the related variables.
data <- read.csv('Commute_Mode.csv')
str(data)
data_long <- mlogit.data(data, shape="long", choice = "choice", alt.var = "mode", chid.var = "id")
head(data_long)

#2. Estimate the discrete choice model using the mlogit() function. Show the R output for the
#estimation results.

discrete_model <- mlogit(choice~cost+time, data_long)
summary(discrete_model)

#3. Examine the estimated coefficients and make sense of them. Are the signs of the
#coefficients reasonable? How many intercept coefficients are included, and why the
#number?

#bus choice has been selected as a baseline, there three intercept coefficients included: car, carpool, rail
#carpool has a negative coefficient of -0.9051585 which means that compared to other choices, it is cheaper.
#while car and rail are more costly.

#4. Compute the predicted choice probability for each alternative. Use the fitted() function and
#show the first few individuals’ predicted choice probability for all alternatives. Interpret the
#results: for example, what does the value at the first row and the last column mean?

head(fitted(discrete_model, outcome=FALSE))

#results are probability measures between 0 and 1; 
#The predicted choice probabilities of all alternatives of all individuals.
#car has a higher probability of being chosen

#5. Calculate the marginal effects of the covariate “time” of the rail alternative on the choice
#probability of all alternatives. Evaluate the marginal effects at the mean values of the
#explanatory variables in the data sample. Interpret the results: for example, what do the
#values at (i) the last row and first column and (ii) the last row and the last column mean

time_avg = tapply(data_long$time, data_long$mode, mean)
time_avg
#     bus      car  carpool     rail 
#39.92341 37.04412 39.77101 39.50451 #average time per each mode of transportation

cost_avg = tapply(data_long$cost, data_long$mode, mean)
cost_avg
#     bus      car  carpool     rail 
#2.035669 4.873487 1.686348 2.211683  #average cost per each mode of transportation

x_val = data.frame(time=time_avg, cost=cost_avg)
x_val

#calculate marginal effects
effects(discrete_model, covariate = "time", data=x_val)
effects()

#A unit increase in the commuting time of the “rail” alternative 
#will increase the probability of “bus” being chosen by about 0.29%.

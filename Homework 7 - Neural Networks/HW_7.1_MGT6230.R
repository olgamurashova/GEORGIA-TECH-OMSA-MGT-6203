#1. Install and load the “neuralnet” package that will be needed. Import the data into R
install.packages("neuralnet")
library(neuralnet)

data <- read.csv("Smarket.csv")

#2. Note that neural networks perform the best when variables are scaled. To scale the data,
#remove Columns 1 and 8 (which are “Year” and “Today”) because we won’t incorporate
#them into the analysis. Also exclude the “Up” column when scaling because it is binary and
#meant to enter the analysis as is. Use the scale() function to scale the data.

data <- data[,-c(1,8)]
data[,-7] <- scale(data[,-7])

#3. Separate the scaled data into a training set and a testing set. Use 80% of all records as the
#training set (i.e., 1,000 records) and leave the rest 20% as the testing set (i.e., 250 records).
#Do random sampling in creating the training/testing sets. As usual, to ensure replicability,
#set the seed to 1000.

n.train <- floor(nrow(data) *0.8)
set.seed(1000)
sample_data <- sample(1:nrow(data), n.train)
train_data <- data[sample_data,]
test_data <- data[-sample_data,]

#4. Let us first fit a simple neural network to the training data set, with only 1 layer and 2
#activation nodes. Use Up as the output variable, and only Lag1 and Lag2 as the input
#variables. Use the neuralnet() function in the “neuralnet” package. Set the linear.output
#argument to FALSE, meaning that we are applying the same logistic activation function to
#the output node as well.
#Note: Because the estimation of neural networks involves randomness, the fitted neural
#network will be different each time it’s run. To ensure uniformity of the answers, please
#use the neural network I have pre-fitted for you and provided on Canvas. Use the
#following statement to load the pre-fitted neural network and complete the rest of Task 2
#based on this provided neural network.
#load("Smarket_nn1.Rda")

neural_net_model <- neuralnet(Up ~ Lag1 + Lag2, data=train_data, hidden=2, linear.output = FALSE)
summary(neural_net_model)
load("Smarket_nn1.Rda")

#5. Plot the neural network using the plot() function. Be sure to understand what all the
#information on the plot means.
plot(nn, rep='best')
nn$weights

#7. Take the first record in the testing data set.

#Manually calculate the values of all activation nodes and the output node for this record.
#Use the formula we introduced in class with the logistic (a.k.a. Sigmoid) activation function. Be
#sure to include the bias terms (a.k.a. intercepts). You need to use the weights retrieved in
#Step 6.
test_data[1,]

s1 <- (-1.5526810) + (-0.4006151) * (-0.5516457) + (0.3979986) * (0.9047775)
s1 <- exp(s1) / (1+exp(s1))
s1 #node1 #0.274565

s2 <- (-20.092955) +  (1.325688) * (-0.5516457) + (33.606746) *(0.9047775)
s2 <- exp(s2) / (1 + exp(s2))
s2 #node2 #0.9999311

#output value 
p <- (-0.2670554) +  (2.5133068) *s1 + (-0.7918837)*s2
p <- exp(p) / (1+exp(p))
p #0.4088266

compute(nn, test_data[1,])

#9. Now let us fit a more complex neural network to the training data set, with 2 layers, 4
#activation nodes for the first layer, and 2 activation nodes for the second layer. Now
#include all available variables as the predictors: Lag1 through Lag5, and Volume. Same as
#Step 4, use the neuralnet() function, and set the linear.output argument to FALSE.
#Note: Again, because the estimation of neural networks involves randomness, the fitted
#neural network will be different each time it’s run. To ensure uniformity of the answers,
#please use the neural network I have pre-fitted for you and provided on Canvas. Use the
#following statement to load the pre-fitted neural network and complete the rest of Task 3
#based on this provided neural network.
#load("Smarket_nn2.Rda")

neural_net_model <- neuralnet(Up ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +Volume, data=train_data, hidden= c(4,2), linear.output = FALSE)
load("Smarket_nn2.Rda")
plot(nn,rep="best")

nn$weights #show the weights

#11. Use the given neural network to predict the probabilities of “Up” for all the trading days in
#the testing set. Use the compute() function.

pred <- compute(nn, test_data)
pred$net.result

#12. Based on the predicted probabilities of “Up” in the previous step, determine the predicted
#market performance (i.e., whether “Up” is TRUE or FALSE) for each trading day in the
#testing set. Use 0.5 as the cutoff.
pred.class <- rep(FALSE,nrow(test_data))
pred.class[pred$net.result >0.5] <- TRUE

#13. Compare the predicted outcomes with the actual outcomes by constructing the confusion
#matrix. Calculate the out-of-sample prediction accuracy of this multi-layer neural network.

confusion <- table(pred.class, test_data$Up)
confusion
sum(diag(confusion)) / sum(confusion)
#0.54 prediction accuracy

#14.As a benchmark for performance comparison, fit a usual logistic regression model to the
#training data set.
#• As we did previously, use the glm() function, and include all available predictors.
log_reg <- glm(Up~., data=train_data, family=binomial(link=logit))
summary(log_reg)

#• Predict the probabilities of “Up” for all the trading days in the testing set. Use the
#predict() function, and set type=”response”.
pred_prob <- predict(log_reg, test_data, type='response')

#• Determine the predicted market performance (i.e., whether “Up” is TRUE or FALSE)
#for each trading day in the testing set by using 0.5 as the cutoff for the predicted
#probabilities.
logit_pred <- rep(FALSE,nrow(test_data))
logit_pred[pred_prob>0.5] <- TRUE

#• Construct the confusion matrix, and calculate the out-of-sample prediction accuracy
#of the logistic regression model.
confusion <- table(logit_pred, test_data$Up)
confusion

sum(diag(confusion)) / sum(confusion)
#0.516 prediction accuracy
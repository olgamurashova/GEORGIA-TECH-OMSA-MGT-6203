#Load and inspect data
#1. Import “UsedCars2.csv.” Set the stringsAsFactors option to TRUE so the string variables are
#treated as factors. Examine the structure of the data to make sure categorical variables are
#properly stored as factors.

data <- read.csv("UsedCars2.csv", stringsAsFactors = TRUE, header = TRUE)
head(data, 10)
str(data)
attach(data)

#2. According to our analysis from last homework, we already find those variables that have
#significant effects on Price. Run a linear regression of Price on these variables 
#(i.e., Age, KM,HP, Automatic, Gears, Weight) and also include Color. Show the regression result summary.
#Discuss how Color is treated in the regression. How many dummy variables are created, and
#which color is the baseline? Use contrasts() to show how the dummy variables are coded.
#What does each dummy variable’s coefficient mean? Does the color of a car have any
#significant effect on the sales price?

lm <- lm(Price~Age+KM+HP+Automatic+Gears+Weight+Color, data=data)
lm.sum <- summary(lm)
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.431e+03  1.684e+03  -2.037 0.041864 *  
  Age         -1.280e+02  2.620e+00 -48.853  < 2e-16 ***
  KM          -1.516e-02  1.438e-03 -10.537  < 2e-16 ***
  HP           2.632e+01  3.296e+00   7.985 3.17e-15 ***
  Automatic    5.036e+02  1.472e+02   3.422 0.000641 ***
  Gears        5.768e+02  1.893e+02   3.048 0.002355 ** 
  Weight       1.496e+01  1.201e+00  12.456  < 2e-16 ***
  ColorBlack   9.440e+02  7.118e+02   1.326 0.184999    
ColorBlue    7.770e+02  7.105e+02   1.094 0.274287    
ColorGreen   4.866e+02  7.115e+02   0.684 0.494143    
ColorGrey    8.511e+02  7.104e+02   1.198 0.231119    
ColorRed     5.941e+02  7.099e+02   0.837 0.402789    
ColorSilver  9.442e+02  7.165e+02   1.318 0.187777    
ColorViolet  1.246e+03  9.333e+02   1.336 0.181939    
ColorWhite   4.485e+02  7.596e+02   0.590 0.555018    
ColorYellow  2.276e+02  9.980e+02   0.228 0.819664    
---
#9 dummy variables were created for Color, and are treated as insignificant variables, however, all the color dummy variables
#have positive estimates
contrasts(Color)
#beige color is the baseline

#3. Following Step 2, exclude Color from the regression, but include an interaction term
#between Age and KM in the regression. (Notice that you also need to include the main
#effects of Age and KM.) Show the regression result summary and discuss the implications. 
#Is the interaction effect between Age and KM significant? If so, what does it mean?

lm2 <- lm(Price~Age:KM+HP+Automatic+Gears+Weight, data=data)
#OR to include the main effects of Age and KM#
lm3 <- lm(Price~Age*KM+HP+Automatic+Gears+Weight, data=data)
summary(lm3)

#the interaction effect between Age and KM is significant and has a positive estimate 7.237e-04 
#while Age and KM variables on their own have a significant effect with negative estimates.
#the negative effect of KM on the sales price is weakened as a car ages.

#4. Create a scatterplot of KM and Price. Is there any non-linear relationship? If so, briefly
#describe such a non-linear relationship in words. We can capture such a non-linear
#relationship in a regression along with other independent variables. This is called a
#Generalized Additive Model (GAM). Regress Price on Polynomials of KM up to degree 4
#along with another variable Automatic. Set “raw=TRUE” to use the standard basis functions
#for the polynomials. Show the regression result summary and see how many coefficients
#there are in total. Add the curve of fitted Price versus KM onto the plot. Notice that you
#need to specify the value of Automatic (as the mean value from the data) in order to produce the curve.
#Hint: You can use the following R code to plot the curve (where lm3 is the Polynomial regression result).
#km.grid <- seq(from=min(KM), to=max(KM), by=1000)
#preds <- predict(lm3, newdata=list(KM=km.grid, Automatic=rep(mean(Automatic),length(km.grid))))

#Create a scatterplot of KM and Price
plot(KM, Price, cex=0.6)
#The plot shows that there is a non-linear relationship between Price and KM, the higher mileage, 
#the lower price of a car

fit1 <- lm(Price~KM, data=data)
summary(fit1)
abline(fit1) #add a linear regression line to the plot

#GAM
library(mgcv)
# Fit the Generalized Additive Model
gam.model <- gam(Price ~ KM + s(Age) + s(Weight) + HP + Automatic, data = data)
summary(gam.model)

#Regress Price on Polynomials of KM up to degree 4 along with another variable Automatic.
fit2 <- lm(Price~poly(KM, 4, raw=TRUE)+Automatic, data=data)
summary(fit2)

#Add the curve of fitted Price versus KM onto the plot
km.grid <- seq(from=min(KM), to=max(KM), by=1000)
preds <- predict(fit2, newdata=list(KM=km.grid, Automatic=rep(mean(Automatic),length(km.grid))))
lines(km.grid, preds, col= "blue", lwd=2)
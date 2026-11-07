#1. Applying the algorithm of User-based Collaborative Filtering (UBCF) as we learned in class,
#manually calculate the predicted rating of User 1 (𝑈1) on Item 2 (𝐼2).



#2. nstall the recommenderlab package and load it into R. (If prompted, you may need to
#install a few packages used in the recommenderlab package.) Load the MovieLense data
#(using data() function). To perform personalized recommendations, based on your own
#preferences, rate the following 15 movies (all of which are included in the MovieLense
#dataset). First create your ratings as a matrix, and then convert it into a “realRatingMatrix”.

install.packages("recommenderlab")
library(recommenderlab)
data(MovieLense)
getRatingMatrix(MovieLense)
fix(MovieLenseMeta)
fix(MovieLenseUser)
#the data consists of 3 datasets, to review each dataset we can use fix function

myrating <- matrix(NA, 1, 1664)

myrating[c(1, 11, 12, 22, 23, 29, 50, 56, 64, 71, 82, 88, 94, 96, 98)] <- c(5, 4, 4, 5, 3, 2, 5, 5, 4, 3, 4, 5, 4, 5, 2)

myrating <- as(myrating, "realRatingMatrix")

#3. Perform UBCF on the MovieLense data. Then use your own ratings as the new data, and
#predict a “topNList” for you. You can set n=10 (which is the default value), and display the
#list. 

recom_ubcf <- Recommender(MovieLense, "UBCF") #- user based collaborative filtering

predic_ubcf <- predict(recom_ubcf, myrating, n=10, type="topNList")

as(predic_ubcf, "list")
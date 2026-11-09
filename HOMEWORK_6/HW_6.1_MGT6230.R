# 1. Install and load the three packages that will be needed: “tm”, “SnowballC”, and “lsa”.
install.packages("tm")
install.packages("SnowballC")
install.packages("lsa")
library(tm)
library(SnowballC)
library(lsa)


#2. Import the data into R. Take the following steps:
#Import the data from the file "Ads.csv" using read.csv() as usual, and save it to a 
#(data frame) variable (e.g., “mydata”);

data <- read.csv("Ads.csv")

#Take the first two columns of the above data frame (i.e., “doc_id” and “text”) and
#convert it into a corpus using the DataframeSource() function in this way:
#Corpus(DataframeSource(mydata[,1:2])). Save the corpus to a variable;

corpus_data <- Corpus(DataframeSource(data[,1:2]))

#Take the third column of the above data frame (i.e., “label”), and save it to a variable.
#This will be the outcome variable.

label_var <- data$label

#3. Perform the following preprocessing of the text:
#Remove extra white spaces, using tm_map(…, stripWhitespace);
corpus_data <- tm_map(corpus_data, stripWhitespace)

#Remove punctuations, using tm_map(…, removePunctuation);
corpus_data <- tm_map(corpus_data, removePunctuation)

#Remove numbers, using tm_map(…, removeNumbers);
corpus_data <- tm_map(corpus_data, removeNumbers)

#Remove stopwords, using tm_map(…, removeWords, stopwords(“english”));
corpus_data <- tm_map(corpus_data,removeWords,stopwords("english"))

#Stem the documents, using tm_map(…, stemDocument)
corpus_data <- tm_map(corpus_data, stemDocument)

#4. Based on the processed corpus, construct the term-document matrix using the
#TermDocumentMatrix() function. Display the first 30 terms in the matrix.

term_doc_matrix <- TermDocumentMatrix(corpus_data)
term_doc_matrix$dimnames$Terms[1:30]

#<<TermDocumentMatrix (terms: 17300, documents: 1000)>>
#Non-/sparse entries: 110829/17189171

#5. Calculate the TF-IDF values for the term-document matrix using the weightTfIdf() function.

tf_idf <-  weightTfIdf(term_doc_matrix)
#The strength of unique relevance of a certain term to a certain document

#6. Further reduce the dimensionality by performing the Latent Semantic Analysis (LSA) and
#construct 20 concepts. Use the lsa() function and set the dim argument to 20.
#Note: This step is computationally intensive and may take several minutes to complete.
#Once the result is available, convert the “dk” component (which stores the document-concept matrix) into a data frame.

lsa_tfidf <- lsa(tf_idf, dim=20)
lsa_tfidf$dk[1:10,]

words_df <- as.data.frame(as.matrix(lsa_tfidf$dk))

#7. Let’s perform the logistic regression here.
#First, as usual, separate the data into the training set and the test set. Set the seed to 1111.
#Draw a random sample of 800 documents from the 1,000 documents in the corpus. For the 3
#training and test sets, respectively, combine the corresponding labels and the values in the
#document-concept matrix from Step 6.

#Draw a random sample of 800 documents from the 1,000 documents in the corpus
set.seed(1111)

sample_data <- sample(1:1000, 800)

train_data <- cbind(label=label_var[sample_data], words_df[sample_data,])

test_data <- cbind(label=label_var[-sample_data], words_df[-sample_data,])

fix(train_data) #examine data 


#8. Run a logistic regression with the label as the dependent variable and the document-
#concept coefficients as the independent variables using the glm() function, just as we did
#before. Display a summary of the logistic regression results. (Inspect the output and recall what they mean.)

log_reg <- glm(label~., family=binomial(link=logit), data=train_data)
summary(log_reg)

#9. Evaluate the out-of-sample prediction performance of our model:
#For ads in the test set, predict the probability of an ad being “relevant” using predict(…,type=”response”).
pred_prob <- predict(log_reg, newdata=test_data, type="response")
  
#Use 0.5 as the threshold. Classify the ads with the predicted probability of being
#relevant greater than or equal to 0.5 as “relevant,” and those with the predicted
#probability less than 0.5 as “irrelevant.”

yhat <-rep(1, nrow(test_data))
yhat[pred_prob<0.5] <- 0

#Compare the predicted outcomes (i.e., ir/relevancy) with the actual outcomes for all ads
#in the test set. Construct and display the confusion matrix. Calculate the overall
#prediction accuracy. Comment on the performance of our simple text mining analysis
confusion_matrix <- table(yhat, test_data$label)
confusion_matrix

sum(diag(confusion_matrix)) / sum(confusion_matrix)
#0.93 accuracy rate


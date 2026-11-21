#Install and load the four packages that will be needed: “tm”, “SnowballC”, “topicmodels”,
#and “wordcloud”.

install.packages('topicmodels')
install.packages('wordcloud')
library(topicmodels)
library(wordcloud)
library(tm)
library(SnowballC)

#2.Import the data into R. Take the following steps:
#Import the data from the file "News.csv" using read.csv() as usual, and save it to a (dataframe) variable (e.g., “textdata”);
data <- read.csv('News.csv')

#Convert the data frame into a corpus using the DataframeSource() function in this way:Corpus(DataframeSource(textdata)). Save the corpus to a variable;

data_corpus <- Corpus(DataframeSource(data))
head(data_corpus)

#3. Perform the following preprocessing of the text:
data_corp_clean <- data_corpus
#Remove extra white spaces, using tm_map(…, stripWhitespace);
data_corp_clean <- tm_map(data_corp_clean, stripWhitespace)

#Remove punctuations, using tm_map(…, removePunctuation);
data_corp_clean <- tm_map(data_corp_clean, removePunctuation)

#Remove numbers, using tm_map(…, removeNumbers);
data_corp_clean <- tm_map(data_corp_clean, removeNumbers)

#Remove stopwords, using tm_map(…, removeWords, stopwords(“english”));
data_corp_clean <- tm_map(data_corp_clean, removeWords, stopwords("english"))

#Stem the documents, using tm_map(…, stemDocument);
data_corp_clean <- tm_map(data_corp_clean, stemDocument)
#Note: Keep the original corpus so that we can retrieve the original full news content later.
#Save the processed corpus to a different variable.

#4. Based on the processed corpus, construct the document-term matrix using the
#DocumentTermMatrix() function. Set the minimum frequency for terms to be included to
#be 3. Display the dimension of the matrix and explain what the numbers of rows and
#columns mean.

doc_term_matrx <- DocumentTermMatrix(data_corp_clean, control=list(bounds=list(global=c(3, Inf))))

dim(doc_term_matrx) #dimension of the matrix

nTerms(doc_term_matrx) #number of terms

nDocs(doc_term_matrx) #number of docs

doc_term_matrx$dimnames$Terms[1:30]
row.indx <- slam::row_sums(doc_term_matrx) > 0
data <- data[row.indx, ]
dim(doc_term_matrx)

#5. Set the seed to 1000, and perform the LDA analysis using the LDA() function. Set the
#number of topics to be 20 and the method to be “Gibbs.” In the “control” argument,
#specify the number of iterations to be 1000 and one status print every 50 iterations

set.seed(1000)
LDA_anals <- LDA(doc_term_matrx, 20, method="Gibbs", control=list(iter=1000, verbose=50))

#6. Extract the posterior distributions from the LDA result, and do the following:
LDA_anals_poster <- posterior(LDA_anals)

#• Retrieve the “terms” matrix. What is this matrix about? What is its dimension? What
#does each row/column represent? Display the first 5 columns of this matrix.
terms_matx <- LDA_anals_poster$terms
dim(terms_matx)
terms_matx[,1:5]

#• Retrieve the “topics” matrix. What is this matrix about? What is its dimension? What
#does each row/column represent? Display the first 5 rows of this matrix.
topics_matrx <- LDA_anals_poster$topics
dim(topics_matrx)
topics_matrx[1:5,]

#7. Display the 10 most relevant terms for each of the 20 topics, using the terms() function.
#Pick 3 topics, think what each of them is about, and come up with a label/description for
#each of them.

terms(LDA_anals,10)
terms(LDA_anals, 10)[, 14]

#8. Pick a news article that interests you the most. (You may go back to the original News.csv
#file to examine the text content.) For example, news article #1082 describes the global
#financial market reactions right after 2016 U.S. presidential election. Complete the following
#tasks:

#• Display the text content of the news article of your choice. Note: you need to retrieve
#the text from the original (NOT the processed) corpus.
#as.character(data_corpus[7], content)
as.character(data_corpus[1082], content)

#• Create a bar plot showing the topic distribution associated with this document.
#barplot(topics_matrx[7,])
barplot(topics_matrx[1082,])

#• Pick the topic that is the most relevant to this document. Sort and retrieve the 50 most
#relevant terms associated with this topic, and create a word cloud of these terms. Think
#about what this topic is describing and come up with a label/description for it.

top.term.prob <- sort(terms_matx[4,], decreasing = TRUE)[1:50]
wordcloud(names(top.term.prob), top.term.prob, random.order = FALSE)

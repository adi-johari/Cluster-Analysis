#read csv file
library(tm)
tweets<- readLines("Tweets.txt")

#Build corpus
corpus<- Corpus(VectorSource(tweets))

#Create term document matrix
tdm<- TermDocumentMatrix(corpus, control= list(minWordLength=c(1, Inf)))
t <- removeSparseTerms(tdm, sparse=0.98)
m<- as.matrix(t)

#Plot frequent Terms
freq<- rowSums(m)
freq<- subset(freq, freq>=50)
barplot(freq, las=2, col= rainbow(25))


#Hierarchial tweet clustering using Dendogram
distance<- dist(scale(m))
print(distance, digits=2)
hc<- hclust(distance, method = "ward.D")
plot(hc, hang=-1 )
rect.hclust(hc, k=10)


#Nonhierarchical k-means clustering of tweets
m1<- t(m)
set.seed(222)
k<- 18
kc<- kmeans(m1, k)
kc

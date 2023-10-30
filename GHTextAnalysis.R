library(readr)
imdb_top_1000 <- read_csv("C:/Users/Nadezhda/Desktop/imdb_top_1000.csv")
vec1 <- imdb_top_1000$Overview
library(tm)
library(SnowballC)
docs <- VCorpus(VectorSource(vec1))
head(docs[[1]])
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument) # Если нужен стеминг
dtm <- DocumentTermMatrix(docs)
inspect(dtm)
findAssocs(dtm, 'family', 0.2) # Смотрим, какие слова появляются в тексте вместо со словом family наиболее часто.
dtmw<-weightTfIdf(dtm)
dtms<-removeSparseTerms(dtmw, 0.98) 
inspect(dtms)
freqlist<-as.data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE)) # построение частотного списка
colnames(freqlist) <- 'freq'
freqlist$word <- rownames(freqlist)
freqlist$freq <- round(freqlist$freq, 0)
freqlist$word <- factor(freqlist$word, levels = freqlist$word[order(freqlist$freq, decreasing = FALSE)])
freqlist2 <- freqlist[1:25, ] #  Для визуализации выберем 25 самых частотных слов
library(ggplot2)
ggplot(freqlist2, aes(x=word, y=freq)) + 
geom_bar(stat = "identity", fill = ifelse(freqlist2$freq > 50, 'pink', 'grey')) + coord_flip()
library(wordcloud) # визуализируем облако слов
set.seed(123)
wordcloud(docs, max.words=120, random.order=F, rot.per=0.3, use.r.layout=F, colors=brewer.pal(8, 'Dark2'))
library(textmineR)
term_matrix <- CreateTcm(vec1, skipgram_window = Inf, stopword_vec = stopwords("en"), lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE, stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"))
library(igraph)
term_matrix <- as.matrix(term_matrix)
g1 <- graph_from_adjacency_matrix(term_matrix, mode = 'undirected', weighted = TRUE)
summary(g1)
g1 <- simplify(g1, remove.loops=TRUE, remove.multiple =TRUE)
g1 <- delete.edges(g1, E(g1)[E(g1)$weight < 8])
g1 <- delete.vertices(g1, degree(g1) < 1)
plot(g1, edge.curved = 0.5, vertex.color= 'red', vertex.size = 2.5, vertex.label.color="black", vertex.label.cex = 0.7, vertex.label.dist=0.8, edge.width = ifelse(E(g1)$weight > 15, 5, 1), layout=layout_with_fr)
term_matrix2 <- as_adjacency_matrix(g1)
library(wordspace)
plot(hclust(dist.matrix(term_matrix2, as.dist=TRUE)))

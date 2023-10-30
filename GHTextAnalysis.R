# База данных по 1000 фильмам и их оценкам на IMDB.
# Для примера текстового анализа выбрана переменная Overview - краткое описание сюжета фильма.

library(readr)
imdb_top_1000 <- read_csv("C:/Users/Nadezhda/Desktop/imdb_top_1000.csv")
vec1 <- imdb_top_1000$Overview
library(tm)
library(SnowballC) # Если понадобится стемминг
docs <- VCorpus(VectorSource(vec1)) # Создаем корпус
head(docs[[1]])
# Подготовительная работа с корпусом: удаляем числа, стоп-слова, приводим к нижнему регистру и т.д.
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument) # Если нужен стеминг
dtm <- DocumentTermMatrix(docs) # Создаем матрицу, несущую информацию о том, в каких документах и с какой частотой встречаются слова
inspect(dtm)
findAssocs(dtm, 'family', 0.2) # Смотрим, какие слова появляются в тексте вместо со словом family наиболее часто
freqlist<-as.data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE)) # Построение частотного списка
colnames(freqlist) <- 'freq'
freqlist$word <- rownames(freqlist)
freqlist$word <- factor(freqlist$word, levels = freqlist$word[order(freqlist$freq, decreasing = FALSE)])
freqlist2 <- freqlist[1:25, ] #  Для визуализации выберем 25 самых часто встречающихся слов
library(ggplot2)
ggplot(freqlist2, aes(x=word, y=freq)) + 
geom_bar(stat = "identity", fill = ifelse(freqlist2$freq > 50, 'pink', 'grey')) + coord_flip()
# https://github.com/DatExpN/DataAnalysisExample/blob/main/WordsFrequency.png

library(wordcloud) 
set.seed(123)
wordcloud(docs, max.words=120, random.order=F, rot.per=0.3, use.r.layout=F, colors=brewer.pal(8, 'Dark2')) # Визуализируем облако слов
# https://github.com/DatExpN/DataAnalysisExample/blob/main/Wordcloud.png

library(textmineR)
term_matrix <- CreateTcm(vec1, skipgram_window = Inf, stopword_vec = stopwords("en"), lower = TRUE, remove_punctuation = TRUE, 
remove_numbers = TRUE) # Если нужен стемминг, добаляем stem_lemma_function = function(x) SnowballC::wordStem(x, "porter")
library(igraph)
term_matrix <- as.matrix(term_matrix)
g1 <- graph_from_adjacency_matrix(term_matrix, mode = 'undirected', weighted = TRUE) # Создаем граф из матрицы
summary(g1) # В нашем графе 5456 вершин; для визуализации требуется уменьшить это число - оставить самые часто встречающиеся пары слов
g1 <- simplify(g1, remove.loops=TRUE, remove.multiple =TRUE)
g1 <- delete.edges(g1, E(g1)[E(g1)$weight < 8]) # Удаляем связи с весом менее 8, то есть те случаи, когда два слова встречаются вместе в менее чем 8 документах
g1 <- delete.vertices(g1, degree(g1) < 1) # Удаляем вершины, которые потеряли все связи на прошлом шаге (то есть те слова, которые встречаются с другими менее чем в 8 документах)
plot(g1, edge.curved = 0.5, vertex.color= 'red', vertex.size = 2.5, 
vertex.label.color="black", vertex.label.cex = 0.7, vertex.label.dist=0.8, edge.width = ifelse(E(g1)$weight > 15, 5, 1), 
layout=layout_with_fr) # Визуализируем полученный граф, толщина линии сооветствует случаям, когда два слова встречаются вместе в более чем 15 документах
# https://github.com/DatExpN/DataAnalysisExample/blob/main/NetworkWords.png

term_matrix2 <- as_adjacency_matrix(g1) # Извлекаем матрицу из объекта igraph для дальнейшей работы (после редактирования число вершин 54)
library(wordspace)
plot(hclust(dist.matrix(term_matrix2, as.dist=TRUE))) # Еще один способ визуализировать близость между словами - создать дендрограму
# https://github.com/DatExpN/DataAnalysisExample/blob/main/WordsDendrogram.png

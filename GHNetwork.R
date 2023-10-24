# Предобработка данных.
# Для создания графа мы будем использовать только информацию про актеров - переменные star1, Star2, Star3 и Star4. 
# Строим граф, где вершинами будут актеры, а связью - факт участия в съемках одних и тех же фильмов.


library(readr)
imdb_top_1000 <- read_csv("C:/Users/Nadezhda/Desktop/imdb_top_1000.csv")
df <- imdb_top_1000[, 11: 14]
library(igraph)
relations1 <- data.frame(from = df[, 1], to = df[, 2])
relations2 <- data.frame(from = df[, 1], to = df[, 3])
relations3 <- data.frame(from = df[, 1], to = df[, 4])
relations4 <- data.frame(from = df[, 2], to = df[, 3])
relations5 <- data.frame(from = df[, 2], to = df[, 4])
relations6 <- data.frame(from = df[, 3], to = df[, 4])
listdata <- lapply(list(relations1, relations2, relations3, relations4, relations5, relations6), 
                   function(x) {setNames(x, c('Star1', 'Star2'))})
gmain <- do.call(rbind, listdata)
g1 <- graph_from_data_frame(gmain, directed=FALSE)
summary(g1)
library(netUtils)


# Так как у нас нет изначально заданных атрибутов, для визуализации сделаем атрибуты при помощи информации о позиции 
# вершины в сети. Так как мы анализируем сеть актеров, нам может быть интересно какие актеры находятся в ядре индустрии, 
# а какие на периферии. Также нам может быть интересно, какие актеры имеют наибольшее число контактов в индустрии. 
# Для того, чтобы рассчитать позицию актера относительно ядра/периферии, нам нужно выделить в сети главный компонент.


c <- components(g1, mode = 'weak')
c$csize
g2 <- decompose(g1, mode = 'weak', min.vertices = 1724)
g2 <- g2[[1]]
g2 <- simplify(g2, remove.loops=TRUE, remove.multiple =TRUE)
degree_graph <- degree(g2, mode = 'all') # считаем число контактов каждого актера
g2 <- set_vertex_attr(g2, 'degree', v = degree_graph)
coreness <- core_periphery(g2, method = "rk1_dc")$vec # определяем позицию вершины относительно ядра/периферии
g2 <- set_vertex_attr(g2, 'core', v = coreness)
summary(g2)


# Визуализируем полученный граф с учетом информации о числе контактов и позиции относительно ядра/периферии. 
# Красным цветом обозначены вершины, попавшие в ядро, размер выделяет вершины с числом контактов больше 20.

plot(g2, edge.curved = 0.2, vertex.color= ifelse(V(g2)$core > 0, 'red', 'grey'), vertex.size = ifelse(V(g2)$degree > 20, 6, 2.5), layout=layout_with_fr, vertex.label = NA)
# https://github.com/DatExpN/DataAnalysisExample/blob/main/Graph.png


# Посмотрим, какие актеры имеют наибольшее число контактов в индустрии 
# (контакты основаны на числе фильмов, в которых они снимались вместе с другими актерами).

actors <- as.data.frame(cbind(V(g2)$name, V(g2)$degree))
colnames(actors) <- c('Actor', 'Number_of_contacts')
actors$Number_of_contacts <- as.numeric(actors$Number_of_contacts)
actors <- actors[order(actors$Number_of_contacts, decreasing = TRUE),]
actors2 <- actors[1:35, ]
library(ggplot2)
actors2$Actor <- factor(actors2$Actor, levels = actors2$Actor[order(actors2$Number_of_contacts, decreasing = FALSE)])
ggplot(actors2, aes(x=Actor, y=Number_of_contacts)) + 
geom_bar(stat = "identity", fill = ifelse(actors2$Number_of_contacts > 20, 'red', 'grey')) + coord_flip()
# https://github.com/DatExpN/DataAnalysisExample/blob/main/PopularActors.png

# Посмотрим, на какие сообщества (community) разделится наша сеть - в данном случае сообщество будет объединять актеров, 
# которые часто снимаются вместе. Попробуем два алгоритма - Louvain и Girvan-Newman.
set.seed(123)
love1 <- cluster_louvain(g2) # применяем алгоритм поиска сообществ Louvain
modularity(love1) # модулярность говорит о том, насколько хорошо сеть разделилась на сообщества
sizes(love1) # смотрим количество и размер полученных сообществ
plot(g2, vertex.color=membership(love1), edge.curved = 0.2, vertex.size=3.5, vertex.label = NA, 
     edge.color = "grey", layout=layout_with_fr) # визуализируем сеть с учетом информации о сообществах
love2 <- ifelse(love1$membership == 5 | love1$membership == 9 | love1$membership == 13 | love1$membership == 16, love1$membership, 0)
love2 <- replace(love2, love2 == 0, 1)
love2 <- replace(love2, love2 == 5, 2)
love2 <- replace(love2, love2 == 9, 3)
love2 <- replace(love2, love2 == 13, 4)
love2 <- replace(love2, love2 == 16, 5)
g2 <- set_vertex_attr(g2, 'comluv', v = love2)
colrs <- c('grey',"orange", "tomato", "green", 'blue')
plot(g2, vertex.color= colrs[as.factor(V(g2)$comluv)], edge.curved = 0.2, vertex.size=3.5, vertex.label = NA, 
     edge.color = "grey", layout=layout_with_fr) # визуализируем только большие сообщества (с числом вершин от 100)
# https://github.com/DatExpN/DataAnalysisExample/blob/main/GraphLouvain.png
set.seed(123)
com1 <- cluster_edge_betweenness(g2) # применяем алгоритм поиска сообществ Girvan-Newman
modularity(com1) # модулярность говорит о том, насколько хорошо сеть разделилась на сообщества. 
# В нашем случае модулярность первого и второго решения практически одинаковая (76-78%)
sizes(com1) # смотрим количество и размер полученных сообществ
plot(g2, vertex.color=membership(com1), edge.curved = 0.2, vertex.size=3.5, vertex.label = NA, edge.color = "grey", layout=layout_with_fr) # визуализируем сеть с учетом информации о комьюнити
com2 <- ifelse(com1$membership == 1 | com1$membership == 4 | com1$membership == 12 | com1$membership == 25, com1$membership, 0)
com2 <- replace(com2, com2 == 0, 1)
com2 <- replace(com2, com2 == 1, 2)
com2 <- replace(com2, com2 == 4, 3)
com2 <- replace(com2, com2 == 12, 4)
com2 <- replace(com2, com2 == 25, 5)
g2 <- set_vertex_attr(g2, 'comnew', v = com2)
colrs <- c('grey','red', 'blue','purple','pink')
plot(g2, vertex.color= colrs[as.factor(V(g2)$comnew)], edge.curved = 0.2, vertex.size=3.5, vertex.label = NA, edge.color = "grey", layout=layout_with_fr) # визуализируем только большие сообщества (с числом вершин от 70 и выше). 
# В данном случае берем такие границы деления, потому что этот алгоритм делит граф на большое число небольших сообществ.
# https://github.com/DatExpN/DataAnalysisExample/blob/main/GraphNewman.png

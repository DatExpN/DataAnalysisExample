# Предобработка данных.
# Для создания графа мы будем использовать только информацию про актеров - переменные star1, Star2, Star3 и Star4. Строим граф, где вершинами будут актеры, а связью - факт участия в съемках одних и тех же фильмов.
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
listdata <- lapply(list(relations1, relations2, relations3, relations4, relations5, relations6), function(x) {setNames(x, c('Star1', 'Star2'))})
gmain <- do.call(rbind, listdata)
g1 <- graph_from_data_frame(gmain, directed=FALSE)
summary(g1)
library(netUtils)
# Так как у нас нет изначально заданных атрибутов, для визуализации сделаем атрибуты при помощи информации о позиции вершины в сети. Так как мы анализируем сеть актеров, нам может быть интересно какие актеры находятся в ядре индустрии, а какие на периферии. Также нам может быть интересно, какие актеры имеют наибольшее число контактов в индустрии. Для того, чтобы рассчитать позицию актера относительно ядра/периферии, нам нужно выделить из сети главный компонент.
c <- components(g1, mode = 'weak')
c$csize
g2 <- decompose(g1, mode = 'weak', min.vertices = 1724)
g2 <- g2[[1]]
degree_graph <- degree(g2, mode = 'all') # считаем число контактов каждого актера
g2 <- set_vertex_attr(g2, 'degree', v = degree_graph)
coreness <- core_periphery(g2, method = "rk1_dc")$vec # определяем позицию вершины относительно ядра/периферии
g2 <- set_vertex_attr(g2, 'core', v = coreness)
summary(g2)
# Визуализируем полученный граф с учетом информации о числе контактов и позиции относительно ядра/периферии. Красным цветом обозначены вершины, попавшие в ядро, размер выделяет вершины с числом контактов больше 20.
plot(g2, edge.curved = 0.2, vertex.color= ifelse(V(g2)$core > 0, 'red', 'grey'), vertex.size = ifelse(V(g2)$degree > 20, 6, 2.5), layout=layout_with_fr, vertex.label = NA)
# Посмотрим, какие актеры имеют наибольшее число контактов в индустрии (контакты основаны на числе фильмов, в которых они снимались вместе с другими актерами).
actors <- as.data.frame(cbind(V(g2)$name, V(g2)$degree))
colnames(actors) <- c('Actor', 'Number_of_contacts')
actors$Number_of_contacts <- as.numeric(actors$Number_of_contacts)
actors <- actors[order(actors$Number_of_contacts, decreasing = TRUE),]
actors2 <- actors[1:35, ]
library(ggplot2)
actors2$Actor <- factor(actors2$Actor, levels = actors2$Actor[order(actors2$Number_of_contacts, decreasing = FALSE)])
ggplot(actors2, aes(x=Actor, y=Number_of_contacts)) + 
geom_bar(stat = "identity", fill = ifelse(actors2$Number_of_contacts > 20, 'red', 'grey')) + coord_flip()

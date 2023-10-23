# Предобработка данных. 
# Нужно перекодировать переменную Genre (так как у одного фильма одновременно может быть указано несколько жанров): 
# создадим новую переменную, которая будет нести информацию о первом (основном) из указанных жанров. 
# Наиболее часто появляющимися жанрами в этой базе являются Drama, Comedy, Action, Crime, 
# все остальные жанры объединим в категорию Other (объединяем в большую группу, так как присутствующее число наблюдений 
# по этим категориям не достаточно для анализа)
# Нужно перекодировать переменную Runtime, чтобы осталось только число минут для каждого фильма.
# Несколько переменных содержат пропущенные значения, заменим их на медианные значения.
# Для анализа нам также понадобятся новые переменные: выручка больше или равная медианному значению, 
# год релиза позже или равный медианному значению, оценка критиков больше или равная медианному значению, 
# оценка аудитории больше или равная медианному значению.
# Основными переменными для анализа в этой базе для нас будут: год релиза, длительность фильма, жанр, 
# рейтинг среди аудитории, рейтинг среди критиков и выручка.
library(readr)
imdb_top_1000 <- read_csv("C:/Users/Nadezhda/Desktop/imdb_top_1000.csv")
df <- imdb_top_1000[, c(3, 5:7, 9, 16)] 
library(stringr)
df[c('Time', 'Last')] <- str_split_fixed(df$Runtime, ' ', 2) 
df[c('Genre1', 'Genre2', 'Genre3')] <- str_split_fixed(df$Genre, ',', 3)
sapply(df, class) 
cols.num <- c('Released_Year', 'Time')
df[cols.num] <- sapply(df[cols.num], as.numeric)
names(which(colSums(is.na(df))>0)) 
df$Gross[is.na(df$Gross)]<- median(df$Gross,na.rm = TRUE)
df$Meta_score[is.na(df$Meta_score)]<- median(df$Meta_score,na.rm = TRUE)
df$Released_Year[is.na(df$Released_Year)]<- median(df$Released_Year,na.rm = TRUE)
df$Revenue <- with(df, ifelse(Gross >= 23530892, 1, 0))
df$Rating <- with(df, ifelse(IMDB_Rating >= 7.9, 1, 0))
df$Year <- with(df, ifelse(Released_Year >= 1999, 1, 0))
df$Critics <- with(df, ifelse(Meta_score >= 79, 1, 0))
df$MainGenre <- with(df, ifelse(Genre1 == 'Drama' | Genre1 == 'Comedy' | Genre1 == 'Action' | Genre1 == 'Crime', Genre1, 'Other'))
cols.char <- c('Genre1', 'Genre2', 'Genre3', 'MainGenre', 'Revenue', 'Rating', 'Year', 'Critics')
df[cols.char] <- lapply(df[cols.char], factor)
sapply(df, class)
df <- df[, c(1, 4:7, 12:16)]
# Далее мы будем решать задачу классификации фильмов, представленных в этой базе. 
# Эту задачу мы будем решать при помощи кластерного анализа. Однако до перехода к кластерному анализу, 
# посмотрим при помощи визуального анализа на какие группы потенциально могут быть разделены фильмы. 
# Для решения этой задачи по визуализации воспользуемся алгоритмом многомерного шкалирования. 
# Многомерное шкалирование хорошо подходит для задач визуального отображения близости между объектами, 
# помещенными в двумерное пространство. Для такой визуализации возьмем три переменных из нашей базы - рейтинг критиков, 
# рейтинг аудитории и доход. Одной из базовых гипотез может быть предположение о том, что наша выборка в 1000 фильмов 
# разделится на группу, включающие фильмы с высоким рейтингом и большой выручкой и группу, объединяющую фильмы 
# с низким рейтингом и маленькой выручкой.
df_stand <- as.data.frame(scale(df[, 2:4])) # переменные измерены в разных шкалах, требуется стандартизация
library(vegan)
nmds_result <- metaMDS(df_stand, distance = 'euclidean', k = 2)
nmds_result$stress # показатель говорит о том, насколько хорошо двумерное пространство подходит для отображения наших данных,
# то есть с какой точностью сработал алгоритм для наших данных 
# (stress < 0.2 означает, что решение на два измерения хорошо подходит)
data_scores <- as.data.frame(scores(nmds_result))
data_scores <- cbind(data_scores, df[, 6:10])
# визуализируем наши данные при помощи координат, которые мы получили 
library(ggplot2)
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Rating), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Audience Rating", labels = c('lower than 7.9', '7.9 or higher'))
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Critics), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Critics Rating", labels = c('lower than 79', '79 or higher'))
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot1.png
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot2.png
# Как мы и предполагали, фильмы разделяются по рейтингу: в нашем случае горизонтальное измерение может быть описано 
# при помощи информации об оценках аудитории, а вертикальное измерение через информацию об оценках критиков.
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Year), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Year of release", labels = c('earlier than 1999', '1999 or later'))
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = MainGenre), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2)))
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot3.png
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot4.png
# Как мы видим, в случае жанров или года релиза, визуализация не дает нам однозначно предположить, фильмы какого жанра 
# (или какого года релиза) будут выше оценены критиками или аудиторией.
# Перейдем к задаче кластеризации. Для кластеризации мы будем использовать следующие переменные: длительность, 
# год релиза, жанр, выручку, рейтинг аудитории и рейтинг критиков. Так как у нас и числовые, и категориальные переменные, 
# используем алгоритм кластеризации PAM (gower distance).
library(factoextra)
library(cluster)
library(fpc)
df_clust <- df[, 1:5]
df_clust <- as.data.frame(scale(df_clust))   
df_clust <- cbind(df_clust, df[, 10])
gower_dist <- daisy(df_clust, metric = "gower")
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width) # оптимальное решение - 5 кластеров
set.seed(123)
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit$clusinfo
dd <- cbind(df_clust, cluster = pam_fit$clustering)
dd$cluster <- as.factor(dd$cluster)
sapply(dd, class)
fviz_silhouette(pam_fit, palette = "jco", ggtheme = theme_classic()) # насколько хорошо 5 кластеров описывают наши данные
km_stats <- cluster.stats(gower_dist, pam_fit$clustering)
km_stats$dunn 
dd <- cbind(dd, df[, 6:9])
# Сделаем несколько изображений, чтобы более подробно описать полученные кластеры с точки зрения жанров фильмов, 
# года релиза, выручки и рейтингов. Как мы видим, жанр стал основной переменной, разделивший кластеры.
library("gridExtra")
p1 <- ggplot(dd, aes(MainGenre, fill = Revenue)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), labels = c('lower 23530892', '23530892 or higher')) + 
theme(axis.title.x = element_blank(),
axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
p2 <- ggplot(dd, aes(MainGenre, fill = Rating)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), name = 'Audience Rating', labels = c('lower 7.9', '7.9 or higher')) + 
theme(axis.title.x = element_blank(),
       axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
p3 <- ggplot(dd, aes(MainGenre, fill = Year)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), name = 'Year of release', labels = c('before 1999', '1999 or later')) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
p4 <- ggplot(dd, aes(MainGenre, fill = Critics)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), name = 'Critics Rating', labels = c('lower 79', '79 or higher')) + 
theme(axis.title.x = element_blank(),
      axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
grid.arrange(p1, p2, p3, p4)
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot5.png
# Далее мы будем решать задачу предсказания высокой выручки фильмов. Здесь для нас высокая выручка - 
# равная или больше медианного значения 23530892. Для начала построим модель логистической регрессии.
df_logit <- df[, c(1:3, 5:6, 10)]
levels(df_logit$MainGenre) <- c('Other', 'Comedy', 'Action', 'Crime', 'Drama')
model_reg <- glm(Revenue ~ ., data = df_logit, family = binomial)
pR2 = 1 - model_reg$deviance / model_reg$null.deviance
pR2
library(DescTools)
PseudoR2(model_reg, c('McFadden', 'Nagel'))
coef <- as.data.frame(exp(coef(model_reg))/(1+exp(coef(model_reg))))
coef
library(performance)
check_collinearity(model_reg)
library(sjPlot)
plot_model(model_reg, vline.color = 'red')
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot6.png
summary(model_reg) # Значимыми для предсказания высокой выручки является переменная длительность фильма и 
# переменная жанр фильма: фильмы с большей длительностью и жанром Other (так как мы перекодировали переменную жанр, 
# в этом случае значит, что жанр должен быть не драма, комедия, экшен или криминал) с наибольшей вероятностью соберут 
# большую кассу. 
# Далее для решения задачи предсказания большой выручки попробуем алгоритм дерева решений.
library(rpart)
df_tree <- df[, c(1:3, 5:6, 10)]
# разделяем выборку на обучающую и тестовую
set.seed(456)
train.index <- sample(c(1:dim(df_tree)[1]), dim(df_tree)[1]*0.5)
train.df <- df_tree[train.index, ]
valid.df <- df_tree[-train.index, ]
model <- rpart(Revenue ~ ., data = train.df, method = 'class', control = rpart.control(minsplit = 7, minbucket = 10, maxdepth = 10))
library(rpart.plot)
library(RColorBrewer)
library(rattle)
rpart.plot(model, type=2, extra = 1)
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot7.png
# Как и в случае модели логистической регрессии, здесь мы видим, что жанр является важной переменной для предсказания 
# высокой выручки фильма. Следующая развилка - это год релиза фильма: если релиз был в 1993 или позже, тогда для нас 
# важен рейтинг зрителей; если ранее 1993, тогда следующая развилка - это год релиза ранее 1961 и затем рейтинг критиков. 
# Интересное наблюдение, что если фильм недавнего производства (1993 и позже), тогда для предсказания высокой выручки 
# важен рейтинг аудитории, но для более поздних фильмов (до 1961) важен рейтинг критиков. 
# Однако мы должны с осторожностью интерпретировать эти результаты, потому что данная модель дает верное предсказание 
# только в 61% случаев.
predicted_measure1 <- predict(model, train.df[ , -5], type = 'class')
table_mat1 <- table(train.df$Revenue, predicted_measure1)
table_mat1
predicted_measure2 <- predict(model, valid.df[ , -5], type = 'class')
table_mat2 <- table(valid.df$Revenue, predicted_measure2)
table_mat2
accuracy_Test <- sum(diag(table_mat2)) / sum(table_mat2)
print(paste('Accuracy for test', accuracy_Test))
# Чтобы улучшить модель, мы используем алгоритм Random Forest.
library(randomForest)
set.seed(3217)
ntree.1 <- 45 # пробуем решения для разного числа деревьев, останавливаемся на том решении, 
# где число ошибок распознавания не меняется.
nodesize.1 <-1
keep.forest.1 <- TRUE
modelRandomForest <- randomForest(Revenue ~., data = train.df, ntree=ntree.1, mtry=floor(sqrt(ncol(train.df))), 
                                  replace=FALSE, nodesize = nodesize.1, importance=TRUE, localImp=FALSE, proximity=FALSE, 
                                  norm.votes=TRUE, do.trace=ntree.1/10, keep.forest=keep.forest.1, corr.bias=FALSE, 
                                  keep.inbag=FALSE) 
tree_predicted1 <- predict(modelRandomForest, newdata = train.df[, -5])
tree_predicted2 <- predict(modelRandomForest, newdata = valid.df[, -5])
tablerf1 <- table(train.df$Revenue, tree_predicted1)
tablerf2 <- table(valid.df$Revenue, tree_predicted2)
tablerf1
tablerf2 # смотрим улучшилась ли работа алгоритма по количеству верно распознанных наблюдений
varImpPlot(modelRandomForest, sort=F) # можем увидеть какие переменные наиболее важны для классификатора
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RPlot8.png

# База данных по 1000 фильмам и их оценкам на IMDB.
# Предобработка данных. 
# Основными переменными для анализа в этой базе будут: год релиза (Released_Year), длительность фильма (Runtime), 
# жанр (Genre), рейтинг среди аудитории (IMDB_Rating), рейтинг среди критиков (Meta_score) и выручка (Gross).
# Во-первых, нужно перекодировать несколько переменных: Genre и Runtime.
# Переменная Genre несет информацию о всех указанных жанрах фильма (может быть указано больше одного).
# Создадим новую переменную, которая будет нести информацию о первом (основном) из указанных жанров.
# Затем объединим редко встречающиеся жанры в категорию Other (объединяем, так как числа наблюдений не достаточно для анализа).
# Переменная Runtime содержит подпись min, нужно оставить только число минут для каждого фильма.
# Во-вторых, несколько переменных содержат пропущенные значения, заменим их на медианные значения.
# В-третьих, для анализа понадобятся новые переменные: выручка больше или равная медианному значению, 
# год релиза позже или равный медианному значению, оценка критиков больше или равная медианному значению, 
# оценка аудитории больше или равная медианному значению.
# В-четвертых, нужно обозначить тип переменных, перекодировав часть переменных в факторы.

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

# Кластерный анализ будет использован для задачи объединения фильмов со схожими характеристиками в одну группу.
# Однако до перехода к кластерному анализу при помощи визуализации результатов работы алгоритма многомерного шкалирования
# визуально также можно увидеть, на какие группы разделятся наблюдения.
# Многомерное шкалирование хорошо подходит для задач визуального отображения близости между объектами, 
# помещенными в двумерное пространство. Для такой визуализации возьмем три переменных из базы - рейтинг критиков, 
# рейтинг аудитории и доход. Одной из базовых гипотез может быть предположение о том, что выборка в 1000 фильмов 
# разделится на группу, объединяющую фильмы с высоким рейтингом и большой выручкой и группу, объединяющую фильмы 
# с низким рейтингом и маленькой выручкой.

df_stand <- as.data.frame(scale(df[, 2:4])) # переменные измерены в разных шкалах, требуется стандартизация
library(vegan)
nmds_result <- metaMDS(df_stand, distance = 'euclidean', k = 2)
nmds_result$stress # показатель говорит о том, насколько хорошо двумерное пространство подходит для отображения наших данных,
# то есть с какой точностью сработал алгоритм для наших данных 
# (stress < 0.2 означает, что решение выбрать два измерения хорошо подходит)
data_scores <- as.data.frame(scores(nmds_result))
data_scores <- cbind(data_scores, df[, 6:10])

# Визуализируем наши данные при помощи координат, которые мы получили.

library(ggplot2)
ggplot() + 
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Rating), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Audience Rating", labels = c('lower than 7.9', '7.9 or higher'))
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Critics), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Critics Rating", labels = c('lower than 79', '79 or higher'))
# https://github.com/DatExpN/DataAnalysisExample/blob/main/NMDSAudienceRating.png
# https://github.com/DatExpN/DataAnalysisExample/blob/main/NMDSCriticsRating.png

# Как и предполагалось, фильмы разделяются по рейтингу: горизонтальное измерение может быть описано 
# при помощи информации об оценках аудитории, а вертикальное измерение через информацию об оценках критиков.

ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = Year), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2))) +
scale_color_discrete(name = "Year of release", labels = c('earlier than 1999', '1999 or later'))
ggplot() +
geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = MainGenre), size = 2.5, alpha = 0.6) +
annotate(geom = 'label', x = -1, y = 5.5, size = 9.5, label = paste('Stress: ', round(nmds_result$stress, digits = 2)))
# https://github.com/DatExpN/DataAnalysisExample/blob/main/NMDSYear.png
# https://github.com/DatExpN/DataAnalysisExample/blob/main/NMDSGEnre.png
# Как мы видим, в случае жанров или года релиза, визуализация не дает однозначно предположить, фильмы какого жанра 
# (или какого года релиза) будут выше оценены критиками или аудиторией. 
# Визуализация может служить полезным инструментом только на предварительных этапах анализа, для проверки гипотезы о разделении
# наблюдений на группы требуется воспользоваться алгоритмом кластеризации.

# Перейдем к задаче кластеризации. Для кластеризации будут использоваться следующие переменные: длительность фильма, 
# год релиза, жанр, выручка, рейтинг аудитории и рейтинг критиков. Так как присутствуют числовые и категориальная переменные, 
# используем алгоритм кластеризации PAM, Gower distance.

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
lines(1:10, sil_width) # График говорит какое число кластеров является оптимальным (в этом случае 5 кластеров).
set.seed(123)
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit$clusinfo
dd <- cbind(df_clust, cluster = pam_fit$clustering)
dd$cluster <- as.factor(dd$cluster)
sapply(dd, class)
fviz_silhouette(pam_fit, palette = "jco", ggtheme = theme_classic()) # График показывает насколько хорошо данные кластеризуются.
# По графику мы смотрим каково среднее значение silhouette coefficient для каждого кластера (1- это максимальное значение при идеальной кластеризации).
# https://github.com/DatExpN/DataAnalysisExample/blob/main/ClustersSilhouettePlot.png
km_stats <- cluster.stats(gower_dist, pam_fit$clustering)
km_stats$dunn # Этот индекс также позволяет нам оценить, насколько хорошо данные кластеризуются. 
# Чем выше значение этого индекса, тем лучше, так как в идеальной ситуации мы хотим получить плотные, 
# четко раздлеленные кластеры (диаметр кластеров небольшой, дистанция между кластерами большая).
# В нашем случае Dunn index имеет значение 0.39.

# Сделаем несколько изображений, чтобы более подробно описать полученные кластеры с точки зрения жанров фильмов, 
# года релиза, выручки и рейтингов. Жанр стал основной переменной, разделивший кластеры.

dd <- cbind(dd, df[, 6:9])
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
theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
p3 <- ggplot(dd, aes(MainGenre, fill = Year)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), name = 'Year of release', labels = c('before 1999', '1999 or later')) + 
theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
p4 <- ggplot(dd, aes(MainGenre, fill = Critics)) +
geom_bar() +
scale_fill_manual(values = c("grey", "purple"), name = 'Critics Rating', labels = c('lower 79', '79 or higher')) + 
theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
geom_text(aes(label= ..count..), stat = "count", position = position_stack(vjust = 0.5))
grid.arrange(p1, p2, p3, p4)
# https://github.com/DatExpN/DataAnalysisExample/blob/main/ClusteringResult.png

# Далее мы будем решать задачу предсказания высокой выручки фильмов. Высокая выручка - равная или больше медианного 
# значения 23530892. Для начала построим модель логистической регрессии.

df_logit <- df[, c(1:3, 5:6, 10)]
levels(df_logit$MainGenre) <- c('Other', 'Comedy', 'Action', 'Crime', 'Drama')
ggplot(df_logit, aes(MainGenre, fill = Revenue)) +
geom_bar() +
coord_flip() # при помощи визуализации посмотрим фильмы каких жанров приносят большую выручку
# Так как мы решаем задачу предсказания, разделим выборку на обучающую и тестовую.
set.seed(123)
train.index <- sample(c(1:dim(df_logit)[1]), dim(df_logit)[1]*0.5)
train.df <- df_logit[train.index, ]
valid.df <- df_logit[-train.index, ]
model_reg <- glm(Revenue ~ ., data = train.df, family = binomial)
library(DescTools)
PseudoR2(model_reg, c('McFadden', 'Nagel')) # считаем Pseudo R2 для модели
library(performance)
check_collinearity(model_reg) # проверка на мультиколлинеарность
coef <- as.data.frame(exp(coef(model_reg))/(1+exp(coef(model_reg)))) # перевели коэффициенты в более удобную форму для интерпретации
coef 
library(sjPlot)
plot_model(model_reg, vline.color = 'red') # Удобная визуальная форма представления коэффициентов
# https://github.com/DatExpN/DataAnalysisExample/blob/main/OddsRatios.png
summary(model_reg) # Значимыми для предсказания высокой выручки является переменная длительность фильма и переменная 
# жанр фильма: фильмы с большей длительностью и жанром Other с наибольшей вероятностью соберут большую кассу. 
# Так как мы перекодировали переменную жанр, в этом случае значит, что жанр должен быть не комедия, экшен или криминал: 
# изменение жанра с Other на комедию, экшен или криминал уменьшает вероятность получения высокой выручки.
# Используя тестовую выборку посмотрим, насколько хорошо наша модель предсказывает высокую выручку.
pred_logit <- predict(model_reg, valid.df[, -5], type = 'response')
r_pred = rep(0, dim(valid.df)[1])
r_pred[pred1 > .5] = 1
table_logit <- table(valid.df$Revenue, r_pred)
table_logit # Для тестовой выборки 76 значений верно распознаны как фильмы с небольшой выручкой, 210 значений верно 
# распознаны как фильмы с большой выручкой.
mean(r_pred == valid.df$Revenue) # Смотрим процент верно предсказанных значений, он не очень большой - всего 57%.
library(ROCR)
pred <- prediction(pred1, valid.df$Revenue)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE) # Еще один способ посмотреть на точность классификатора - визуализировать ROC кривую и 
# посчитать значение AUC. Полученная визуализация опять же говорит нам о том, что классификатор работает плохо.
unlist(slot(performance(pred, "auc"), "y.values"))
# https://github.com/DatExpN/DataAnalysisExample/blob/main/ROCCurve.png

# Далее для решения задачи предсказания большой выручки попробуем алгоритм дерева решений.

library(rpart)
df_tree <- df[, c(1:3, 5:6, 10)]
# Так как мы решаем задачу предсказания, здесь также понадобится тестовая и обучающая выборки.
set.seed(456)
train.index <- sample(c(1:dim(df_tree)[1]), dim(df_tree)[1]*0.5) 
train.df <- df_tree[train.index, ]
valid.df <- df_tree[-train.index, ]
model <- rpart(Revenue ~ ., data = train.df, method = 'class', control = rpart.control(minsplit = 7, minbucket = 10, maxdepth = 10))
library(rpart.plot)
library(RColorBrewer)
library(rattle)
rpart.plot(model, type=2, extra = 1) # визуализируем дерево решений
# https://github.com/DatExpN/DataAnalysisExample/blob/main/DecisionTree.png
# Как и в случае модели логистической регрессии, здесь мы видим, что жанр является важной переменной для предсказания высокой выручки фильма. 
# Следующая развилка - это год релиза фильма: если релиз был в 1993 или позже, тогда для нас важен рейтинг зрителей; если ранее 1993, 
# тогда следующая развилка - это год релиза ранее 1961 и затем рейтинг критиков. Интересное наблюдение, что если фильм был произведен недавно (год релиза 1993 и позже), 
# тогда для предсказания высокой выручки важен рейтинг аудитории, но для более поздних фильмов (до 1961) важен рейтинг критиков.
# Нужно посмотреть в каком проценте случаев модель делает верное предсказание.
predicted_measure2 <- predict(model, valid.df[ , -5], type = 'class') 
table_mat2 <- table(valid.df$Revenue, predicted_measure2)
table_mat2 # Смотрим количество верно предсказанных значений для тестовой выборки: верно распознано 92 фильма как 
# фильмы с небольшой выручкой, верно распознано 213 фильмов как фильмы с большой выручкой
accuracy_Test <- sum(diag(table_mat2)) / sum(table_mat2)
print(paste('Accuracy for test', accuracy_Test)) # 61% верно классифицированных фильмов

# Чтобы улучшить модель, попробуем использовать алгоритм Random Forest.
library(randomForest)
set.seed(456)
ntree.1 <- 50 # пробуем решения для разного числа деревьев, останавливаемся на том решении, где число ошибок распознавания не меняется
nodesize.1 <-1
keep.forest.1 <- TRUE
modelRandomForest <- randomForest(Revenue ~., data = train.df, ntree=ntree.1, mtry=floor(sqrt(ncol(train.df))), replace=FALSE, 
nodesize = nodesize.1, importance=TRUE, localImp=FALSE, proximity=FALSE, norm.votes=TRUE, do.trace=ntree.1/10, 
keep.forest=keep.forest.1, corr.bias=FALSE, keep.inbag=FALSE) 
tree_predicted2 <- predict(modelRandomForest, newdata = valid.df[, -5])
tablerf2 <- table(valid.df$Revenue, tree_predicted2)
tablerf2 # смотрим улучшилась ли работа алгоритма по количеству верно распознанных наблюдений
accuracy_Test2 <- sum(diag(tablerf2)) / sum(tablerf2)
print(paste('Accuracy for test', accuracy_Test2))
varImpPlot(modelRandomForest, sort=F) # можем увидеть какие переменные наиболее важны для классификатора
# https://github.com/DatExpN/DataAnalysisExample/blob/main/RFVariables.png

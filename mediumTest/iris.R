rm(list=ls())
library(data.table) ##載入
library(factoextra)
library(dplyr) ##載入
head(iris)
data <- iris[, -5] # 因為Species是第五欄位，故移除掉
head(data)         # 現在data只剩下前四個欄位的資料
iris_1 = iris
summary(iris_1$Species)

# 分成三群
kmeans.cluster <- kmeans(data, centers=2) 

# 群內的變異數
kmeans.cluster$withinss
# 分群結果和實際結果比較
table(kmeans.cluster$cluster, iris$Species)  
# 視覺化 k-means 分群結果(基於ggplot2的語法)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = data,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態


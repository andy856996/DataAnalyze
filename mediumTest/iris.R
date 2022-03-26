library(factoextra)
head(iris)
data <- iris[, -5] # 因為Species是第五欄位，故移除掉
head(data)         # 現在data只剩下前四個欄位的資料
E.dist <- dist(data, method="euclidean") # 歐式距離
M.dist <- dist(data, method="manhattan") # 曼哈頓距離
par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化

# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")

# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")

# 分成三群
kmeans.cluster <- kmeans(data, centers=3) 

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
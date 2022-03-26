rm(list=ls())
#品種 酒精 蘋果酸 灰分 灰分的鹼度 鎂 總酚 黃酮 非黃酮酚 原花青素 顏色強度 色相 稀釋葡萄酒的 OD280/OD315 脯氨酸  
#https://www.jaredlander.com/data/wine.csv
data_wine <- read.csv("https://www.jaredlander.com/data/wine.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
head(data_wine)
data_wine_Wo_Cultivar	 <- data_wine[, -1]

# 分成三群
kmeans.cluster <- kmeans(data_wine_Wo_Cultivar, centers=2) 

# 群內的變異數
kmeans.cluster$withinss
# 分群結果和實際結果比較
table(kmeans.cluster$cluster, data_wine$Cultivar)  
# 視覺化 k-means 分群結果(基於ggplot2的語法)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = data_wine,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態

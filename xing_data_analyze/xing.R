data <- read.csv("C:/Users/andy8/Desktop/DataAnalyze/xing_data_analyze/preview_data.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
data_mod <- subset(data,(item2 == "臺東縣" | item2 == "臺中市")  & item1 == "111年 2月")

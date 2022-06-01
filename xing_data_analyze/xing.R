data <- read.csv("C:/Users/andy8/Desktop/DataAnalyze/xing_data_analyze/preview_data.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
data<-data.frame(data)
#打我啊打我啊打我啊打我啊打我啊
#data_mod <- subset(data,(item2 == "臺東縣" | item2 == "臺中市")  & item1 == "111年 2月")
data$value1 <- as.numeric(data$value1)
data$value2 <- as.numeric(data$value2)
data$value3 <- as.numeric(data$value3)
data$value4 <- as.numeric(data$value4)
data$value5 <- as.numeric(data$value5)
data$value6 <- as.numeric(data$value6)
data$value7 <- as.numeric(data$value7)

dataSixCity <- subset(data,item2 == "	桃園市" | item2 == "臺中市"  
                      | item2 == "高雄市" | item2 == "新北市" | item2 == "臺南市" | item2 == "臺北市")
SixCityRadiusLessThan2_5 <- subset(dataSixCity,value2 > 10)

class(dataSixCity$value1)
class(5)

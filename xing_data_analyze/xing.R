data <- read.csv("C:/Users/andy8/Desktop/DataAnalyze/xing_data_analyze/preview_data.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
data<-data.frame(data)
#data_mod <- subset(data,(item2 == "臺東縣" | item2 == "臺中市")  & item1 == "111年 2月")
dataSixCity <- subset(data,item2 == "	桃園市" | item2 == "臺中市"  
                      | item2 == "高雄市" | item2 == "新北市" | item2 == "臺南市" | item2 == "臺北市")
dataSixCity$value1 <- as.numeric(dataSixCity$value1)
dataSixCity$value2 <- as.numeric(dataSixCity$value2)
dataSixCity$value3 <- as.numeric(dataSixCity$value3)
dataSixCity$value4 <- as.numeric(dataSixCity$value4)
dataSixCity$value5 <- as.numeric(dataSixCity$value5)
dataSixCity$value6 <- as.numeric(dataSixCity$value6)
dataSixCity$value7 <- as.numeric(dataSixCity$value7)

SixCityRadiusLessThan2_5 <- subset(dataSixCity,value2 > 10)

class(dataSixCity$value1)
class(5)

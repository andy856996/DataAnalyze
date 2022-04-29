rm(list=ls())
library(neuralnet)
#品種 酒精 蘋果酸 灰分 灰分的鹼度 鎂 總酚 黃酮 非黃酮酚 原花青素 顏色強度 色相 稀釋葡萄酒的 OD280/OD315 脯氨酸  
#https://www.jaredlander.com/data/wine.csv
data_wine <- read.csv("https://www.jaredlander.com/data/wine.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
head(data_wine)


#分類資料 分成TEST 和 train 
first_data = data_wine[1:59,]
second_data = data_wine[60:130,]
third_data = data_wine[131:178,]
#(七成訓練三成預測)
f_size = round(0.7 * nrow(first_data))#first
s_size = round(0.7 * nrow(second_data))#second
t_size = round(0.7 * nrow(third_data)) #third

train_data <- rbind(first_data[1:f_size,], second_data[1:s_size,],third_data[1:t_size,])
test_data  <- rbind(first_data[(f_size+1):nrow(first_data),], second_data[(s_size+1):nrow(second_data),],third_data[(t_size+1):nrow(third_data),])
#整理資料
train_data$first <- ifelse(train_data$Cultivar == 1, 1, 0)
train_data$second <- ifelse(train_data$Cultivar == 2, 1, 0)
train_data$third <- ifelse(train_data$Cultivar == 3, 1, 0)

test_data$first <- ifelse(test_data$Cultivar == 1, 1, 0)
test_data$second <- ifelse(test_data$Cultivar == 2, 1, 0)
test_data$third <- ifelse(test_data$Cultivar == 3, 1, 0)

#訓練模型
f1 <- as.formula('first + second + third  ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline')
bpn <- neuralnet(formula = f1, data = train_data, hidden = c(5,5),learningrate = 0.01)
predicted_data <- compute(bpn,test_data)
predictions <- predicted_data$net.result


AnsOutPut <-  rep(0,length(1:nrow(predictions)))
ans = 0
for(i in c(1:nrow(predictions))){ # for-loop裡，i會依序帶入1~135的值，重複進行括號內的程式碼
  if(predictions[i,1] > predictions[i,2]){
    ans = 1
  }else{
    ans = 2
  }
  if(predictions[i,2] > predictions[i,3]){
    ans = 2
  }else{
    ans = 3
  }
  if(predictions[i,1] > predictions[i,3]){
    ans = 1
  }else{
    ans = 3
  }
  AnsOutPut[i] = ans
}




error <- data.frame(test_data$Cultivar,AnsOutPut)
library(ggplot2)
ggplot(error,aes(x=test_data$Cultivar,y=AnsOutPut)) + geom_point() + stat_smooth()


print(bpn)

#圖解BP
plot(bpn)

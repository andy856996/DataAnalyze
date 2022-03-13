# ---------
#  21-3
# ---------
# ---------------------------------------------------------- #

library(boot)
# 用glm重新對house1建立模型,而不再用lm
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro,
               data=housing, family=gaussian(link="identity"))

# 確保它跟lm的結果是一樣的
identical(coef(house1), coef(houseG1))

# 執行5折(群)的交叉驗證
houseCV1 <- cv.glm(housing, houseG1, K=5)

# 檢視誤差
houseCV1$delta

# ---------------------------------------------------------- #

# 用glm重新建立模型
houseG2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
houseG3 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + Class,
               data=housing)
houseG4 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class,
               data=housing)
houseG5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing)

# 執行交叉驗證
houseCV2 <- cv.glm(housing, houseG2, K=5)
houseCV3 <- cv.glm(housing, houseG3, K=5)
houseCV4 <- cv.glm(housing, houseG4, K=5)
houseCV5 <- cv.glm(housing, houseG5, K=5)

## 檢視誤差結果
# 給結果建立一個data.frame
cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta,
                                 houseCV3$delta, houseCV4$delta,
                                 houseCV5$delta))

## 進行一些處理以更好地呈現結果
# 替直排取更好的名稱
names(cvResults) <- c("Error", "Adjusted.Error")

# 加入模型名稱
cvResults$Model <- sprintf("houseG%s", 1:5)

# 檢視結果
cvResults

# ---------------------------------------------------------- #

# 視覺化結果
# 用ANOVA檢定
cvANOVA <-anova(houseG1, houseG2, houseG3, houseG4, houseG5)
cvResults$ANOVA <- cvANOVA$`Resid. Dev`

# 用AIC測量
cvResults$AIC <- AIC(houseG1, houseG2, houseG3, houseG4, houseG5)$AIC

# 處理data.frame以便繪圖
library(reshape2)
cvMelt <- melt(cvResults, id.vars="Model", variable.name="Measure",
               value.name="Value")
cvMelt

ggplot(cvMelt, aes(x=Model, y=Value)) +
       geom_line(aes(group=Measure, color=Measure)) +
       facet_wrap(~Measure, scales="free_y") +
       theme(axis.text.x=element_text(angle=90, vjust=.5)) +
       guides(color=FALSE)

# ---------------------------------------------------------- #

cv.work <- function(fun, k = 5, data,
                    cost = function(y, yhat) mean((y - yhat)^2),
                    response="y", ...)
   {
      # 生成資料群
      folds <- data.frame(Fold=sample(rep(x=1:k, length.out=nrow(data))),
                          Row=1:nrow(data))

      # 讓誤差的初始值為0
      error <- 0

      ## 對每群資料進行迭代
      ## 其中對每一群資料:
      ## 用訓練資料(training data)建立模型
      ## 用測試資料(testing data)進行預測
      ## 計算誤差並把它累積起來
      for(f in 1:max(folds$Fold))
      {
         # 抽取測試資料的橫排索引
         theRows <- folds$Row[folds$Fold == f]

         ## 對data[-theRows, ]應用fun函數
         ## 對data[theRows, ]做預測
         mod <- fun(data=data[-theRows, ], ...)
         pred <- predict(mod, data[theRows, ])

         # 累積誤差,並以群中的橫排個數當權重
         error <- error +
         cost(data[theRows, response], pred) *
         (length(theRows)/nrow(data))
      }

   return(error)
   
   }

# ---------------------------------------------------------- #

cv1 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt + Boro)
cv2 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units * SqFt + Boro)
cv3 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + Class)
cv4 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class)
cv5 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Boro + Class)

cvResults <- data.frame(Model=sprintf("house%s", 1:5),
                        Error=c(cv1, cv2, cv3, cv4, cv5))
cvResults

# ---------------------------------------------------------- #
# ---------
#  23-5
# ---------
# ---------------------------------------------------------- #

library(useful)
# 形容模型的formula(公式)
# 因為是建立樹模型，我們不需要截矩項
creditFormula <- Credit ~ CreditHistory + Purpose + Employment +
     Duration + Age + CreditAmount - 1

#因為是建立樹模型，我們使用所有level的類別變數
creditX <- build.x(creditFormula, data=credit, contrasts=FALSE)
creditY <- build.y(creditFormula, data=credit)

# 將logical vector(邏輯向量)轉換為[0,1]
creditY <- as.integer(relevel(creditY, ref='Bad')) - 1
      
# ---------------------------------------------------------- #

library(xgboost)
creditBoost <- xgboost(data=creditX, label=creditY, max.depth=3,
                         eta=.3, nthread=4, nrounds=3,
                         objective="binary:logistic")


# ---------------------------------------------------------- #

creditBoost20 <- xgboost(data=creditX, label=creditY, max.depth=3,
                           eta=.3, nthread=4, nrounds=20,
                           objective="binary:logistic")

# ---------------------------------------------------------- #

xgb.plot.multi.trees(creditBoost, feature_names=colnames(creditX))

# ---------------------------------------------------------- #

xgb.plot.importance(xgb.importance(creditBoost,
                                     feature_names=colnames(creditX)))

# ---------------------------------------------------------- #
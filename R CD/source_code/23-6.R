# ---------
#  23-6
# ---------
# ---------------------------------------------------------- #

library(randomForest)
creditFormula <- Credit ~ CreditHistory + Purpose + Employment +
  Duration + Age + CreditAmount - 1

# 因為是樹的建立，我們使用類別變數的所有level
creditX <- build.x(creditFormula, data=credit, contrasts=FALSE)
creditY <- build.y(creditFormula, data=credit)

# 建立隨機森林
creditForest <- randomForest(x=creditX, y=creditY)
creditForest

# ---------------------------------------------------------- #

#建立反應變數matrix(矩陣)
creditY2 <- as.integer(relevel(creditY, ref='Bad')) - 1

# 建立隨機森林
boostedForest <- xgboost(data=creditX, label=creditY2, max_depth=4,
                         num_parallel_tree=1000,
                         subsample=0.5, colsample_bytree=0.5,
                         nrounds=3, objective="binary:logistic")

# ---------------------------------------------------------- #

xgb.plot.multi.trees(boostedForest, feature_names=colnames(creditX))

# ---------------------------------------------------------- #



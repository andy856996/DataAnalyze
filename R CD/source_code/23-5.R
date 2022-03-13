# ---------
#  23-5
# ---------
# ---------------------------------------------------------- #

library(useful)
# �ήe�ҫ���formula(����)
# �]���O�إ߾�ҫ��A�ڭ̤��ݭn�I�x��
creditFormula <- Credit ~ CreditHistory + Purpose + Employment +
     Duration + Age + CreditAmount - 1

#�]���O�إ߾�ҫ��A�ڭ̨ϥΩҦ�level�����O�ܼ�
creditX <- build.x(creditFormula, data=credit, contrasts=FALSE)
creditY <- build.y(creditFormula, data=credit)

# �Nlogical vector(�޿�V�q)�ഫ��[0,1]
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
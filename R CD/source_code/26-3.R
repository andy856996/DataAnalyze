# ---------
#  26-3
# ---------
# ---------------------------------------------------------- #

acs <- tibble::as_tibble(
       read.table(
       "http://jaredlander.com/data/acs_ny.csv",
       sep=",", header=TRUE, stringsAsFactors=FALSE
        )
        )

# ---------------------------------------------------------- #

library(plyr)
library(dplyr)
acs <- acs %>%
            mutate(Income=factor(FamilyIncome >= 150000,
            levels=c(FALSE, TRUE),
            labels=c('Below', 'Above')))

# ---------------------------------------------------------- #

acsFormula <- Income ~ NumChildren +
                        NumRooms + NumVehicles + NumWorkers + OwnRent +
                        ElectricBill + FoodStamp + HeatingFuel

# ---------------------------------------------------------- #

ctrl <- trainControl(method = "repeatedcv",
                       repeats=2,
                       number=5,
                       summaryFunction=twoClassSummary,
                       classProbs=TRUE,
                       allowParallel=FALSE)

# ---------------------------------------------------------- #

boostGrid <- expand.grid(nrounds=100, #最高迭帶次數
                           max_depth=c(2, 6, 10),
                           eta=c(0.01, 0.1), # 壓縮程度
                           gamma=c(0),
                           colsample_bytree=1,
                           min_child_weight=1,
                           subsample=0.7)


# ---------------------------------------------------------- #

set.seed(73615)
boostTuned <- train(acsFormula, data=acs,
                      method="xgbTree",
                      metric="ROC",
                      trControl=ctrl,
                      tuneGrid=boostGrid, nthread=4)

# ---------------------------------------------------------- #

boostTuned$results %>% arrange(ROC)

# ---------------------------------------------------------- #

plot(boostTuned)

# ---------------------------------------------------------- #

xgb.plot.multi.trees(boostTuned$finalModel,
                      feature_names=boostTuned$coefnames)

# ---------------------------------------------------------- #

acsNew <- read.table('http://www.jaredlander.com/data/acsNew.csv',
                       header=TRUE, sep=',', stringsAsFactors=FALSE)

# ---------------------------------------------------------- #

predict(boostTuned, newdata=acsNew, type='raw') %>% head
predict(boostTuned, newdata=acsNew, type='prob') %>% head

# ---------------------------------------------------------- #
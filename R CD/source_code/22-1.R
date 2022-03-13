# ---------
#  22-1
# ---------
# ---------------------------------------------------------- #

acs <- read.table("http://jaredlander.com/data/acs_ny.csv", sep = ",",
                   header = TRUE, stringsAsFactors = FALSE)

# ---------------------------------------------------------- #

建立一個前三個直排為numeric的data.frame
testFrame <-
   data.frame(First=sample(1:10, 20, replace=TRUE),
            Second=sample(1:20, 20, replace=TRUE),
            Third=sample(1:10, 20, replace=TRUE),
            Fourth=factor(rep(c("Alice", "Bob", "Charlie", "David"),
                              5)),
            Fifth=ordered(rep(c("Edward", "Frank", "Georgia",
                                 "Hank", "Isaac"), 4)),
            Sixth=rep(c("a", "b"), 10), stringsAsFactors=F)
head(testFrame)

head(model.matrix(First ~ Second + Fourth + Fifth, testFrame))

# ---------------------------------------------------------- #

library(useful)
# 對所有變數使用其所有level
head(build.x(First ~ Second + Fourth + Fifth, testFrame,
             contrasts=FALSE))

# 只對Fourth使用所有level
head(build.x(First ~ Second + Fourth + Fifth, testFrame,
             contrasts=c(Fourth=FALSE, Fifth=TRUE)))

# ---------------------------------------------------------- #

# 對Income變數建立一個新的二元變數以建立羅吉斯迴歸
acs$Income <- with(acs, FamilyIncome >= 150000)
head(acs)

# 建立預測函數矩陣
# 不需要加入截距項,因為glmnet會自動加入它
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople +
                NumRooms + NumUnits + NumVehicles + NumWorkers +
                OwnRent + YearBuilt + ElectricBill + FoodStamp +
                HeatingFuel + Insurance + Language - 1,
                data=acs, contrasts=FALSE)

# 檢視class(資料結構)和dim(維度)
class(acsX)
dim(acsX)

# 檢視左上(top left)和右上(top right)的資料
topleft(acsX, c=6)
topright(acsX, c=6)

# 建立反應變數
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople +
                NumRooms + NumUnits + NumVehicles + NumWorkers +
                OwnRent + YearBuilt + ElectricBill + FoodStamp +
                HeatingFuel + Insurance + Language - 1, data=acs)

head(acsY)
tail(acsY)

# ---------------------------------------------------------- #

library(glmnet)
set.seed(1863561)

# 執行附有交叉驗證的glmnet
acsCV1 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5)

# ---------------------------------------------------------- #

acsCV1$lambda.min
acsCV1$lambda.1se
plot(acsCV1)

# ---------------------------------------------------------- #

coef(acsCV1, s = "lambda.1se")

# ---------------------------------------------------------- #

# 把路徑畫出
plot(acsCV1$glmnet.fit, xvar = "lambda")

# 對極佳化的lambda值加入垂直線
abline(v = log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty = 2)

# ---------------------------------------------------------- #

# 建立脊迴歸模型
set.seed(71623)
acsCV2 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5,
                    alpha = 0)

# 檢視lambda值
acsCV2$lambda.min
acsCV2$lambda.1se

# 檢視係數
coef(acsCV2, s = "lambda.1se")

# 繪製交叉驗證誤差路徑
plot(acsCV2)

# 繪製係數路徑
plot(acsCV2$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty = 2)	

# ---------------------------------------------------------- #

library(parallel)
library(doParallel)

# ---------------------------------------------------------- #

# 設定種子以讓隨機結果可以被重複
set.seed(2834673)

# 建立層別,我們要觀測值在每次執行時都會落在同一層
theFolds <- sample(rep(x = 1:5, length.out = nrow(acsX)))

# 製造一序列的alpha值
alphas <- seq(from = 0.5, to = 1, by = 0.05)

# ---------------------------------------------------------- #

# 設定種子以讓隨機結果可以被重複
set.seed(5127151)

# 啟動一個擁有兩個worker的叢集
cl <- makeCluster(2)

# 設worker為暫存器(register)
registerDoParallel(cl)

# 對過程計時
before <- Sys.time()

# 建立foreach迴圈並以平行運算的方式執行
## 一些引數的設定
acsDouble <- foreach(i=1:length(alphas), .errorhandling="remove",
                     .inorder=FALSE, .multicombine=TRUE,
                     .export=c("acsX", "acsY", "alphas", "theFolds"),
                     .packages="glmnet") %dopar%
            {
               print(alphas[i])
               cv.glmnet(x=acsX, y=acsY, family="binomial", nfolds=5,
               foldid=theFolds, alpha=alphas[i])
            }

# 停止計時
after <- Sys.time()

# 確保在所有過程完成後將叢集終止
stopCluster(cl)

# 過程所耗的時間
# 這因電腦速度,記憶體和核心個數而異
after - before
sapply(acsDouble, class)

# 用來抽取cv.glmnet物件訊息的函數
extractGlmnetInfo <- function(object)
      {
         # 找出被選中的lambda
         lambdaMin <- object$lambda.min
         lambda1se <- object$lambda.1se

         # 找出那些lambda落在路徑的什麼地方
         whichMin <- which(object$lambda == lambdaMin)
         which1se <- which(object$lambda == lambda1se)

         # 建立一個只有一行的data.frame,裏面含有被選中的lambda
         # 和它相關的錯誤訊息
         data.frame(lambda.min=lambdaMin, error.min=object$cvm[whichMin],
                    lambda.1se=lambda1se, error.1se=object$cvm[which1se])
      }

# 將該函數應用到list中的每個元素
# 把結果都整合到一個data.frame裏
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))

# 也可以通過plyr套件中的ldply來完成
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical(alphaInfo, alphaInfo2)

# 建立一個直排以列出alpha
alphaInfo$Alpha <- alphas
alphaInfo

# ---------------------------------------------------------- #

## 建立data.frame以方便將不同的訊息繪製出來
library(reshape2)
library(stringr)

# 將資料熔化成長的格式
alphaMelt <- melt(alphaInfo, id.vars="Alpha", value.name="Value",
                  variable.name="Measure")
alphaMelt$Type <- str_extract(string=alphaMelt$Measure,
                              pattern="(min)|(1se)")

# 做出一些處理讓它更整齊
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure,
                                 pattern="nn.(min|1se)",
                                 replacement="")
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure,
                   value.var="Value")
ggplot(alphaCast, aes(x=Alpha, y=error)) +
       geom_line(aes(group=Type)) +
       facet_wrap(~Type, scales="free_y", ncol=1) +
       geom_point(aes(size=lambda))

# ---------------------------------------------------------- #

set.seed(5127151)
acsCV3 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5,
                    alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])

# ---------------------------------------------------------- #

plot(acsCV3)
plot(acsCV3$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV3$lambda.min, acsCV3$lambda.1se)), lty = 2)

# ---------------------------------------------------------- #

theCoef <- as.matrix(coef(acsCV3, s = "lambda.1se"))
coefDF <- data.frame(Value = theCoef,
                     Coefficient = rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV3, s = "lambda.1se")), ]
ggplot(coefDF, aes(x = X1, y = reorder(Coefficient, X1))) +
       geom_vline(xintercept = 0, color = "grey", linetype = 2) +
       geom_point(color = "blue") + labs(x = "Value",
                                         y = "Coefficient", title = "Coefficient Plot")

# ---------------------------------------------------------- #
# ---------
#  24-3
# ---------
# ---------------------------------------------------------- #

library(quantmod)
load("data/att.rdata")

library(quantmod)
att <- getSymbols("T", auto.assign = FALSE)

# ---------------------------------------------------------- #

library(xts)

# 顯示資料
head(att)
plot(att)

# ---------------------------------------------------------- #

chartSeries(att)
addBBands()
addMACD(32, 50, 12)

# ---------------------------------------------------------- #

attClose <- att$T.Close
class(attClose)
head(attClose)

# ---------------------------------------------------------- #

library(rugarch)
attSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1, 1)),
                      mean.model=list(armaOrder=c(1, 1)),
                      distribution.model="std")

# ---------------------------------------------------------- #

attGarch <- ugarchfit(spec = attSpec, data = attClose)
attGarch

# ---------------------------------------------------------- #

# attGarch是一個S4物件,裏面的資料需透過@來對它進行套用
# 該資料儲存格式為list,因此需通過$來讀取
plot(attGarch@fit$residuals, type="l")
plot(attGarch, which=10)

# ---------------------------------------------------------- #

# ARMA(1,1)
attSpec1 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(1, 1)),
                       distribution.model="std")
# ARMA(0,0)
attSpec2 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(0, 0)),
                       distribution.model="std")
# ARMA(0,2)
attSpec3 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(0, 2)),
                       distribution.model="std")
# ARMA(1,2)
attSpec4 <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1, 1)),
                       mean.model=list(armaOrder=c(1, 2)),
                       distribution.model="std")

attGarch1 <- ugarchfit(spec=attSpec1, data=attClose)
attGarch2 <- ugarchfit(spec=attSpec2, data=attClose)
attGarch3 <- ugarchfit(spec=attSpec3, data=attClose)
attGarch4 <- ugarchfit(spec=attSpec4, data=attClose)

infocriteria(attGarch1)
infocriteria(attGarch2)
infocriteria(attGarch3)
infocriteria(attGarch4)

# ---------------------------------------------------------- #

attPred <- ugarchboot(attGarch, n.ahead=50,
                      method = c("Partial", "Full")[1])
plot(attPred, which=2)

# ---------------------------------------------------------- #

# 對對數報酬率做差分,並把第一個變成NA值的資料移除掉
attLog <- diff(log(attClose))[-1]

# 建立模型規格
attLogSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                             garchOrder=c(1, 1)),
                         mean.model=list(armaOrder=c(1, 1)),
                         distribution.model="std")

# 建立模型
attLogGarch <- ugarchfit(spec=attLogSpec, data=attLog)
infocriteria(attLogGarch)

# ---------------------------------------------------------- #
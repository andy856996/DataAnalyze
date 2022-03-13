# ---------
#  11-3
# ---------
# ---------
#  11-3-1
# ---------
# ---------------------------------------------------------- #

library(plyr)
head(baseball)

# ---------------------------------------------------------- #

# 用[來抽取資料比用ifelse來得快
baseball$sf[baseball$year < 1954] <- 0

# 檢查是否已經成功
any(is.na(baseball$sf))

# 把HBP的NA值設為0
baseball$hbp[is.na(baseball$hbp)] <- 0

# 檢查是否成功
any(is.na(baseball$hbp))

# 只保留在一季裏AB(打數)至少為50的選手
baseball <- baseball[baseball$ab >= 50, ]

# 計算OBP
baseball$OBP <- with(baseball, (h + bb + hbp)/(ab + bb + hbp + sf))
tail(baseball)

# ---------------------------------------------------------- #

# 此函數假設資料的直排名稱如下
obp <- function(data)
   {
      c(OBP = with(data, sum(h + bb + hbp)/sum(ab + bb + hbp + sf)))
   }

# 使用ddply對每個選手計算其整個棒球職業生涯的OBP
careerOBP <- ddply(baseball, .variables = "id", .fun = obp)

# 依據OBP對結果做出排序
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE), ]

# 勘查結果
head(careerOBP, 10)

# ---------------------------------------------------------- #
# ---------
#  11-3-2
# ---------
# ---------------------------------------------------------- #

theList <- list(A = matrix(1:9, 3), B = 1:5, C = matrix(1:4, 2), D = 2)
lapply(theList, sum)

# ---------------------------------------------------------- #

llply(theList, sum)
identical(lapply(theList, sum), llply(theList, sum))

# ---------------------------------------------------------- #

sapply(theList, sum)
laply(theList, sum)

# ---------------------------------------------------------- #
# ---------
#  11-3-3
# ---------
# ---------------------------------------------------------- #

aggregate(price ~ cut, diamonds, each(mean, median))

# ---------------------------------------------------------- #

system.time(dlply(baseball, "id", nrow))
iBaseball <- idata.frame(baseball)
system.time(dlply(iBaseball, "id", nrow))

# ---------------------------------------------------------- #
# ---------
#  16-3
# ---------
# ---------------------------------------------------------- #

library(XML)

# ---------------------------------------------------------- #

load("data/presidents.rdata")
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
presidents <- readHTMLTable(theURL, which=3, as.data.frame=TRUE,
                            skip.rows=1, header=TRUE,
                            stringsAsFactors=FALSE)

# ---------------------------------------------------------- #

head(presidents)

# ---------------------------------------------------------- #

tail(presidents$YEAR)
presidents <- presidents[1:64, ]

# ---------------------------------------------------------- #

library(stringr)
# 拆開字串
yearList <- str_split(string = presidents$YEAR, pattern = "-")
head(yearList)

# 把它們合併成一個matrix(矩陣)
yearMatrix <- data.frame(Reduce(rbind, yearList))
head(yearMatrix)

# 對直排命名
names(yearMatrix) <- c("Start", "Stop")

# 把新的直排合併到data.frame
presidents <- cbind(presidents, yearMatrix)

# 把上任和卸任年份轉換成numeric
presidents$Start <- as.numeric(as.character(presidents$Start))
presidents$Stop <- as.numeric(as.character(presidents$Stop))

# 看做了甚麼改變
head(presidents)
tail(presidents)

# ---------------------------------------------------------- #

# 抽取前三個字元
str_sub(string = presidents$PRESIDENT, start = 1, end = 3)

# 抽取第四到第八個字元
str_sub(string = presidents$PRESIDENT, start = 4, end = 8)

# ---------------------------------------------------------- #

presidents[str_sub(string = presidents$Start, start = 4,
                   end = 4) == 1, c("YEAR", "PRESIDENT", "Start", "Stop")]

# ---------------------------------------------------------- #
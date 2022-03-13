# ---------
#  14-2
# ---------
# ---------------------------------------------------------- #

download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile="data/ForeignAid.zip")
unzip("data/ForeignAid.zip", exdir="data")

# ---------------------------------------------------------- #

library(stringr)

# 首先取得檔案的列表
theFiles <- dir("data/", pattern="\\.csv")

## 對這些檔案進行迭代
for(a in theFiles)
{
   # 建立適合的名稱以指派到資料群
   nameToUse <- str_sub(string=a, start=12, end=18)
   
   # 用read.table讀取csv檔
   # 用file.path來指定文件夾和檔名是的一個便捷的方法
   temp <- read.table(file=file.path("data", a),
                      header=TRUE, sep=",", stringsAsFactors=FALSE)
   
   # 把它們指派到工作空間
   assign(x=nameToUse, value=temp)
}

# ---------------------------------------------------------- #
# ---------
#  14-2-1
# ---------
# ---------------------------------------------------------- #

Aid90s00s <- merge(x=Aid_90s, y=Aid_00s,
                   by.x=c("Country.Name", "Program.Name"),
                   by.y=c("Country.Name", "Program.Name"))
head(Aid90s00s)

# ---------------------------------------------------------- #
# ---------
#  14-2-2
# ---------
# ---------------------------------------------------------- #

library(plyr)
Aid90s00sJoin <- join(x = Aid_90s, y = Aid_00s, by = c("Country.Name",
                                                       "Program.Name"))
head(Aid90s00sJoin)

# ---------------------------------------------------------- #

# 先找出data.frame的名稱
frameNames <- str_sub(string = theFiles, start = 12, end = 18)

# 建立一個空list
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames

# 把每個data.frame放入list裏
for (a in frameNames)
{
   frameList[[a]] <- eval(parse(text = a))
}

# ---------------------------------------------------------- #

head(frameList[[1]])
head(frameList[["Aid_00s"]])
head(frameList[[5]])
head(frameList[["Aid_60s"]])

# ---------------------------------------------------------- #

allAid <- Reduce(function(...)
{
   join(..., by = c("Country.Name", "Program.Name"))
}, frameList)
dim(allAid)

require(useful)
corner(allAid, c = 15)
bottomleft(allAid, c = 15)

# ---------------------------------------------------------- #
# ---------
#  14-2-3
# ---------
# ---------------------------------------------------------- #

library(data.table)
dt90 <- data.table(Aid_90s, key = c("Country.Name", "Program.Name"))
dt00 <- data.table(Aid_00s, key = c("Country.Name", "Program.Name"))

# ---------------------------------------------------------- #

dt0090 <- dt90[dt00]

# ---------------------------------------------------------- #
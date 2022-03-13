# ---------
#  14-2
# ---------
# ---------------------------------------------------------- #

download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile="data/ForeignAid.zip")
unzip("data/ForeignAid.zip", exdir="data")

# ---------------------------------------------------------- #

library(stringr)

# �������o�ɮת��C��
theFiles <- dir("data/", pattern="\\.csv")

## ��o���ɮ׶i�歡�N
for(a in theFiles)
{
   # �إ߾A�X���W�٥H�������Ƹs
   nameToUse <- str_sub(string=a, start=12, end=18)
   
   # ��read.tableŪ��csv��
   # ��file.path�ӫ��w��󧨩M�ɦW�O���@�ӫK������k
   temp <- read.table(file=file.path("data", a),
                      header=TRUE, sep=",", stringsAsFactors=FALSE)
   
   # �⥦�̫�����u�@�Ŷ�
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

# ����Xdata.frame���W��
frameNames <- str_sub(string = theFiles, start = 12, end = 18)

# �إߤ@�Ӫ�list
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames

# ��C��data.frame��Jlist��
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
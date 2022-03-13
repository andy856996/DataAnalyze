# ---------
#  6-3
# ---------
# ---------------------------------------------------------- #

download.file("http://www.jaredlander.com/data/diamonds.db",
              destfile = "data/diamonds.db", mode='wb')

# ---------------------------------------------------------- #

library(RSQLite)

# ---------------------------------------------------------- #

drv <- dbDriver('SQLite')
class(drv)

# ---------------------------------------------------------- #

con <- dbConnect(drv, 'data/diamonds.db')
class(con)

# ---------------------------------------------------------- #

dbListTables(con)
dbListFields(con, name='diamonds')
dbListFields(con, name='DiamondColors')

# ---------------------------------------------------------- #

# 用SELECT * 查詢一個表
diamondsTable <- dbGetQuery(con,
                            "SELECT * FROM diamonds",
                            stringsAsFactors=FALSE)

# 用SELECT * 查詢一個表
colorTable <- dbGetQuery(con,
                         "SELECT * FROM DiamondColors",
                         stringsAsFactors=FALSE)

#  將兩張表合併起來
longQuery <- "SELECT * FROM diamonds, DiamondColors
              WHERE
              diamonds.color = DiamondColors.Color"

diamondsJoin <- dbGetQuery(con, longQuery,
                           stringsAsFactors=FALSE)

# ---------------------------------------------------------- #
longquery1="SELECT diamonds.cut,diamonds.color ,DiamondColors.Color FROM diamonds inner join DiamondColors on diamonds.color=DiamondColors.color"
head(diamondsTable)
head(colorTable)
head(diamondsJoin)

# ---------------------------------------------------------- #
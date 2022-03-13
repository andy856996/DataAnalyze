# ---------
#  16-4
# ---------
# ---------------------------------------------------------- #

# 回傳TRUE/FALSE以表示是否在名字找到”John”
johnPos <- str_detect(string = presidents$PRESIDENT, pattern = "John")
presidents[johnPos, c("YEAR", "PRESIDENT", "Start", "Stop")]

# ---------------------------------------------------------- #

badSearch <- str_detect(presidents$PRESIDENT, "john")
goodSearch <- str_detect(presidents$PRESIDENT, ignore.case("John"))
sum(badSearch)
sum(goodSearch)

# ---------------------------------------------------------- #

con <- url("http://www.jaredlander.com/data/warTimes.rdata")
load(con)
close(con)

# ---------------------------------------------------------- #

head(warTimes, 10)

# ---------------------------------------------------------- #

warTimes[str_detect(string = warTimes, pattern = "-")]

# ---------------------------------------------------------- #

theTimes <- str_split(string = warTimes, pattern = "(ACAEA)|-", n = 2)
head(theTimes)

# ---------------------------------------------------------- #

which(str_detect(string = warTimes, pattern = "-"))
theTimes[[147]]
theTimes[[150]]

# ---------------------------------------------------------- #

theStart <- sapply(theTimes, FUN = function(x) x[1])
head(theStart)

# ---------------------------------------------------------- #

theStart <- str_trim(theStart)
head(theStart)

# ---------------------------------------------------------- #

# 抽出任何含有'January'的地方,否則回傳NA
str_extract(string = theStart, pattern = "January")

# ---------------------------------------------------------- #

# 只回傳偵測到'January'的原素
theStart[str_detect(string = theStart, pattern = "January")]

# ---------------------------------------------------------- #

# 搜尋四個連在一起的數字
head(str_extract(string = theStart, "[0-9][0-9][0-9][0-9]"), 20)

# ---------------------------------------------------------- #

# 以更聰明的方法來找尋四個數字
head(str_extract(string = theStart, "[0-9]{4}"), 20)

# ---------------------------------------------------------- #

# \\d是"[0-9]"的一個捷徑
head(str_extract(string = theStart, "\\d{4}"), 20)

# ---------------------------------------------------------- #

# 這會搜尋任何一個出現過一次,兩次或三次的數字
str_extract(string = theStart, "\\d{1,3}")

# ---------------------------------------------------------- #

# 在文字前端搜尋四個數字
head(str_extract(string = theStart, pattern = "^\\d{4}"), 30)

# 在文字後端搜尋四個數字
head(str_extract(string = theStart, pattern = "\\d{4}$"), 30)

# 在文字前端和後端搜尋四個數字
head(str_extract(string = theStart, pattern = "^\\d{4}$"), 30)

# ---------------------------------------------------------- #

# 將第一個數字取代為"x"
head(str_replace(string=theStart, pattern="\\d", replacement="x"), 30)

# 將所有看得到的數字取代為"x"
# 這意味著"7" -> "x"和"382" -> "xxx"
head(str_replace_all(string=theStart, pattern="\\d", replacement="x"),
     30)

# 取代任何由一位數到四位數組成的數字字串為"x"
# 這意思是"7" -> "x"和"382" -> "x"
head(str_replace_all(string=theStart, pattern="\\d{1,4}",
                     replacement="x"), 30)

# ---------------------------------------------------------- #

# 建立一個HTML指令的vector
commands <- c("<a href=index.html>The Link is here</a>",
              "<b>This is bold text</b>")

# ---------------------------------------------------------- #

# 取得HTML標籤之間的文字
# 在(.+?)裏的內容將被1(第一組文字)取代
str_replace(string=commands, pattern="<.+?>(.+?)<.+>",
            replacement="\\1")

# ---------------------------------------------------------- #
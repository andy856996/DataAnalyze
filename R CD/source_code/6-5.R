# ---------
#  6-5
# ---------
# ---------------------------------------------------------- #

# 儲存tomato 這個data.frame入硬盤
save(tomato, file = "data/tomato.rdata")

# 從記憶體移除tomato
rm(tomato)

# 查看tomate是否還存在
head(tomato)

# 從rdata讀取
load("data/tomato.rdata")

# 查看它現在是否存在
head(tomato)

# ---------------------------------------------------------- #

# 建立一些物件
n <- 20
r <- 1:10
w <- data.frame(n, r)

# 勘查它們
n
r
w

# 儲存它們
save(n, r, w, file = "data/multiple.rdata")

# 刪除它們
rm(n, r, w)

# 它們消失了嗎?
n
r
w

# 把它們再讀取進來
load("data/multiple.rdata")

# 再次查看它們的存在
n
r
w

# ---------------------------------------------------------- #

#  建立一個物件
smallVector <- c(1, 5, 4)

# 檢視它
smallVector

# 儲存到rds檔’
saveRDS(smallVector, file='thisObject.rds')
  
# 讀取檔案，接著儲存到另一個物件
thatVect <- readRDS('thisObject.rds')

# 顯示它
thatVect

# 檢查它們是否相同
identical(smallVector, thatVect)

# ---------------------------------------------------------- #

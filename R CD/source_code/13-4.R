# ---------
#  13-4
# ---------
# ---------------------------------------------------------- #

## 建立兩個list
firstList <- list(A=matrix(1:16, 4), B=matrix(1:16, 2), C=1:5)
secondList <- list(A=matrix(1:16, 4), B=matrix(1:16, 8), C=15:1)

## 將對應元素的橫列數量(或長度)加總
simpleFunc <- function(x, y)
              {
                NROW(x) + NROW(y)
              }
      
# 將函數套用到該兩個list
map2(firstList, secondList, simpleFunc)

# 將函數套用到該兩個list，並回傳一個integer(整數)
map2_int(firstList, secondList, simpleFunc)
    
# ---------------------------------------------------------- #
    
# 使用更廣義的pmap
pmap(list(firstList, secondList), simpleFunc)
pmap_int(list(firstList, secondList), simpleFunc)
    
# ---------------------------------------------------------- #
    
# ---------
#  12-10
# ---------
# ---------------------------------------------------------- #

topN <- function(x, N=5)
        {
              x %>% arrange(desc(price)) %>% head(N)
        }


# ---------------------------------------------------------- #

diamonds %>% group_by(cut) %>% do(topN(., N=3))

# ---------------------------------------------------------- #

diamonds %>%
  # 根據切割品質(cut)對資料分群
  # 這將建立出分隔開來的資料群
  group_by(cut) %>%
  # 套用topN函數，第二個引數設定為3
  # 這將套用到每群資料
  do(Top=topN(., 3))

topByCut <- diamonds %>% group_by(cut) %>% do(Top=topN(., 3))

class(topByCut)
class(topByCut$Top)
class(topByCut$Top[[1]])
topByCut$Top[[1]]

# ---------------------------------------------------------- #
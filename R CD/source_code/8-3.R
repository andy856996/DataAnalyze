# ---------
#  8-3
# ---------
# ---------------------------------------------------------- #

# 建立函數時不使用return指令
double.num <- function(x)
{
   x * 2
}
double.num(5)

# 使用return指令建立
double.num <- function(x)
{
   return(x * 2)
}
double.num(5)

# 再次建立函數,這次在return後面也放一些指令
double.num <- function(x)
{
   return(x * 2)
   # 以下指令不會被執行,因為函數已經被退出了
   print("Hello!")
   return(17)
}
double.num(5)

# ---------------------------------------------------------- #
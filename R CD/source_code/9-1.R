# ---------
#  9-1
# ---------
# ---------------------------------------------------------- #

as.numeric(TRUE)
as.numeric(FALSE)

# ---------------------------------------------------------- #

1 == 1 # TRUE
1 < 1 # FALSE
1 <= 1 # TRUE
1 > 1 # FALSE
1 >= 1 # TRUE
1 != 1 # FALSE

# ---------------------------------------------------------- #

# 建立一個含有1的變數
toCheck <- 1

# 若toCheck等於1,顯示hello
if (toCheck == 1)
{
   print("hello")
}

# 現在如果toCheck等於0,則顯示hello
if (toCheck == 0)
{
 	print("hello")
}

# 可以看到結果並沒顯示任何東西

# ---------------------------------------------------------- #

# 首先先建立函數
check.bool <- function(x)
{
   if (x == 1)
   {

   # 若輸入值為1則顯示hello
   print("hello")
   } else
   {
   # 否則顯示goodbye
   print("goodbye")
   }
}

# ---------------------------------------------------------- #

check.bool(1)
check.bool(0)
check.bool("k")
check.bool(TRUE)
# ---------------------------------------------------------- #

check.bool <- function(x)
{
   if (x == 1)
   {
      # 若輸入值等於1,顯示hello
      print("hello")
   } else if (x == 0)
   {
      # 若輸入值等於0,顯示goodbye
      print("goodbye")
   } else
   {
      # 否則顯示confused
      print("confused")
   }
}

check.bool(1)
check.bool(0)
check.bool(2)
check.bool("k")

# ---------------------------------------------------------- #
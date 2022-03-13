# ---------
#  8-4
# ---------
# ---------------------------------------------------------- #

do.call("hello.person", args = list(first = "Jared", last = "Lander"))
do.call(hello.person, args = list(first = "Jared", last = "Lander"))

# ---------------------------------------------------------- #

run.this <- function(x, func = mean)
{
   do.call(func, args = list(x))
}

# 用預設的mean函數找平均值
run.this(1:10)

# 指定要找平均值
run.this(1:10, mean)

# 計算總和
run.this(1:10, sum)

# 計算標準差
run.this(1:10, sd)

# ---------------------------------------------------------- #
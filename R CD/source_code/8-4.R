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

# �ιw�]��mean��Ƨ䥭����
run.this(1:10)

# ���w�n�䥭����
run.this(1:10, mean)

# �p���`�M
run.this(1:10, sum)

# �p��зǮt
run.this(1:10, sd)

# ---------------------------------------------------------- #
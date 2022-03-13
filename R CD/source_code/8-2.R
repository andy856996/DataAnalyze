# ---------
#  8-2
# ---------
# ---------------------------------------------------------- #

# 疭块じ
sprintf("Hello %s", "Jared")

# ㄢ疭块じ
sprintf("Hello %s, today is %s", "Jared", "Sunday")

# ---------------------------------------------------------- #

hello.person <- function(name)
{
   print(sprintf("Hello %s", name))
}

hello.person("Jared")
hello.person("Bob")
hello.person("Sarah")

# ---------------------------------------------------------- #

hello.person <- function(first, last)
{
   print(sprintf("Hello %s %s", first, last))
}

# 硓筁ま计癸竚
hello.person("Jared", "Lander")

# 硓筁ま计嘿
hello.person(first = "Jared", last = "Lander")

# рま计は筁ㄓ
hello.person(last = "Lander", first = "Jared")

# ﹚ま计嘿
hello.person("Jared", last = "Lander")

# ﹚ま计嘿
hello.person(first = "Jared", "Lander")

# 硓筁ま计嘿﹚材ま计,钡帝⊿块ま计嘿﹚材ま计
hello.person(last = "Lander", "Jared")

# ---------------------------------------------------------- #

hello.person(fir = "Jared", l = "Lander")

# ---------------------------------------------------------- #
# ---------
#  8-2-1
# ---------
# ---------------------------------------------------------- #

hello.person <- function(first, last = "Doe")
{
   print(sprintf("Hello %s %s", first, last))
}

# ㊣ㄧ计ぃ﹚﹎ん
hello.person("Jared")

# ㊣ㄧ计﹚ㄤウ﹎ん
hello.person("Jared", "Lander")

# ---------------------------------------------------------- #
# ---------
#  8-2-2
# ---------
# ---------------------------------------------------------- #

# ㊣hello.person,肂ま计
hello.person("Jared", extra = "Goodbye")

# ㄢΤま计㊣ㄧ计,材ま计琌肂
hello.person("Jared", "Lander", "Goodbye")

# 瞷ミhello.person, ...ま计钡ま计
hello.person <- function(first, last = "Doe", ...)
{
   print(sprintf("Hello %s %s", first, last))
}

# ㊣hello.person,肂ま计
hello.person("Jared", extra = "Goodbye")

#ㄢΤま计㊣ㄧ计,材ま计琌肂
hello.person("Jared", "Lander", "Goodbye")

# ---------------------------------------------------------- #
# ---------
#  12-4
# ---------
# ---------------------------------------------------------- #

diamonds %>% filter(cut == 'Ideal')

# ---------------------------------------------------------- #

diamonds[diamonds$cut == 'Ideal', ]

# ---------------------------------------------------------- #

diamonds %>% filter(cut %in% c('Ideal', 'Good'))

# ---------------------------------------------------------- #

diamonds %>% filter(price >= 1000)
diamonds %>% filter(price != 1000)

# ---------------------------------------------------------- #

diamonds %>% filter(carat > 2 , price < 14000)
diamonds %>% filter(carat > 2 & price < 14000)

# ---------------------------------------------------------- #

diamonds %>% filter(carat < 1 | carat > 5)

# ---------------------------------------------------------- #

diamonds %>% filter_("cut == 'Ideal'")
diamonds %>% filter_(~cut == 'Ideal')

# 先把條件值存在變數裡
theCut <- 'Ideal'
diamonds %>% filter_(~cut == theCut)

# ---------------------------------------------------------- #

theCol <- 'cut'
theCut <- 'Ideal'
diamonds %>% filter_(sprintf("%s == '%s'", theCol, theCut))

# ---------------------------------------------------------- #

library(lazyeval)

# 用變數建立一個公式
interp(~a == b, a=as.name(theCol), b=theCut)

# 將該公式設為filter_的引數
diamonds %>% filter_(interp(~ a == b, a=as.name(theCol), b=theCut))

# ---------------------------------------------------------- #

diamonds %>% filter(UQE(as.name(theCol)) == theCut)

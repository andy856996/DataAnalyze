# ---------
#  12-3
# ---------
# ---------------------------------------------------------- #

select(diamonds, carat, price)
diamonds %>% select(carat, price)

#也可以使用一個含有直行名稱的向量(vector) 指定直行
diamonds %>% select(c(carat, price))

# ---------------------------------------------------------- #

diamonds %>% select_('carat', 'price')

# ---------------------------------------------------------- #

theCols <- c('carat', 'price')
diamonds %>% select_(.dots=theCols)

# ---------------------------------------------------------- #

diamonds %>% select(one_of('carat', 'price'))

# ---------------------------------------------------------- #

# 作為一個變數
theCols <- c('carat', 'price')
diamonds %>% select(one_of(theCols))

# ---------------------------------------------------------- #
  
diamonds[, c('carat', 'price')]
  
# ---------------------------------------------------------- #

select(diamonds, 1, 7)
select(diamonds, 1, 7)

# ---------------------------------------------------------- #
  
diamonds %>% select(starts_with('c'))
diamonds %>% select(ends_with('e'))
diamonds %>% select(contains('l'))

# ---------------------------------------------------------- #
  
diamonds %>% select(matches('r.+t'))
  
# ---------------------------------------------------------- #
  
# 使用直行名稱
diamonds %>% select(-carat, -price)
diamonds %>% select(-c(carat, price))

# 使用索引
diamonds %>% select(-1, -7)
diamonds %>% select(-c(1, 7))

# ---------------------------------------------------------- #

diamonds %>% select_(.dots=c('-carat', '-price'))
  
# ---------------------------------------------------------- #
  
diamonds %>% select(-one_of('carat', 'price'))

# ---------------------------------------------------------- #
# ---------
#  6-2
# ---------
# ---------------------------------------------------------- #

download.file(url='http://www.jaredlander.com/data/ExcelExample.xlsx',
              destfile='data/ExcelExample.xlsx', method='curl')

# ---------------------------------------------------------- #

library(readxl)
excel_sheets('data/ExcelExample.xlsx')

# ---------------------------------------------------------- #

tomatoXL <- read_excel('data/ExcelExample.xlsx')
tomatoXL

# ---------------------------------------------------------- #

wineXL1 <- read_excel('data/ExcelExample.xlsx', sheet=2)
head(wineXL1)

# ---------------------------------------------------------- #

wineXL2 <- read_excel('data/ExcelExample.xlsx', sheet='Wine')
head(wineXL2)

# ---------------------------------------------------------- #
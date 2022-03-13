# ---------
#  26-2
# ---------
# ---------
#  26-2-1
# ---------
# ---------------------------------------------------------- #

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                       repeats=3,
                       number=5,
                       summaryFunction=defaultSummary,
                       allowParallel=TRUE)

# ---------------------------------------------------------- #
# ---------
#  26-2-2
# ---------
# ---------------------------------------------------------- #

gamGrid <- data.frame(select=c(TRUE, TRUE, FALSE, FALSE),
                        method=c('GCV.Cp', 'REML', 'GCV.Cp', 'REML'),
                        stringsAsFactors=FALSE)
gamGrid

# ---------------------------------------------------------- #
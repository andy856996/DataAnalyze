# ---------
#  25-1
# ---------
# ---------------------------------------------------------- #

wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineUrl, header=FALSE, sep=',',
                     stringsAsFactors=FALSE,
                     col.names=c('Cultivar', 'Alcohol', 'Malic.acid',
                                   'Ash', 'Alcalinity.of.ash',
                                   'Magnesium', 'Total.phenols',
                                   'Flavanoids', 'Nonflavanoid.phenols',
                                   'Proanthocyanin', 'Color.intensity',
                                   'Hue', 'OD280.OD315.of.diluted.wines',
                                   'Proline'
                                   ))

head(wine)

# ---------------------------------------------------------- #

wineTrain <- wine[, which(names(wine) != "Cultivar")]

# ---------------------------------------------------------- #

set.seed(278613)
wineK3 <- kmeans(x = wineTrain, centers = 3)
wineK3

# ---------------------------------------------------------- #

library(useful)
plot(wineK3, data = wineTrain)

# ---------------------------------------------------------- #

plot(wineK3, data = wine, class = "Cultivar")

# ---------------------------------------------------------- #

set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers = 3, nstart = 25)

# 以一個初始條件做分群
wineK3$size

# 以25個初始條件做分群
wineK3N25$size

# ---------------------------------------------------------- #

wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25,
                      seed=278613)
wineBest
PlotHartigan(wineBest)

# ---------------------------------------------------------- #

table(wine$Cultivar, wineK3N25$cluster)
plot(table(wine$Cultivar, wineK3N25$cluster),
     main="Confusion Matrix for Wine Clustering",
     xlab="Cultivar", ylab="Cluster")

# ---------------------------------------------------------- #

library(cluster)
theGap <- clusGap(wineTrain, FUNcluster = pam, K.max = 20)
gapDF <- as.data.frame(theGap$Tab)
gapDF

# ---------------------------------------------------------- #

# logW曲線
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
       geom_line(aes(y=logW), color="blue") +
       geom_point(aes(y=logW), color="blue") +
       geom_line(aes(y=E.logW), color="green") +
       geom_point(aes(y=E.logW), color="green") +
       labs(x="Number of Clusters")

# 差距曲線
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
       geom_line(aes(y=gap), color="red") +
       geom_point(aes(y=gap), color="red") +
       geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
       labs(x="Number of Clusters", y="Gap")

# ---------------------------------------------------------- #
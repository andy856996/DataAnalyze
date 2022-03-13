# ---------
#  25-3
# ---------
# ---------------------------------------------------------- #

wineH <- hclust(d = dist(wineTrain))
plot(wineH)

# ---------------------------------------------------------- #

# 計算距離
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",
                                         "capital", "iso3c"))
wbDaisy <- daisy(x=wbInfo[, keep.cols])
wbH <- hclust(wbDaisy)
plot(wbH)

# ---------------------------------------------------------- #

wineH1 <- hclust(dist(wineTrain), method = "single")
wineH2 <- hclust(dist(wineTrain), method = "complete")
wineH3 <- hclust(dist(wineTrain), method = "average")
wineH4 <- hclust(dist(wineTrain), method = "centroid")

plot(wineH1, labels = FALSE, main = "Single")
plot(wineH2, labels = FALSE, main = "Complete")
plot(wineH3, labels = FALSE, main = "Average")
plot(wineH4, labels = FALSE, main = "Centroid")

# ---------------------------------------------------------- #

# 繪製樹
plot(wineH)

# 將資料分為3群
rect.hclust(wineH, k = 3, border = "red")

# 將資料分為13群
rect.hclust(wineH, k = 13, border = "blue")

# ---------------------------------------------------------- #

# 繪製樹
plot(wineH)

# 將資料分為3群
rect.hclust(wineH, h = 200, border = "red")

# 將資料分為13群
rect.hclust(wineH, h = 800, border = "blue")

# ---------------------------------------------------------- #
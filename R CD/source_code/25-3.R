# ---------
#  25-3
# ---------
# ---------------------------------------------------------- #

wineH <- hclust(d = dist(wineTrain))
plot(wineH)

# ---------------------------------------------------------- #

# �p��Z��
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

# ø�s��
plot(wineH)

# �N��Ƥ���3�s
rect.hclust(wineH, k = 3, border = "red")

# �N��Ƥ���13�s
rect.hclust(wineH, k = 13, border = "blue")

# ---------------------------------------------------------- #

# ø�s��
plot(wineH)

# �N��Ƥ���3�s
rect.hclust(wineH, h = 200, border = "red")

# �N��Ƥ���13�s
rect.hclust(wineH, h = 800, border = "blue")

# ---------------------------------------------------------- #
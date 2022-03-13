# ---------
#  23-3
# ---------
# ---------------------------------------------------------- #

# 建立直排名稱的vector
creditNames <- c("Checking", "Duration", "CreditHistory",
                 "Purpose", "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits", "Job",
                 "NumLiable", "Phone", "Foreign", "Credit")

# 用read.table讀取文件
# 指定原本不包括在資料裏的直排名稱
# col.names的輸入值是從creditNames來的
theURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit <- read.table(theURL, sep = " ", header = FALSE,
                     col.names = creditNames,
                     stringsAsFactors = FALSE)
head(credit)

# ---------------------------------------------------------- #

# 之前
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])

creditHistory <- c(A30 = "All Paid", A31 = "All Paid This Bank",
                   A32 = "Up To Date", A33 = "Late Payment",
                   A34 = "Critical Account")

purpose <- c(A40 = "car (new)", A41 = "car (used)",
             A42 = "furniture/equipment", A43 = "radio/television",
             A44 = "domestic appliances", A45 = "repairs",
             A46 = "education", A47 = "(vacation - does not exist?)",
             A48 = "retraining", A49 = "business", A410 = "others")

employment <- c(A71 = "unemployed", A72 = "< 1 year",
                A73 = "1 - 4 years", A74 = "4 - 7 years", A75 = ">= 7 years")

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]

# 將信用重新編成好(good)/差(bad)
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")

# 將信用好(good)設為基層
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))

# ---------------------------------------------------------- #

# 之後
head(credit[, c("CreditHistory", "Purpose", "Employment","Credit")])

library(useful)
ggplot(credit, aes(x=CreditAmount, y=Credit)) +
         geom_jitter(position = position_jitter(height = .2)) +
         facet_grid(CreditHistory ~ Employment) +
         xlab("Credit Amount") +
         theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
         scale_x_continuous(labels=multiple)

ggplot(credit, aes(x=CreditAmount, y=Age)) +
         geom_point(aes(color=Credit)) +
         facet_grid(CreditHistory ~ Employment) +
         xlab("Credit Amount") +
         theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
         scale_x_continuous(labels=multiple)

# ---------------------------------------------------------- #

library(mgcv)

# 建立一個羅吉斯GAM
# 把張量積運用在CreditAmount, 而將樣條運用在Age
creditGam <- gam(Credit ~ te(CreditAmount) + s(Age) + CreditHistory +
                 Employment,
                 data=credit, family=binomial(link="logit"))
summary(creditGam)

# ---------------------------------------------------------- #

plot(creditGam, select = 1, se = TRUE, shade = TRUE)
plot(creditGam, select = 2, se = TRUE, shade = TRUE)

# ---------------------------------------------------------- #
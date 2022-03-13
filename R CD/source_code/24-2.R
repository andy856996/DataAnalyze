# ---------
#  24-2
# ---------
# ---------------------------------------------------------- #

# ���Jreshape2
library(reshape2)

# �Ndata.frame�ഫ���e���榡
gdpCast <- dcast(Year ~ Country,
                 data=gdp[, c("Country", "Year", "PerCapGDP")],
                 value.var="PerCapGDP")
head(gdpCast)

# �������e�Q�Ӿ��,��]�O�w��(Germany)�èS�� <-�o�ӳ������Ӧ����D,�n���O�֤F�I����
# �N���ഫ���ɶ��ǦC
gdpTS <- ts(data=gdpCast[, -1], start=min(gdpCast$Year),
            end=max(gdpCast$Year))

# �Τ���ø�ϥ\��ӵe�ϩM�[�J����
plot(gdpTS, plot.type="single", col=1:8)
legend("topleft", legend=colnames(gdpTS), ncol=2, lty=1,
       col=1:8, cex=.9)

# ---------------------------------------------------------- #

gdpTS <- gdpTS[, which(colnames(gdpTS) != "Germany")]

# ---------------------------------------------------------- #

numDiffs <- ndiffs(gdpTS)
numDiffs

gdpDiffed <- diff(gdpTS, differences=numDiffs)
plot(gdpDiffed, plot.type="single", col=1:7)
legend("bottomleft", legend=colnames(gdpDiffed), ncol=2, lty=1,
       col=1:7, cex=.9)

# ---------------------------------------------------------- #

library(vars)

# �إ߼ҫ�
gdpVar <- VAR(gdpDiffed, lag.max = 12)

# �ҬD�諸�춥(order)
gdpVar$p

# �C�Ӽҫ����W��
names(gdpVar$varresult)

# �C�Ӽҫ���곣�Olm����
class(gdpVar$varresult$Canada)
class(gdpVar$varresult$Japan)

# �C�Ӽҫ������U�۪��Y��
head(coef(gdpVar$varresult$Canada))
head(coef(gdpVar$varresult$Japan))

library(coefplot)
coefplot(gdpVar$varresult$Canada)
coefplot(gdpVar$varresult$Japan)

# ---------------------------------------------------------- #

predict(gdpVar, n.ahead = 5)

# ---------------------------------------------------------- #
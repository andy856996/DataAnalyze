# ---------
#  18-3
# ---------
# ---------------------------------------------------------- #

head(tips)

# �A�ȭ����ʧO
unique(tips$sex)

# �C�P�U��
unique(tips$day)

# ---------------------------------------------------------- #
# ---------
#  18-3-1
# ---------
# ---------------------------------------------------------- #

t.test(tips$tip, alternative = "two.sided", mu = 2.5)

# ---------------------------------------------------------- #

## �إߤ@��t���G
randT <- rt(30000, df=NROW(tips)-1)

# �o��t�έp�q�M�䥦�����T��
tipTTest <- t.test(tips$tip, alternative="two.sided", mu=2.50)

# ø�s��
ggplot(data.frame(x=randT)) +
       geom_density(aes(x=x), fill="grey", color="grey") +
       geom_vline(xintercept=tipTTest$statistic) +
       geom_vline(xintercept=mean(randT) + c(-2, 2)*sd(randT), linetype=2)

# ---------------------------------------------------------- #

t.test(tips$tip, alternative = "greater", mu = 2.5)

# ---------------------------------------------------------- #
# ---------
#  18-3-2
# ---------
# ---------------------------------------------------------- #

# ������U�խp���ܲ���;
# �ϥ�formula����
# �p��C�өʧO�p�O���ܲ���
aggregate(tip ~ sex, data=tips, var)

# �{�b�˴��p�O�����G�O�_���`�A
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])

# �Υش��i�H�P�_�Ҧ��˴������q�L
ggplot(tips, aes(x=tip, fill=sex)) +
       geom_histogram(binwidth=.5, alpha=1/2)
ansari.test(tip ~ sex, tips)

# ---------------------------------------------------------- #

# �]�wvar.equal=TRUE�N�i��зǪ����˥�t�˩w
# �]�wvar.equal=FALSE(�w�])�h�i��Welch�˩w
t.test(tip ~ sex, data = tips, var.equal = TRUE)

# ---------------------------------------------------------- #

library(plyr)
tipSummary <- ddply(tips, "sex", summarize,
                    tip.mean=mean(tip), tip.sd=sd(tip),
                    Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                    Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))
tipSummary

# ---------------------------------------------------------- #

ggplot(tipSummary, aes(x=tip.mean, y=sex)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)

# ---------------------------------------------------------- #
# ---------
#  18-3-3
# ---------
# ---------------------------------------------------------- #

data(father.son, package='UsingR')
head(father.son)
t.test(father.son$fheight, father.son$sheight, paired = TRUE)

# ---------------------------------------------------------- #

heightDiff <- father.son$fheight - father.son$sheight
ggplot(father.son, aes(x=fheight - sheight)) +
       geom_density() +
       geom_vline(xintercept=mean(heightDiff)) +
       geom_vline(xintercept=mean(heightDiff) +
                  2*c(-1, 1)*sd(heightDiff)/sqrt(nrow(father.son)),
                  linetype=2)

# ---------------------------------------------------------- #
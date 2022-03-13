# ---------
#  21-3
# ---------
# ---------------------------------------------------------- #

library(boot)
# ��glm���s��house1�إ߼ҫ�,�Ӥ��A��lm
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro,
               data=housing, family=gaussian(link="identity"))

# �T�O����lm�����G�O�@�˪�
identical(coef(house1), coef(houseG1))

# ����5��(�s)����e����
houseCV1 <- cv.glm(housing, houseG1, K=5)

# �˵��~�t
houseCV1$delta

# ---------------------------------------------------------- #

# ��glm���s�إ߼ҫ�
houseG2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
houseG3 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + Class,
               data=housing)
houseG4 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class,
               data=housing)
houseG5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing)

# �����e����
houseCV2 <- cv.glm(housing, houseG2, K=5)
houseCV3 <- cv.glm(housing, houseG3, K=5)
houseCV4 <- cv.glm(housing, houseG4, K=5)
houseCV5 <- cv.glm(housing, houseG5, K=5)

## �˵��~�t���G
# �����G�إߤ@��data.frame
cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta,
                                 houseCV3$delta, houseCV4$delta,
                                 houseCV5$delta))

## �i��@�ǳB�z�H��n�a�e�{���G
# �����ƨ���n���W��
names(cvResults) <- c("Error", "Adjusted.Error")

# �[�J�ҫ��W��
cvResults$Model <- sprintf("houseG%s", 1:5)

# �˵����G
cvResults

# ---------------------------------------------------------- #

# ��ı�Ƶ��G
# ��ANOVA�˩w
cvANOVA <-anova(houseG1, houseG2, houseG3, houseG4, houseG5)
cvResults$ANOVA <- cvANOVA$`Resid. Dev`

# ��AIC���q
cvResults$AIC <- AIC(houseG1, houseG2, houseG3, houseG4, houseG5)$AIC

# �B�zdata.frame�H�Kø��
library(reshape2)
cvMelt <- melt(cvResults, id.vars="Model", variable.name="Measure",
               value.name="Value")
cvMelt

ggplot(cvMelt, aes(x=Model, y=Value)) +
       geom_line(aes(group=Measure, color=Measure)) +
       facet_wrap(~Measure, scales="free_y") +
       theme(axis.text.x=element_text(angle=90, vjust=.5)) +
       guides(color=FALSE)

# ---------------------------------------------------------- #

cv.work <- function(fun, k = 5, data,
                    cost = function(y, yhat) mean((y - yhat)^2),
                    response="y", ...)
   {
      # �ͦ���Ƹs
      folds <- data.frame(Fold=sample(rep(x=1:k, length.out=nrow(data))),
                          Row=1:nrow(data))

      # ���~�t����l�Ȭ�0
      error <- 0

      ## ��C�s��ƶi�歡�N
      ## �䤤��C�@�s���:
      ## �ΰV�m���(training data)�إ߼ҫ�
      ## �δ��ո��(testing data)�i��w��
      ## �p��~�t�ç⥦�ֿn�_��
      for(f in 1:max(folds$Fold))
      {
         # ������ո�ƪ���Ư���
         theRows <- folds$Row[folds$Fold == f]

         ## ��data[-theRows, ]����fun���
         ## ��data[theRows, ]���w��
         mod <- fun(data=data[-theRows, ], ...)
         pred <- predict(mod, data[theRows, ])

         # �ֿn�~�t,�åH�s������ƭӼƷ��v��
         error <- error +
         cost(data[theRows, response], pred) *
         (length(theRows)/nrow(data))
      }

   return(error)
   
   }

# ---------------------------------------------------------- #

cv1 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt + Boro)
cv2 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units * SqFt + Boro)
cv3 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + Class)
cv4 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class)
cv5 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
               formula=ValuePerSqFt ~ Boro + Class)

cvResults <- data.frame(Model=sprintf("house%s", 1:5),
                        Error=c(cv1, cv2, cv3, cv4, cv5))
cvResults

# ---------------------------------------------------------- #
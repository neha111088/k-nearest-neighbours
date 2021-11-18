View(glass)

glass <- glass[-10]
View(glass)

table(glass$Type)
str(glass)


round(prop.table(table(glass$Type)) * 100, digits = 1)

summary(glass[c("Na", "Si", "Ca")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

glass_n <- as.data.frame(lapply(glass[1:9], normalize))
View(glass_n)


summary(glass_n$Na)
summary(glass_n$Si)
summary(glass_n$Ca)

glass_train <- glass_n[1:170, ]
glass_test <- glass_n[170:214, ]


glass_train_labels <- glass[1:170, 1]
glass_train_labels


glass_test_labels <- glass[170:214, 1]
glass_test_labels


install.packages("class")
library(class)


glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=3)
glass_test_pred


install.packages("gmodels")
library(gmodels)



CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

confusionMatrix(glass_test_pred, glass_test_labels)


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=11)


CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=7)


CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

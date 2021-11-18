View(Zoo)

zoo <- Zoo[-1]
View(zoo)

table(zoo$animal.name)
str(zoo)


View(zoo)
str(zoo)


round(prop.table(table(zoo$animal.name)) * 100, digits = 1)

summary(zoo[c("hair", "milk", "backbone")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

zoo_n <- as.data.frame(lapply(Zoo[2:17], normalize))
View(zoo_n)

summary(zoo_n$hair)
summary(zoo_n$milk)
summary(zoo_n$backbone)


zoo_train <- zoo_n[1:70, ]

zoo_test <- zoo_n[71:101, ]

zoo_train_labels <- zoo[1:70, 1]
zoo_train_labels

zoo_test_labels <- zoo[71:101, 1]
zoo_test_labels


install.packages("class")
library(class)


zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=5)
zoo_test_pred


install.packages("gmodels")
library(gmodels)

CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)



zoo_z <- as.data.frame(scale(Zoo[-1]))
View(zoo_z)

summary(zoo_z$hair)


zoo_train <- zoo_z[1:70, ]
zoo_test <- zoo_z[71:101, ]


zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=3)


CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)



zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                     cl = zoo_train_labels, k=11)


CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)


zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                     cl = zoo_train_labels, k=21)


CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)

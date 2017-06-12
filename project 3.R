library(datasets)
data(iris)
head(iris)

library(e1071)
model1=svm(Species~.,data=iris, kernel="linear",probability=T)
summary(model1)
model1$coefs
model1$SV
model1$index
model1$fitted

model2=svm(Species~.,data=iris, kernel="linear", type="nu-classification")
summary(model2)
model2$index

#polynomial
model3=svm(Species~.,data=iris, kernel="polynomial")
summary(model3)

#polynomial
model4=svm(Species~.,data=iris, kernel="polynomial",degree=5,coef0= 0.7,gamma= 0.4, cost=10)
summary(model4)

#radial basis
model5=svm(Species~.,data=iris, kernel="radial")
summary(model5)

#tuning
model6=tune(svm, iris[,1:4],iris$Species,data=iris,kernel="polynomial",
            ranges=list(cost=c(10,14,17,100,1000),coef0=seq(0.01,0.05,0.01),
                        degree=c(2,3,4,5),gamma=c(0.2,0.3,0.4)))
summary(model6)
model6$best.parameters
model6$best.model

#predition
pre=predict(model3,iris[-5])
table(pre)
library(caret)
confusionMatrix(pre,iris$Species)

pre1=predict(model1,iris[-5],probability = T)
pre1
confusionMatrix(pre1,iris$Species)

plot(model1,iris,Petal.Length~Sepal.Length,symbolPalette = c("Red","Green","Blue"),fill=0)

iris1=iris[1:100,]
model=svm(Species~Petal.Length+Sepal.Length,data=iris1,kernel='linear')
plot(model,iris1,Petal.Length~Sepal.Length,symbolPalette = c("Red","Green"),fill=0,svSymbol = "S",dataSymbol = "D")
summary(model)

model_svm= svm(Loan_Status~.,data=X_train, kernel="radial")
summary(model_svm)

pred=predict(model_svm, X_train[-12])
confusionMatrix(pred,X_train$Loan_Status)


model_n=tune(svm,Loan_Status~.,data=X_train,kernel='linear', type='nu-classification',
               ranges=list(nu=c(0.3,0.4,0.5),gamma=c(0.05,0.07,0.2,0.25)))
summary(model_n)
pred1=predict(model_n$best.model,X_train[-12])

confusionMatrix(pred1,X_train$Loan_Status)
model_n$best.model

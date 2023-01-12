mydata <- read.csv("C:/Users/Owner/Desktop/dataset/creditcard.csv") 
View(mydata)

mydata <- mydata[,-1]

N <- nrow(mydata)

t <- sample(1:N,0.35*N)

df_training <- mydata[t,]
df_test <- mydata[-t,]

model <- glm(Class~.,data = df_training,family = "binomial")
probabilities <- predict(model,df_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)

length(which(predicted.classes == 0))
length(which(predicted.classes == 1))

length(which(df_test$Class == 1))

auc(as.factor(df_test$Class),predicted.classes)

model_svm <- svm(Class~.,data = df_training,type="C-classification",kernel="radial")
predicted.classes <- predict(model_svm,df_test)
auc(df_test$Class,as.numeric(predicted.classes))

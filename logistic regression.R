install.packages('ggplot2')
install.packages('pROC')

#load in the dataset
data1 <- read.csv(file='/finaldataset.csv')
head(data1)

#split into train and test dataset
set.seed(1234)
nn=0.8
data=data1
length(data1[,1])
sub<-sample(1:nrow(data),round(nrow(data)*nn))
length(sub)
data_train<-data1[sub,]
data_test<-data1[-sub,]
dim(data_train)
dim(data_test) 

#initial logistic regression
logistic = glm(formula = followback ~ from + factor(follow) + factor(likeornot)+likes, data = data_train, family = binomial)
print(summary(logistic))

#stepwise logistic regression
logisticstep<-step(logistic)
summary(logisticstep)
coef(logisticstep)
exp(coef(logisticstep))

#test the model performance
prob<-predict(object =logisticstep,newdata=data_test,type = "response")
pred<-ifelse(prob>=0.5,"1","0")
pred<-factor(pred,levels = c("0","1"),order=TRUE)
f<-table(data_test$followback,pred)
f

#plot ROC curve
library('pROC')
roc_curve <- roc(data_test$followback, prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') + geom_abline(intercept = 0, slope = 1) + annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2))) + labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')


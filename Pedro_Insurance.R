library(readxl)
Insurance<- read_excel("/Users/pedroandrade/Documents/ECON 386.xlsx")
View(Insurance)
Insurance$isFemale <- as.integer(Insurance$sex == "female")
Insurance$isSmoker <- as.integer(Insurance$smoker == "yes")
Insurance$isNorthwest <- as.integer(Insurance$region == "northwest")
Insurance$isNortheast <- as.integer(Insurance$region == "northeast")
Insurance$isSouthwest <- as.integer(Insurance$region == "southwest")
Insurance$isSoutheast <- as.integer(Insurance$region == "southeast")
summary(Insurance)
?cor
cor(Insurance$age, Insurance$charges)
cor(Insurance$bmi, Insurance$charges)
cor(Insurance$children, Insurance$charges)
cor(Insurance$isFemale, Insurance$charges)
cor(Insurance$isSmoker, Insurance$charges)
cor(Insurance$isSouthwest, Insurance$charges)
cor(Insurance$isSoutheast, Insurance$charges)
cor(Insurance$isNorthwest, Insurance$charges)
cor(Insurance$isNortheast, Insurance$charges)
plot(charges~bmi, Insurance)
model1<-lm(charges~ 0 + bmi, Insurance)
summary(model1)
RSS<-c(crossprod(model1$residuals))
MSE<-RSS/length(model1$residuals)
RMSE1<-sqrt(MSE)
RMSE1




##Splitting the data##
ind<-sample(2, nrow(Insurance), replace=TRUE, prob=c(0.7,0.3))
training<-Insurance[ind==1,]
testing<-Insurance[ind==2,]
dim(training)
dim(testing)
##model using training data##
trainmodel<-lm(charges~0+bmi, training)
summary(trainmodel)
trainmodel$coefficients
confint(trainmodel)

##Prediction##
pred<-predict(trainmodel,testing)
head(pred)
head(testing$charges)
View(pred)
RMSE=sqrt(sum((pred-testing$charges)^2)/(length(testing$charges)-1))
RMSE


x<-mean(Insurance$charges)
chargesbin<-ifelse(Insurance$charges >= x, 1, 0)
View(chargesbin)
lrmodel<-glm(chargesbin~0+bmi, data = Insurance, family = "binomial")
summary(lrmodel)


signal <- predict(lrmodel, Insurance)
pred_prob <- (1/(1 + exp(-signal)))
View(pred_prob)
?exp

confint(lrmodel)
confint.default(lrmodel)
point_conf_table<-cbind(lrmodel$coefficients, confint(lrmodel))
point_conf_table
exp(point_conf_table)
?sample

##trying to use bmi as binary variable##
bmimean<-mean(Insurance$bmi)
Insurance$cat.bmi<-ifelse(Insurance$bmi >= bmimean, 1, 0)
View(Insurance)
lrmodel2<-glm(chargesbin~cat.bmi, Insurance, family = "binomial")
summary(lrmodel2)


confint(lrmodel2)
confint.default(lrmodel2)
point_conf_table<-cbind(lrmodel2$coefficients, confint(lrmodel2))
point_conf_table
exp(point_conf_table)




cor(insurance$age, insurance$charges)
Age<- c(insurance["age"])
Age <-as.numeric( unlist(Age))
Charges<-  c(insurance["charges"])
Charges <- as.numeric( unlist(Charges))
plota(Age,Charges)
z = Age^2
Model5<- lm( formula = Charges ~ Age + z)
summary(Model5)
Age <- c(0:64)
Charges <- 6508.553 + 64.573 * x + 2.439 * x^2
y
plot(Age,Charges)
fit <- lm(formula = y ~ x + x^2 , data = insurance)
points(x, predict(fit), type = "l", col = 'red', lwd= 3)

confint(Model5)


library(caret)
set.seed(4321)
inTrain5 <- createDataPartition(y=insurance$charges, p=0.7, list=FALSE)
Training5 <- insurance[inTrain5,]
Testing5 <- insurance[-inTrain5,]
M5Train <- glm(chargebin~z, Training5, family = "binomial")
summary(M5Train)

exp(cbind(M5Train$coefficients, confint(M5Train)))
confusionMatrix(table(predict(M5Train, Training5, type="response") >= 0.5,
                      Training5$chargebin == 1))
confusionMatrix(table(predict(M5Train, Testing5, type="response") >= 0.5,
                      Testing5$chargebin == 1))


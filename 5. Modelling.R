library(dplyr)
library(randomForest)
library(ggplot2)
data <- read.csv("Output Files/completedata.csv",stringsAsFactors = F) %>% select(-c(city,X,Townhouse,shared_room))

ggplot(data,aes(price))+geom_bar() ##distribution is skewed to the right
data$price <- round(data$price,-1) #%>% log() #round and apply log function
ggplot(data,aes(price))+geom_bar()
data$bathrooms <- as.integer(data$bathrooms)

##Sample data
set.seed(123)
samp <- sample(nrow(data), 0.6 * nrow(data))
train <- data[samp, ]
test <- data[-samp, ]
rm(samp)



ggplot(data,aes(price,bedrooms))+
  geom_point() +
  geom_smooth(method='lm',formula=y~x)

ggplot(data,aes(price,beds))+
  geom_point() +
  geom_smooth(method='lm',formula=y~x)

ggplot(data,aes(price,bathrooms))+
  geom_point() +
  geom_smooth(method='lm',formula=y~x)

##Modelling
##Baseline model
best.guess <- mean(train$price) 
RMSE.baseline <- sqrt(mean((best.guess-test$price)^2))
MAE.baseline <- mean(abs(best.guess-test$price))


#Linear Regression
mod_lm <- lm (log(price)~ ., data = train)
summary(mod_lm)
lm_out <- predict(mod_lm,test) %>% as.data.frame()
lm_out$. <- exp(lm_out$.) %>% round(-1)
RMSE.lin.reg <- sqrt(mean((lm_out-test$price)^2))
MAE.lin.reg <- mean(abs(lm_out$.-test$price))



#Random Forest
model_rf <- randomForest(log(price)~.,data=train)
which.min(model_rf$mse) #500 trees needed to reach min error estimate
as.data.frame(sort(importance(model_rf)[,1],decreasing = TRUE),optional = T) #Features importance


plot(model_rf)
varImpPlot(model_rf,
           sort = T,
           main="Variable Importance",
           n.var=20)

rf_out <- predict(model_rf,test) %>% as.data.frame()
rf_out$. <- exp(rf_out$.) %>% round(-1)
RMSE.forest <- sqrt(mean((rf_out-test$price)^2))
MAE.forest <- mean(abs(rf_out$.-test$price))


#SVM
library(e1071) 
model_svm <- svm(log(price) ~ . , train)
svm_out <- predict(model_svm,test,interval='prediction') %>% as.data.frame()
svm_out$. <- exp(svm_out$.) %>% round(-1)
RMSE.svm <- sqrt(mean((svm_out-test$price)^2))
MAE.svm <- mean(abs(svm_out$.-test$price))




accuracy <- data.frame(Method = c("Baseline","Linear Regression","Random forest","SVM"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,RMSE.forest,RMSE.svm),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.forest,MAE.svm))
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy


all.predictions <- data.frame("actual" = test$price,
                              "baseline" = best.guess,
                              "linear.regression" = lm_out,
                              "random.forest" = rf_out,
                              "svm" = svm_out)

colnames(all.predictions) <- c("actual","baseline","linear.regression","random.forest","svm")
head(all.predictions)


library(tidyr)
all.predictions <- gather(all.predictions,key = model,value = predictions,2:5)
head(all.predictions)

ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  #coord_cartesian(xlim = c(0,1000),ylim = c(0,1000)) +
  ggtitle("Predicted vs. Actual, by model")


#SVM is the best model which produces the lowest RMSE
#Therefore, SVM will be the goto model during the ensemble modelling
test$predictedvalue <- 0
test$model <- NA
for(i in 1:nrow(test)){ 
  if(lm_out$.[i] == rf_out$.[i]) {
    test$predictedvalue[i] <-  lm_out$.[i]
    test$model[i] <- "LR/RF"
  } else {
    test$predictedvalue[i] <-  svm_out$.[i]
    test$model[i] <- "SVM"
  }
}

test %>% group_by(model) %>% tally()

RMSE.em <- sqrt(mean((test$predictedvalue-test$price)^2))
MAE.em <- mean(abs(test$predictedvalue-test$price))



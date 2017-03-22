set.seed(123)
samp <- sample(nrow(completedata), 0.6 * nrow(completedata))
train <- completedata[samp, ]
test <- completedata[-samp, ]

mod_lm <- lm (log(price)~ ., data = train)
summary(mod_lm)

pred <- exp(predict(mod_lm,test))-1
summary(pred) 

setwd("~/Desktop/NUS/Y3S1/ST3233/data")
suppressMessages(library(fpp))
library_list = c("ggplot2", "forecast", "Metrics", "modeest", "keras", "data.table", "quantmod", "TTR",
                 "e1071", "corrplot", "pROC", "FSelector", "kernlab", "klaR", "Boruta", "dplyr", "anytime", "knitr")
lapply(library_list, require, character.only = TRUE)

data = read.csv("euro.us.m1.201810.csv",header=TRUE)
data = data.frame(data)

#SVM Model (predicting direction)
macd.data = data.frame(MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent=FALSE))
sar = SAR(data[, c("High", "Low")], accel=c(0.02, 0.2)) 
trend = data$Close - sar
price = diff(data$Close, lag=1)
class = ifelse(price > 0, "UP", "DOWN")
class = c(NA, class)

histogram = macd.data$macd - macd.data$signal
histogram = c(NA, head(histogram, -1))

trend = c(NA, head(trend,-1))
model.data = data.frame(class, trend, histogram)
model.data = na.omit(model.data)

ndata = nrow(model.data)
n_train = as.integer(0.7*ndata)

train.data = model.data[1:n_train,]
test.data = model.data[n_train:ndata,]

SVM = svm(class ~ trend + histogram, data = train.data, kernel="radial", cost=1, gamma=1/2)
pred = predict(SVM, test.data, type="class")
compare.arr = data.frame(test.data, pred)
SVM.arr = accuracy(compare.arr$class, compare.arr$pred)
SVM.arr

ggplot(train.data, aes(x=histogram, y=trend)) + 
  stat_density2d(geom = "contour", aes(color=train.data)) +
  labs(title="SVM Predictions", x="MACD", y="Price - SAR", color="Training Predictions" )

#Feed forward NN
price = diff(data$Close)
class = ifelse(price > 0, "UP", "DOWN")
class = class[10:length(class)]

willr = WPR(data.frame(data$High, data$Low, data$Close), n=5) 
willr = c(NA, head(willr, -1))
willr = willr[11:length(willr)]

rsi5 = RSI(data$Close, n = 5, maType = "WMA") 
rsi5 = c(NA, head(rsi5, -1))
rsi5 = rsi5[11:length(rsi5)]

roc5 = ROC(data$Close, n = 5, type="continuous") * 100 
roc5 = c(NA, head(roc5, -1))
roc5 = roc5[11:length(roc5)]

atr5 = ATR(data[, c("High", "Low", "Close")], n=5, maType = "SMA")[,1] 
atr5 = c(NA, head(atr5, -1))
atr5 = atr5[11:length(atr5)]

dataset = data.frame(class, willr, atr5, rsi5)
dataset = na.omit(dataset)

train <- head(dataset, round(nrow(dataset) * 0.70)) 
h <- nrow(dataset) - nrow(train)
test <- tail(dataset, h) 
x.train = data.matrix(train[,-1])
x.valid = data.matrix(test[,-1])

y.train.temp = data.matrix(train[,1])
y.valid.temp = data.matrix(test[,1])

y.train = to_categorical(ifelse(y.train.temp == "UP", 1, 0))
y.valid = to_categorical(ifelse(y.valid.temp == "UP", 1, 0))

input = layer_input( shape = ncol(x.train))
output = input %>%
  layer_dense(units = 5, activation = "elu")  %>%
  layer_dense(units = 2, activation = "softmax") 

model = keras_model(inputs = input,
                    outputs = output)
model %>% summary  

model %>% compile(optimizer = "adam",
                  loss = "categorical_crossentropy",
                  metrics = c("accuracy")
)

learn.ff <- model %>% fit(x=x.train, y=y.train,
                          epochs = 50, 
                          batch_size = 256,
                          callbacks = list(
                            callback_reduce_lr_on_plateau()
                          ),
                          validation_data = list(x.valid,
                                                 y.valid))
plot(learn.ff)
pred = model %>% predict(x.valid)
learn.ff

#Logistic Regression
y.train = (ifelse(y.train == "UP", 1, 0))
y.valid = (ifelse(y.valid.temp == "UP", 1, 0))
y.train = as.factor(y.train.temp)
y.valid = as.factor(y.valid.temp)
x.train = data.frame(x.train)
x.valid = data.frame(x.valid)

glm.fit = glm(y.train ~., data = x.train, family = "binomial")
glm.probs = predict(glm.fit, x.valid, type="response")  

glm.pred =  rep("DOWN", length(y.valid))
glm.pred[glm.probs > 0.5] = "UP"
table(glm.pred, y.valid)
mean(glm.pred == y.valid)

#LDA
lda.fit <- lda(y.train ~ ., data=x.train) 
lda.pred <- predict(lda.fit, x.valid)
lda.class = lda.pred$class
table(lda.class, y.valid)
mean(lda.pred$class == y.valid)

#naive-bayes
nb.fit = naiveBayes(y.train~. , data=x.train)
nb.pred = predict(nb.fit, x.valid)

mean(nb.pred == y.valid)
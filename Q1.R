# Q1.

# A software company is interested in modelling and predicting its sales. 
# The data-set StartupSales.csv includes the quarterly sales of the company from 
# the first quarter of 1992 to the third quarter of 2017.


# a. Load the data into R and generate appropriate plot(s) to comment 
# on the stationary of the data. Is the data stationary? Why or why not?

sales.data <- read.csv("StartupSales.csv", header=FALSE)
sales <- sales.data$V1
sales.ts <- ts(sales, start = 1, frequency = 4)
par(mfrow = c(1, 2))
plot(sales, main = "Company Sales", ylab = "Sales", xlab = "Quarter", pch = 16,
     col = adjustcolor("black", 0.9))
acf(sales.ts, main = "ACF of sales")

# The data is not stationary because there is a clear quadratic trend in the plot 
# and a slow linear decay in the ACF plot


# b. Suppose the data is divided into test and training sets where the last 2 years 
# of data (8 quarters) is the test set and the remainder of the data is the training 
# set. Apply all possible versions of exponential smoothing on the training data. 
# If you identify seasonality, this means models with and without periodic component 
# and/or trend. Next, use the APSE on the test set to measure the quality of the 
# prediction. Provide the APSE of all the fitted models and justify your choice 
# among the models that you have tried.

n <- length(sales)
train <- sales[1:(n - 8)]
train.ts <- ts(train, start = 1, frequency = 4)
test <- sales[(n - 7):n]
test.ts <- ts(test, start = c(24, 4), frequency = 4)

# Simple exponential smoothing
es <- HoltWinters(train.ts, gamma = FALSE, beta = FALSE)
es
es.predict <- predict(es, n.ahead = 8)
es.predict
apse.es <- mean((test.ts - es.predict)^2)

# Double exponential smoothing
hw <- HoltWinters(train.ts, gamma = FALSE)
hw
hw.predict <- predict(hw, n.ahead = 8)
hw.predict
apse.hw <- mean((test.ts - hw.predict)^2)

# Triple exponential smoothing (additive)
hw.additive <- HoltWinters(train.ts, seasonal = "additive")
hw.additive
hw.additive.predict <- predict(hw.additive, n.ahead = 8)
hw.additive.predict
apse.hw.additive <- mean((test.ts - hw.additive.predict)^2)

# Triple exponential smoothing (multiplicative)
hw.multiplicative <- HoltWinters(train.ts, seasonal = "multiplicative")
hw.multiplicative
hw.multiplicative.predict <- predict(hw.multiplicative, n.ahead = 8)
hw.multiplicative.predict
apse.hw.multiplicative <- mean((test.ts - hw.multiplicative.predict)^2)

# Lowest APSE
apse <- c(apse.es, apse.hw, apse.hw.additive, apse.hw.multiplicative)
which.min(apse)
# Triple exponential smoothing (additive) performs best


# c.

fitted.model <- HoltWinters(sales.ts, seasonal = "additive")
pred.fitted <- predict(fitted.model, n.ahead = 12, prediction.interval = TRUE, level = 0.95)
plot(fitted.model, pred.fitted, main = "Triple Exponential Smoothing (Additive)")
pred.fitted


# d.
par(mfrow = c(1, 3))
hist(residuals(fitted.model), main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(fitted.model))
qqline(residuals(fitted.model))
acf(residuals(fitted.model), main = "ACF of Fitted Model")
# The residuals appear to be normal based on the Q-Q plot, histogram, and the 
# ACF plot, so Gaussian white noise is a reasonable assumption.


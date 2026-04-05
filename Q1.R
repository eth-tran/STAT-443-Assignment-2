# Q1.

# A software company is interested in modelling and predicting its sales. 
# The data-set StartupSales.csv includes the quarterly sales of the company from 
# the first quarter of 1992 to the third quarter of 2017.


# a. Load the data into R and generate appropriate plot(s) to comment 
# on the stationary of the data. Is the data stationary? Why or why not?

sales.data <- read.csv("StartupSales.csv", header=FALSE)
sales <- sales.data$V1
par(mfrow = c(1, 2))
plot(sales, main = "Company Sales", ylab = "Sales", xlab = "Quarter", pch = 16,
     col = adjustcolor("black", 0.9))
acf(sales.data$V1, main = "ACF of sales")

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
test <- sales[(n - 7):n]

# Simple exponential smoothing
es <- HoltWinters(train, gamma = FALSE, beta = FALSE)
es
es.predict <- predict(es, n.ahead = 8)
es.predict
apse.es <- mean((test - es.predict)^2)

# Double exponential smoothing
hw <- HoltWinters(train, gamma = FALSE)
hw
hw.predict <- predict(hw, n.ahead = 8)
hw.predict
apse.hw <- mean((test - hw.predict)^2)

apse.es
apse.hw
min(apse.es, apse.hw)
# Double exponential smoothing performs best
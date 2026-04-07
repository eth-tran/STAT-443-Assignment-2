# a.

# Load the data
sales.data <- read.csv("StartupSales.csv", header=FALSE)
sales <- sales.data$V1
sales.ts <- ts(sales, start = 1, frequency = 4)
par(mfrow = c(1, 2))
plot(sales, main = "Company Sales", ylab = "Sales", xlab = "Quarter", pch = 16,
     col = adjustcolor("black", 0.9))
acf(sales.ts, main = "ACF of sales")

# First order differencing

par(mfrow = c(1, 2))
diff.sales.ts = diff(sales.ts)  # one time differencing in lag 1
plot(diff.sales.ts, main = "First difference of sales ", ylab = "Sales",
     xlab = "Quarter", pch = 16, col = adjustcolor("black", 0.9))
acf(diff.sales.ts, main = "ACF of 1 time differenced data")

par(mfrow = c(1, 2))
diff2.sales.ts = diff(sales.ts, differences = 2)  # two times differencing in lag 1
plot(diff2.sales.ts, main = "Second difference of sales ",
     ylab = "Sales", xlab = "Quarter", pch = 16, col = adjustcolor("black", 0.9))
acf(diff2.sales.ts, main = "ACF of 2 times differenced data")
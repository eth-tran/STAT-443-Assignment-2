library(astsa)

dat <- read.csv(file = "Question3.csv", header = TRUE)
plot(dat, start = 1, frequency = 5)

log.dat <- ts(log(dat$x), start = 1, frequency = 5)
plot(log.dat)
# Looks like constant variance

train <- ts(log.dat[1:175], start = 1, frequency = 5)
test <- ts(log.dat[176:200], start = 176, frequency = 5)
validation.data <- dat$x[176:200]

# a.
par(mfrow = c(2, 1))
acf(log.dat, main = "ACF of log data")
pacf(log.dat)

diff.data <- diff(log.dat)
par(mfrow = c(3, 1))
plot(diff.data)
acf(diff.data)
pacf(diff.data)

diff2.data <- diff(diff.data, lag = 5)
par(mfrow = c(3, 1))
plot(diff2.data)
acf(diff2.data)
pacf(diff2.data)
# d = 1
# D = 1
# s = 5
# Cut off in non-seasonal ACF at lag 2, q = 2
# Cut off in non-seasonal PACF at lag 3, p = 3
# Exponential decay in seasonal ACF, cut off at lag 1, P = 1
# Propose SARIMA(3, 1, 2) x (1, 1, 0)_5 and simpler models

fit1 <- sarima(
  train,
  p = 3,
  d = 1,
  q = 2,
  P = 1,
  D = 1,
  Q = 0,
  S = 5
)
# AIC = -7.73831
# Ljung-Box violated

fit2 <- sarima(
  train,
  p = 2,
  d = 1,
  q = 2,
  P = 1,
  D = 1,
  Q = 0,
  S = 5
)
# AIC = -7.748116
# Ljung-Box violated

fit3 <- sarima(
  train,
  p = 4,
  d = 1,
  q = 2,
  P = 1,
  D = 1,
  Q = 0,
  S = 5
)
# AIC = -7.737129
# Ljung-Box violated

fit4 <- sarima(
  train,
  p = 3,
  d = 0,
  q = 2,
  P = 1,
  D = 1,
  Q = 0,
  S = 5
)
# AIC = -7.713363
# Ljung-Box violated

fit5 <- sarima(
  train,
  p = 3,
  d = 2,
  q = 2,
  P = 1,
  D = 1,
  Q = 0,
  S = 5
)
# AIC = -7.654659
# Ljung-Box violated

fit6 <- sarima(
  train,
  p = 3,
  d = 1,
  q = 2,
  P = 1,
  D = 0,
  Q = 0,
  S = 5
)
# AIC = -7.028604
# Ljung-Box violated

fit7 <- sarima(
  train,
  p = 3,
  d = 1,
  q = 2,
  P = 1,
  D = 2,
  Q = 0,
  S = 5
)
# AIC = -7.676129
# Ljung-Box violated

fit8 <- sarima(
  train,
  p = 3,
  d = 1,
  q = 2,
  P = 1,
  D = 1,
  Q = 1,
  S = 5
)
# AIC = -7.741773
# Ljung-Box violated

fit9 <- sarima(
  train, 
  p=3, 
  d=1, 
  q=3, 
  P=1, 
  D=1, 
  Q=0, 
  S=5)
# Ljung-Box violated

fit10 <- sarima(
  train, 
  p=4, 
  d=1, 
  q=3, 
  P=1, 
  D=1, 
  Q=0, 
  S=5)
# Good

plot.forecast <- function(p, d, q, P, D, Q, S) {
  sarima.for(
    train,
    n.ahead = 5,
    plot.all = TRUE,
    p,
    d,
    q,
    P,
    D,
    Q,
    S,
    ylab = "log(observed values)",
    pcol = adjustcolor("red", 0.5),
    pch = 16
  )
  title.plot = substitute(
    SARIMA(pv, dv, qv)(Pv, Dv, Qv)[Sv],
    list(pv = p, dv = d, qv = q, Pv = P, Dv = D, Qv = Q, Sv = S)
  )
  title(title.plot)
  points(
    log(validation.data),
    pch = 16,
    col = adjustcolor('blue', 0.6),
    cex = 0.7
  )
}
plot.forecast(p = 4, d = 1, q = 3, P = 1, D = 1, Q = 0, S = 5)


Compute.APSE <- function(p, d, q, P, D, Q, S) {
  model <- sarima.for(
    train,
    n.ahead = 5,
    plot = FALSE,
    p,
    d,
    q,
    P,
    D,
    Q,
    S
  )
  predicted.values.original.scale <- exp(as.numeric(model$pred))
  actual <- as.numeric(validation.data)
  print(mean((predicted.values.original.scale - actual)^2))
}
Compute.APSE(p = 4, d = 1, q = 3, P = 1, D = 1, Q = 0, S = 5)


library(forecast)
library(tseries)
##data-set
data <- read.csv('./nyc_taxi.csv')

timestamp <- data$timestamp
temp <- data$value

##data per day
y <- numeric(length = length(temp)/48)

len <- length(y)

for(i in c(1:len))
{
  y[i] <- 0
  for(j in 1:48)
  {
    y[i] <- y[i] + temp[48*(i-1) + j]
  }
}

## trend test
relativeOrderingTest <- function(values, alpha = 0.05)
{
  len = length(values)
  
  Q = 0
  for(i in 1:len)
  {
    for(j in (i+1):len)
    {
      if(i <= len && j <= len && values[i] > values[j])
        Q = Q + 1
    }
  }
  
  tau <- (1 - (4*Q / (len * (len - 1))))
  e_t <- 0
  var_t <- (2 * (2*len + 5)) / ((9*len) * (len-1))
  
  Z <- (tau - e_t)/(sqrt(var_t))
  z_a_2 <- qnorm((1 - alpha/2))
  
  if(abs(Z) > z_a_2)
  {
    print("Trend is present in data")
  }else
  {
    print("No trend is present in data")
  }
  
  Z <- (tau - e_t)/(sqrt(var_t))
  p_value <- pnorm(Z)
  return(p_value)
}

relativeOrderingTest(y)

##Testing for seasonal component
remove <- 1:5

data_matrix <- matrix(y[-remove], nrow = 7)
ncols = ncol(data_matrix)

seasonality_test <- friedman.test(data_matrix)
seasonality_test$p.value



len <- length(y)
t <- 1:len

# Making plot of full data-set
plot(t, y, type = 'l', xlab = 'time index', ylab = 'Passengers', main = 'Number of Passengers in a day')

analysis <- msts(y, c(7))

analysis <- mstl(analysis)
autoplot(analysis)


## checking for any remaining seasonality component
data_matrix <- matrix(analysis[-remove ,4], nrow = 7)
ncols = ncol(data_matrix)

friedman.test(data_matrix)


random <- analysis[ ,4]
mean(random)


## stationarity test
adf.test(random)
pp.test(random)
kpss.test(random)


##acf and pacf plots
acf(random)
pacf(random)

##arima model fitting
scaled_data <- scale(random)

ts_data <- ts(scaled_data, frequency=1)

#bic
bic_model <- auto.arima(ts_data, ic="bic", max.p=20,max.q=20,start.p=0,start.q=0)
summary(bic_model)

residuals <- resid(bic_model)


##anomaly detection
# Plot residuals
plot(residuals, type = "l", col = "red", lwd = 1, main = 'Residuals vs time')

# Identify anomalies based on a threshold (adjust as needed)
alpha = 0.025
anomaly_threshold <- qnorm(1-alpha)  # Adjust as needed
anomalies <- which(abs(residuals) > anomaly_threshold)
# Highlight anomalies on the plot
abline(h = c(anomaly_threshold, -anomaly_threshold), lty = 'dashed')
points(anomalies, residuals[anomalies], col = "blue", pch = 16)

anomalies_date <- data$timestamp[anomalies*48]
anomalies_date

anomalies_index <- as.data.frame(anomalies)
save(file = './anomalies.Rdata', anomalies_index)

data <- read.csv('./nyc_taxi.csv')
load("./anomalies.Rdata")

timestamp <- data$timestamp
y <- data$value
len <- length(y)
t <- 1:len

# Making plot of full dataset
plot(t, y, type = 'l', xlab = 'time index', ylab = 'Passengers')

# Making plot of first 1000 time index
plot(t, y, type = 'l', xlab = 'time index', ylab = 'Passengers', xlim = c(0, 1000), main = 'Half-hourly data for first 1000 observation')

# The data is daily data with time stamps at every hour
# Therefore period of the seasonal component is 24 time indices
div <- seq(0, len, by = 48)

# Making plot of first 1000 time index
abline(v = div, col = 'red')

data_matrix <- matrix(y, nrow = 48)
data_matrix <- data_matrix[ ,-anomalies_index$anomalies]
ncols = ncol(data_matrix)

weekday_sum <- numeric(length = 48)
weekend_sum <- numeric(length = 48)

number_of_weekday <- 0
number_of_weekend <- 0

for(i in 1:48)
{
  for(j in 1:ncols)
  {
    if((j %% 7 == 4) | (j %% 7 == 5))
    {
      weekend_sum[i] <- weekend_sum[i] + data_matrix[i, j]
      if(i == 1) number_of_weekend <- number_of_weekend + 1
    }
    else
    {
      weekday_sum[i] <- weekday_sum[i] +  data_matrix[i, j]
      if(i == 1) number_of_weekday <- number_of_weekday + 1
    }
  }
}

# Calculating weekday and weekend esimates
weekday_est <- weekday_sum / number_of_weekday
weekend_est <- weekend_sum / number_of_weekend

tstamp <- seq(0, 23.5, by = 0.5)
plot(tstamp, weekday_est, type = 'l', xlab = 'Time', ylab = 'Number of Customers per half hour', main = "Weekdays", xaxp = c(0, 24, 6))
points(tstamp, weekday_est, pch = 16)
div <- seq(0, 24, by = 4)
abline(v = div, col = 'red')

tstamp <- seq(0, 23.5, by = 0.5)
plot(tstamp, weekend_est, type = 'l', xlab = 'Time', ylab = 'Number of Customers per half hour', main = "Weekend", xaxp = c(0, 24, 6))
points(tstamp, weekend_est, pch = 16)
div <- seq(0, 24, by = 4)
abline(v = div, col = 'red')

plot(tstamp, weekday_est, type = 'l', xlab = 'Time', ylab = 'Number of Customers', col = 'red', xaxp = c(0, 24, 6), main = 'Variation in a day')
lines(tstamp, weekend_est, col = 'blue')
div <- seq(0, 24, by = 4)
abline(v = div, col = 'black', lty = 'dashed')
legend('bottomright', legend = c('Weekday', 'Weekend'), fill = c('red', 'blue'))

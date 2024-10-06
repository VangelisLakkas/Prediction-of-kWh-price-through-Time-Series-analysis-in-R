require(foreign)
require(xlsx)
library(readxl)
require(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
install.packages("forecast")
library(forecast)
install.packages("ISOweek")
library(ISOweek)
install.packages("tseries")
library(tseries)
library(stats)
library(lmtest)
library(TSA)
library(nortest)
install.packages("FinTS")
library(FinTS)


##### Loading the Data ####
var_names <- c("Date","Price")
energy_prices <-read_excel('electricity price data.xlsx', col_names = var_names)
class(energy_prices$Date)
energy_prices$Date <- as.Date(energy_prices$Date)
class(energy_prices$Date)
max(energy_prices$Price)
max(weekly_prices$Weekly_Mean_Price)

par(mfrow=c(1,1))
boxplot(energy_prices$Price, main="Daily Prices Outliers")
abline(h=0,col="red")
sum(is.na(energy_prices$Date))
sum(is.na(energy_prices$Price))

##### replacing false data with the mean price of the corresponding month #####
which(is.na(energy_prices$Price)) ## one missing value in date 11/03/2013
sum(energy_prices$Price < 0, na.rm = TRUE) ## negative values in rows 889,890
sum(energy_prices$Price > 200, na.rm = TRUE) ##irreasonably high price in date 16.08.2010 (equal to 1147)
sum(energy_prices$Price < 2, na.rm = TRUE)
which(energy_prices$Price > 200)
sum(energy_prices$Price == 0, na.rm = TRUE)

outliers_dates <- as.Date(c("2010-08-16", "2012-12-25", "2012-12-26", "2013-03-11"))
energy_prices <- energy_prices %>%
  mutate(Year = year(Date), Month = month(Date))

monthly_means <- energy_prices %>%
  filter(!(Date %in% outliers_dates) & !is.na(Price)) %>%
  group_by(Year, Month) %>%
  summarize(monthly_mean = mean(Price, na.rm = TRUE))

energy_prices <- energy_prices %>%
  left_join(monthly_means, by = c("Year", "Month")) %>%
  mutate(Price = ifelse(Date %in% outliers_dates | is.na(Price), monthly_mean, Price)) %>%
  select(-monthly_mean, -Year, -Month)

##### Aggregating the prices in weekly basis ####
energy_prices <- energy_prices %>%
  mutate(Year = year(Date), Week = isoweek(Date))

weekly_prices <- energy_prices %>%
  group_by(Year, Week) %>%
  summarize(Weekly_Mean_Price = mean(Price, na.rm = TRUE))

weekly_prices <- weekly_prices %>%
  mutate(Start_of_Week = ISOweek2date(paste0(Year, "-W", sprintf("%02d", Week), "-1")))


weekly_prices <- weekly_prices %>%
  select(Start_of_Week, Weekly_Mean_Price) %>%
  arrange(Start_of_Week)

plot(as.ts(energy_prices))
plot(as.ts(weekly_prices))

ggplot(weekly_prices, aes(x = Start_of_Week, y = Weekly_Mean_Price)) +
  geom_line() +
  labs(title = "Weekly Mean Energy Prices over time(2010-2018)",
       x = "Month",
       y = "Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sum(is.na(weekly_prices$Start_of_Week))
which(is.na(weekly_prices$Start_of_Week))
weekly_prices<-na.omit(weekly_prices)

weekly_prices_ts <- ts(weekly_prices$Weekly_Mean_Price, 
                       start = c(year(min(weekly_prices$Start_of_Week)), 
                                 week(min(weekly_prices$Start_of_Week))), 
                       frequency = 52)

kpss.test(weekly_prices_ts) ### null hypothesis for stationarity rejected



#### trying making series stationary only with differences ####
dweekly_prices_ts <- diff(weekly_prices_ts)
weekly_prices$diffed_price <- c(NA,diff(weekly_prices$Weekly_Mean_Price))
adf.test(dweekly_prices_ts) ### H0 rejected, p-value=0.01, stationarity reasonable assumption
kpss.test(dweekly_prices_ts) ### H0 not rejected, p-value=0.1, stationarity reasonable assumption

ggplot(weekly_prices, aes(x = Start_of_Week, y = diffed_price)) +
  geom_line() +
  labs(title = "Differenced Weekly Mean Energy Prices",
       x = "Week",
       y = "Differenced Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(decompose(dweekly_prices_ts, type = "additive"))

par(mfrow=c(1,2))
acf(as.numeric(dweekly_prices_ts))
pacf(as.numeric(dweekly_prices_ts))


log_weekly_prices <- weekly_prices
log_weekly_prices$Weekly_Mean_Price <- log(log_weekly_prices$Weekly_Mean_Price)

log_weekly_prices_ts <- ts(log_weekly_prices$Weekly_Mean_Price, 
                           start = c(year(min(log_weekly_prices$Start_of_Week)), 
                                     week(min(log_weekly_prices$Start_of_Week))), 
                           frequency = 52)

ggplot(log_weekly_prices, aes(x = Start_of_Week, y = Weekly_Mean_Price)) +
  geom_line() +
  labs(title = "Log-Transformed Weekly Mean Energy Prices",
       x = "Week",
       y = "Log Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

kpss.test(log_weekly_prices_ts) ### null hypothesis for stationarity rejected

log_weekly_prices$Weekly_Mean_Price <- c(NA, diff(log_weekly_prices$Weekly_Mean_Price))
diff_log_weekly_prices_ts <- diff(log_weekly_prices_ts)
sum(is.na(diff_log_weekly_prices_ts))
diff_log_weekly_prices_ts <- na.omit(diff_log_weekly_prices_ts)

kpss.test(diff_log_weekly_prices_ts) ### null hypothesis for stationarity couldn't be rejected
adf.test(diff_log_weekly_prices_ts)  ### null hypothesis rejected, stationarity reasonable assumption

ggplot(log_weekly_prices, aes(x = Start_of_Week, y = Weekly_Mean_Price)) +
  geom_line() +
  labs(title = "Differenced Log-Transformed Weekly Mean Energy Prices",
       x = "Week",
       y = "Differenced Log Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Aggregating the data in monthly basis ######
energy_prices <- energy_prices %>%
  mutate(Year_Month = format(Date, "%Y-%m"))
monthly_prices <- energy_prices %>%
  group_by(Year_Month) %>%
  summarise(Monthly_Mean_Price = mean(Price, na.rm = TRUE))

monthly_prices$Year_Month <- as.Date(paste0(monthly_prices$Year_Month, "-01"))
monthly_prices_ts <- ts(monthly_prices$Monthly_Mean_Price, 
                        start = c(year(min(monthly_prices$Year_Month)), month(min(monthly_prices$Year_Month))), 
                        frequency = 12)
print(monthly_prices_ts)

ggplot(monthly_prices, aes(x = Year_Month, y = Monthly_Mean_Price)) +
  geom_line() +
  labs(title = "Monthly Mean Energy Prices",
       x = "Month",
       y = "Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthly_prices_ts <- ts(monthly_prices$Monthly_Mean_Price, 
                        start = c(year(min(monthly_prices$Year_Month)), month(min(monthly_prices$Year_Month))), 
                        frequency = 12)
kpss.test(monthly_prices_ts)
adf.test(monthly_prices_ts)


log_monthly_prices <- monthly_prices
log_monthly_prices$Monthly_Mean_Price <- log(log_monthly_prices$Monthly_Mean_Price)
names(log_monthly_prices)[names(log_monthly_prices)=="Monthly_Mean_Price"] <- "Monthly_Mean_LogPrice"

log_monthly_prices_ts <- ts(log_monthly_prices$Monthly_Mean_LogPrice,start = c(year(min(log_monthly_prices$Year_Month)), month(min(log_monthly_prices$Year_Month))), 
                            frequency = 12)
kpss.test(log_monthly_prices_ts)
adf.test(log_monthly_prices_ts)
ggplot(log_monthly_prices, aes(x = Year_Month, y = Monthly_Mean_LogPrice)) +
  geom_line() +
  labs(title = "Monthly Mean Energy Log Prices",
       x = "Month",
       y = "Log Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


diff_log_monthly_prices_ts <- diff(log_monthly_prices_ts)
log_monthly_prices$Diff_Monthly_Mean_LogPrice <- c(NA, diff(log_monthly_prices$Monthly_Mean_LogPrice))
kpss.test(diff_log_monthly_prices_ts) 
adf.test(diff_log_monthly_prices_ts)

ggplot(log_monthly_prices, aes(x = Year_Month, y = Diff_Monthly_Mean_LogPrice)) +
  geom_line() +
  labs(title = "Monthly Mean Energy Log Prices",
       x = "Month",
       y = "Log Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dmonthly_prices_ts <- diff(monthly_prices_ts)
monthly_prices$diffed_price <- c(NA,diff(monthly_prices$Monthly_Mean_Price))
adf.test(dmonthly_prices_ts) ### H0 rejected, p-value=0.01, stationarity reasonable assumption
kpss.test(dmonthly_prices_ts) ### H0 not rejected, p-value=0.1, stationarity reasonable assumption

ggplot(monthly_prices, aes(x = Year_Month, y = diffed_price)) +
  geom_line() +
  labs(title = "Differenced Monthly Mean Energy Prices",
       x = "Week",
       y = "Differenced Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

decompose(weekly_prices_ts, type = "additive")
plot(decompose(weekly_prices_ts, type = "additive"))

hist(weekly_prices$Weekly_Mean_Price,ylim = c(0,200) ,main = "Histogram for Weekly Prices", col = "lightblue" ,xlab = "Weekly Mean Price")
hist(log_weekly_prices$Weekly_Mean_Price)
mean(weekly_prices$Weekly_Mean_Price) ; median(weekly_prices$Weekly_Mean_Price); kurtosis(weekly_prices$Weekly_Mean_Price)

decompose(diff_log_weekly_prices_ts, type = "additive")
plot(decompose(diff_log_weekly_prices_ts, type = "additive"))


###### Checking the autocorrelation plots for the order of the model ######
str(diff_log_weekly_prices_ts)
abc <- as.numeric(diff_log_weekly_prices_ts)
par(mfrow=c(1,1))
acf(abc,main = "ACF Plot for Differenced Log-Transformed Weekly Prices")
pacf(abc, main = "PACF Plot for Differenced Log-Transformed Weekly Prices")

def <- as.numeric(diff_log_monthly_prices_ts)
acf(def,main = "ACF Plot for Differenced Log-Transformed Monthly Prices")
pacf(def,main = "PACF Plot for Differenced Log-Transformed Monthly Prices")

length(test_data)

### Creating training and testing dataset ###
### for ex-post prediction and evaluation ###

n_obs <- length(diff_log_weekly_prices_ts)
n_obs
train_size <- floor(0.92 * n_obs) ### σπασιμο των δεδομενων για να εχω ενα 9μηνο για τεστ
train_data <- diff_log_weekly_prices_ts[1:train_size]
test_data <- diff_log_weekly_prices_ts[(train_size + 1):n_obs]

dn_obs <- length(dweekly_prices_ts)
dtrain_size <- floor(0.92 * dn_obs)
dtrain_data <- dweekly_prices_ts[1:dtrain_size]
dtest_data <- dweekly_prices_ts[(dtrain_size + 1):dn_obs]
ma1_train <- arima(dtrain_data, order = c(0,0,5), method = "ML")
ma1_train ### AIC=2693
forecast_length <- length(dtest_data)
actual_prices <- dtest_data
actual_prices

ma3_train <- arima(dtrain_data, order = c(0,0,2), method = "ML")
ma3_train ### AIC=2709

ma8_train <- arima(dtrain_data, order = c(0,0,8), method = "ML")
ma8_train  ### AIC=2696
length(actual_prices)

ma1_forecasts <- predict(ma1_train, n.ahead = forecast_length)$pred
ma1_forecasts
ma1_mae <- mean(abs(ma1_forecasts - actual_prices))
ma1_mae ## 4.14
ma1_mae/diff(range(actual_prices))
ma1_rmse<-sqrt(mean((ma1_forecasts - actual_prices)^2))
ma1_rmse ## 5.29

last_train_value <- weekly_prices_ts[dtrain_size + 1]

# Reverse the differencing to get forecasts on the original scale
ma1_forecasts_original_scale <- cumsum(c(last_train_value, ma1_forecasts))[-1]
ma1_forecasts_original_scale

range(actual_prices)
range(dtrain_data)
range(dtest_data)

#### Checking residuals for MA (q=5) ####
mean(ma1_train$residuals)
ma1_residuals <- residuals(ma1_train)
standardized_residuals <- (ma1_residuals - mean(ma1_residuals)) / sd(ma1_residuals)
ks.test(standardized_residuals, "pnorm")

par(mfrow=c(1,2))
qqnorm(ma1_residuals);qqline(ma1_residuals)
hist(ma1_residuals, main = "Histogram of MA(5) residuals")
skewness(ma1_residuals) ; kurtosis(ma1_residuals)

Box.test(ma1_train$residuals,lag = 5, type = "Ljung-Box")

# Plot residuals vs. fitted values
par(mfrow=c(1,1))
plot(fitted(ma1_train), ma1_residuals, 
     main = "Residuals vs Fitted Values of MA(5) model\n on differenced data", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")
checkresiduals(ma1_train,lag=3,test = FALSE)
ArchTest(dweekly_prices_ts,lag=52)

squared_ma1res <- ma1_residuals^2
acf(squared_ma1res, main = "ACF of Squared Residuals")
pacf(squared_ma1res, main = "PACF of Squared Residuals")

####################
log_actual_prices <- test_data
ma2_train <- arima(train_data, order = c(0,0,5), method = "ML",include.mean = TRUE)
summary(ma2_train)
ma2_train ### AIC=-404.59
ma2_forecasts <- predict(ma2_train, n.ahead = length(test_data))$pred
ma2_mae <- mean(abs(ma2_forecasts - log_actual_prices))
ma2_mae ## 16.94%
sqrt(mean((ma2_forecasts - log_actual_prices)^2))

log_actual_prices
range(log_actual_prices)

ma2_residuals <- residuals(ma2_train)
standardized_residuals2 <- (ma2_residuals - mean(ma2_residuals)) / sd(ma2_residuals)
ks.test(standardized_residuals2, "pnorm")

qqnorm(ma2_residuals);qqline(ma2_residuals)
hist(ma2_residuals)
skewness(ma2_residuals);kurtosis(ma2_residuals)
mean(ma2_train$residuals)

Box.test(ma2_residuals,lag = 3, type = "Ljung-Box")
par(mfrow=c(1,2))
# Plot residuals vs. fitted values
plot(fitted(ma2_train), ma2_residuals, 
     main = "Residuals vs Fitted Values of MA(5) model\n on differenced log-transformed data", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")
checkresiduals(ma2_train,lag=5,test = FALSE)


squared_ma2res <- ma2_residuals^2
acf(squared_ma2res, main = "ACF of Squared Residuals")
pacf(squared_ma2res, main = "PACF of Squared Residuals")

auto.arima(train_data,d=0,max.q = 8,max.p =13,stepwise = TRUE, method = "ML",ic="aic") ### AR(13), AIC=-363.32
auto.arima(dtrain_data,d=0,max.q = 8,max.p =13,stepwise = TRUE, method = "ML",ic="aic") ### MA(5), AIC=2354.56



max_p <- 13
max_q <- 8

best_aic1 <- Inf
best_model1 <- NULL
best_p1 <- NULL
best_q1 <- NULL

# Double iteration through all combinations of p and q on differenced data
for (p in 0:max_p) {
  for (q in 0:max_q) {
    model1 <- tryCatch({
      arima(dtrain_data, order = c(p, 0, q), method = "ML")
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(model1)) {
      aic1 <- AIC(model1)
      cat("ARIMA(", p, ",0,", q, ") AIC:", aic1, "\n")
      
      if (aic1 < best_aic1) {
        best_aic1 <- aic1
        best_model1 <- model1
        best_p1 <- p
        best_q1 <- q
      }
    }
  }
}

cat("Best model on differenced data: ARIMA(", best_p1, ",0,", best_q1, ") with AIC:", best_aic1, "\n")
best_model1 ### 2688.43

arma33 <- arima(dtrain_data, order = c(3,0,3), method = "ML")
arma33

arma33_forecasts <- predict(arma33, n.ahead = length(dtest_data))$pred
arma33_forecasts
arma33_mae <- mean(abs(arma33_forecasts - actual_prices))
arma33_mae ## 4.82
arma33_mae/diff(range(dtest_data)) # 17%

arma33_rmse<-sqrt(mean((arma33_forecasts - actual_prices)^2))
arma33_rmse ## 5.99
range(dtest_data)

arma33_residuals <- arma33$residuals
arma33_standarized_residuals <- (arma33_residuals - mean(arma33_residuals)) / sd(arma33_residuals)
mean(arma33_residuals)
ks.test(arma33_standarized_residuals, "pnorm")

qqnorm(arma33_residuals);qqline(arma33_residuals)
hist(arma33_residuals)
hist(arma33_standarized_residuals)
skewness(arma33_residuals);kurtosis(arma33_residuals)

mean_res <- mean(arma33_residuals)
sd_res <- sd(arma33_residuals)

x_values <- seq(min(arma33_residuals), max(arma33_residuals), length = 100)

normal_density <- dnorm(x_values, mean = mean_res, sd = sd_res)

hist(arma33_residuals, breaks = 30, freq = FALSE, main = "Histogram of ARMA(3,3) Residuals",
     xlab = "Residuals", ylab = "Density", col = "lightgray", border = "black")
lines(x_values, normal_density, col = "red", lwd = 2)

Box.test(arma33_residuals, type = "Ljung-Box")
par(mfrow=c(1,2))
# Plot residuals vs. fitted values
plot(fitted(arma33), arma33_residuals, 
     main = "Residuals vs Fitted Values of ARMA(3,3) model\n on differenced data", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")
checkresiduals(arma33,test = FALSE)

squared_ma2res <- (arma33_residuals)^2
acf(squared_ma2res, main = "ACF of Squared Residuals")
pacf(squared_ma2res, main = "PACF of Squared Residuals")







last_train_value <- weekly_prices_ts[dtrain_size + 1]

# Reversing the differences to get forecasts on the original scale
arma33_forecasts_original_scale <- cumsum(c(last_train_value, arma33_forecasts))[-1]
arma33_forecasts_original_scale

length(arma33_forecasts_original_scale)

farma33 <- arima(dweekly_prices_ts, order = c(0,0,5),method = "ML")
farma33
final_arma33_forecasts <- predict(farma33, n.ahead = 26)$pred
final_arma33_forecasts

last_train_value2 <- tail(weekly_prices_ts,1)

final_arma33_forecasts_original_scale <- cumsum(c(last_train_value2, final_arma33_forecasts))[-1]
final_arma33_forecasts_original_scale


PR <- predict(arma33, n.ahead = 24)
UL<- PR$pred+PR$se
LL<-  PR$pred-PR$se
LL

forecast_data$Month <- format(forecast_data$Start_of_Week, "%Y-%m")
monthly_forecast_data <- aggregate(Weekly_Mean_Price ~ Month, data = forecast_data, mean)
colnames(monthly_forecast_data) <- c("Month", "Mean_Monthly_Price")
monthly_forecast_data


forecast_dates <- seq(from = as.Date("2019-01-01"), by = "week", length.out = length(final_arma33_forecasts_original_scale))
forecast_data <- data.frame(Start_of_Week = forecast_dates, Weekly_Mean_Price = final_arma33_forecasts_original_scale)
combined_data <- rbind(weekly_prices, forecast_data)

last_date <- max(weekly_prices$Start_of_Week)
start_date <- as.Date(cut(last_date, breaks = "years")) - years(1)
filtered_data <- weekly_prices %>%
  filter(Start_of_Week >= start_date)

ggplot() +
  geom_line(data = filtered_data, aes(x = Start_of_Week, y = Weekly_Mean_Price), color = "blue") +
  geom_line(data = forecast_data, aes(x = Start_of_Week, y = Weekly_Mean_Price), color = "red") +
  labs(title = "Weekly Mean Energy Prices with Forecasts for 2019",
       x = "Week",
       y = "Mean Price (Euros)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



par(mfrow=c(1,1))



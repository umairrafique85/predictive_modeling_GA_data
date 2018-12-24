# Basic Time series forecasting to set goals for future, or analyze performance
# if data further in the past available for training. Using prophet

viewId_champion <- 14165234

# data pull

df_sessGoal8_2yrs <- google_analytics(viewId_champion, 
                                      date_range = c("2017-02-01", 'today'),
                                      dimensions = 'date', 
                                      metrics = c('sessions', 'goal8Completions'),
                                      anti_sample = TRUE)
# set parameters. Right now evaluating 2018's performance, after training on data
# from 2017. The validation period is not for validation as in Machine Learning.

train_start <- "2017-02-01"
train_end <- "2018-02-28"
valid_start <- "2018-03-01"
valid_end <- "2018-12-18"
forecast_period <- as.integer(as.Date(valid_end) - as.Date(valid_start))

# plot goal8 conversions over time

ggplotly(ggplot(df_sessGoal8_2yrs, aes(date, goal8Completions)) + 
           geom_line())

# prophet only wants ds and y
train_data <- df_sessGoal8_2yrs %>% 
  select(ds = date, y = goal8Completions) %>% 
  filter(ds >= train_start & ds <= train_end)

validation_data <- df_sessGoal8_2yrs %>% 
  select(ds = date, y = goal8Completions) %>% 
  filter(ds >= valid_start & ds <= valid_end)

# done with foreplay. Training, and forecast:
tsModel <- prophet(train_data, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
future <- make_future_dataframe(tsModel, periods = forecast_period)
forecast <- predict(tsModel, future)

# plot predictions for training and validation periods, with dots representing
# the training observations
ggplotly(plot(tsModel, forecast))

# plot predictions against actual data. Black line representing actual.
ggplotly(ggplot(forecast, aes(ds, yhat)) + 
           geom_line(col = 'deepskyblue1') + 
           geom_linerange(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.25) + 
           geom_line(data = df_sessGoal8_2yrs[-686,], aes(as.POSIXct.Date(date), goal8Completions)))

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], n = forecast_period * 2)

# Total forecast in numbers
total_forecast <- forecast %>%
  filter(ds >= valid_start & ds <= valid_end) %>%
  summarise(transactionRevenue = sum(yhat))
library(tidyverse)

# Read the data and select relevant columns
df <-
  read_csv("https://s3.mortality.watch/data/mortality/GBRTENW/yearly.csv") |>
  select(date, deaths, cmr, asmr_country)

# Split the data into training and test sets
train <- df |> filter(date >= 2017, date < 2020)
test <- df |> filter(date >= 2020)

# Fit the linear regression model
model <- lm(asmr_country ~ date, data = train)

# Calculate prediction intervals for the test data
predictions <- predict(model, newdata = test, interval = "prediction")

# Calculate standard errors for the predictions
standard_errors <- predict(model, newdata = test, se.fit = TRUE)$se.fit

# Calculate z-values
z_values <- (test$asmr_country - predictions[, 1]) / standard_errors

# Calculate p-values
p_values <- 2 * (1 - pnorm(abs(z_values)))
# Assuming 'result' contains the rounded predictions and p-values
result <- data.frame(round(predictions), round(p_values, 3))
result$date <- test$date

# Create the ggplot
ggplot() +
  geom_line(data = train, aes(x = date, y = asmr_country), color = "black") +
  geom_line(data = test, aes(x = date, y = asmr_country), color = "red") +
  geom_line(data = result, aes(x = date, y = fit), color = "blue") +
  geom_ribbon(
    data = result,
    aes(x = date, y = fit, ymin = lwr, ymax = upr),
    fill = "blue", alpha = 0.3
  ) +
  geom_text(
    data = result,
    aes(
      x = date,
      y = fit,
      label = paste("p =", round.p_values..3.)
    ), vjust = 2
  )

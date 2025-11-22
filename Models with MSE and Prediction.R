library(dplyr)
library(lubridate)

# 0. Read your data (adjust path if needed)
df_raw <- read.csv("1 Month SPY Clean.csv")

# 1. Basic cleaning: keep rows with non-missing transactions
df <- df_raw %>%
  filter(!is.na(transactions)) %>%
  mutate(
    datetime      = ymd_hms(datetime_et),
    minute_of_day = hour(datetime) * 60 + minute(datetime)
  ) %>%
  arrange(datetime)

# 2. Create lags for Model 3 (Hawkes-type)
df3 <- df %>%
  mutate(
    lag1 = dplyr::lag(transactions, 1),
    lag2 = dplyr::lag(transactions, 2)
  ) %>%
  filter(!is.na(lag1), !is.na(lag2))  # drop first few rows with missing lags

set.seed(123)  # for reproducibility of any random steps later (not strictly needed here)

n <- nrow(df3)
split_idx <- floor(0.7 * n)

train <- df3[1:split_idx, ]
test  <- df3[(split_idx + 1):n, ]

mse <- function(actual, pred) {
  mean((actual - pred)^2)
}

# Model 1: Poisson GLM with intercept only
mod1 <- glm(
  transactions ~ 1,
  family = poisson(link = "log"),
  data   = train
)
summary(mod1)


# Model 2: Poisson GLM with time-of-day effect
mod2 <- glm(
  transactions ~ minute_of_day,
  family = poisson(link = "log"),
  data   = train
)
summary(mod2)

# Model 3: Poisson autoregression with time-of-day + lagged counts
mod3 <- glm(
  transactions ~ minute_of_day + lag1 + lag2,
  family = poisson(link = "log"),
  data   = train
)
summary(mod3)


# Model 1 predictions & MSE
pred1 <- predict(mod1, newdata = test, type = "response")
mse1  <- mse(test$transactions, pred1)

# Model 2 predictions & MSE
pred2 <- predict(mod2, newdata = test, type = "response")
mse2  <- mse(test$transactions, pred2)

# Model 3 predictions & MSE
pred3 <- predict(mod3, newdata = test, type = "response")
mse3  <- mse(test$transactions, pred3)

mse1; mse2; mse3
pred1; pred2; pred3
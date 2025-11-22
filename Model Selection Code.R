library(dplyr)

# Model 1: Homogeneous Poissson Model
df<- read.csv("1 Month SPY Clean.csv")
df1 <- df %>%
  filter(!is.na(transactions))

# Simple estimate:
lambda_hat <- mean(df1$transactions)

# As Poisson GLM with intercept only:
mod1 <- glm(transactions ~ 1,
            family = poisson(link = "log"),
            data = df1)
summary(mod1)
exp(coef(mod1)[1])  # estimated lambda


# Model 2: Inhomogeneous Poisson Model

library(lubridate)
library(mgcv)

df2 <- df1 %>%
  mutate(
    datetime = ymd_hms(datetime_et),
    minute_of_day = hour(datetime) * 60 + minute(datetime)
  )

mod2_gam <- gam(transactions ~ s(minute_of_day),
                family = poisson(link = "log"),
                data = df2)
summary(mod2_gam)
plot(mod2_gam)   # shows trade intensity vs time of day

# Model 3: Hawkes Type Model

df2.5 <- df %>%
  filter(!is.na(transactions)) %>%
  mutate(
    datetime      = ymd_hms(datetime_et),
    minute_of_day = hour(datetime) * 60 + minute(datetime)
  )

df3 <- df2 %>%
  arrange(datetime) %>%
  mutate(
    lag1 = dplyr::lag(transactions, 1),
    lag2 = dplyr::lag(transactions, 2)
  ) %>%
  filter(!is.na(lag1), !is.na(lag2))

# Model 3: Poisson autoregression with time-of-day (linear)
mod3 <- glm(
  transactions ~ minute_of_day + lag1 + lag2,
  family = poisson(link = "log"),
  data   = df3
)

summary(mod3)

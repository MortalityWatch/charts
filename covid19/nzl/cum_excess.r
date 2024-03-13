source("./lib/common.r")

# https://stackoverflow.com/a/39338512/2302437
lm_predict <- function(lmObject, newdata, diag = TRUE) {
  # input checking
  if (!inherits(lmObject, "lm")) stop("'lmObject' is not a valid 'lm' object!")
  # extract "terms" object from the fitted model, but delete response variable
  tm <- delete.response(terms(lmObject))
  # linear predictor matrix
  Xp <- model.matrix(tm, newdata)
  # predicted values by direct matrix-vector multiplication
  pred <- c(Xp %*% coef(lmObject))

  # efficiently form the complete variance-covariance matrix
  QR <- lmObject$qr # qr object of fitted model
  piv <- QR$pivot # pivoting index
  r <- QR$rank # model rank / numeric rank
  if (is.unsorted(piv)) {
    # pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    # no pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }
  # residual variance
  sig2 <- c(crossprod(residuals(lmObject))) / df.residual(lmObject)
  if (diag) {
    # return point-wise prediction variance
    VCOV <- colSums(B^2) * sig2
  } else {
    # return full variance-covariance matrix of predicted values
    VCOV <- crossprod(B) * sig2
  }
  list(
    fit = pred,
    var.fit = VCOV,
    df = lmObject$df.residual,
    residual.var = sig2
  )
}

# https://stackoverflow.com/a/39338512/2302437
agg_pred <- function(w, predObject, alpha = 0.95) {
  # input checing
  if (length(w) != length(predObject$fit)) stop("'w' has wrong length!")
  if (!is.matrix(predObject$var.fit)) {
    stop("'predObject' has no variance-covariance matrix!")
  }
  # mean of the aggregation
  agg_mean <- c(crossprod(predObject$fit, w))
  # variance of the aggregation
  agg_variance <- c(crossprod(w, predObject$var.fit %*% w))
  # adjusted variance-covariance matrix
  VCOV_adj <- with(predObject, var.fit + diag(residual.var, nrow(var.fit)))
  # adjusted variance of the aggregation
  agg_variance_adj <- c(crossprod(w, VCOV_adj %*% w))
  # t-distribution quantiles
  Qt <- c(-1, 1) * qt((1 - alpha) / 2, predObject$df, lower.tail = FALSE)
  # names of CI and PI
  NAME <- c("lower", "upper")
  # CI
  CI <- setNames(agg_mean + Qt * sqrt(agg_variance), NAME)
  # PI
  PI <- setNames(agg_mean + Qt * sqrt(agg_variance_adj), NAME)
  # return
  list(mean = agg_mean, var = agg_variance, CI = CI, PI = PI)
}

# Yearly NZ data
df <- read.csv("https://s3.mortality.watch/data/mortality/NZL/yearly.csv")
nz <- df |>
  filter(iso3c == "NZL") |>
  as_tibble() |>
  select(date, deaths) |>
  mutate(year = as.integer(left(date, 4))) |>
  filter(year >= 2010)

# Split data into training and test
df_train <- nz |> filter(year <= 2019)
df_test <- nz |> filter(year > 2019)

# Model
model <- lm(deaths ~ year, data = df_train)

# Forecast
oo <- lm_predict(model, df_test, FALSE)
fc_sum_mean <- sum(oo$fit)
fc_sum_variance <- sum(oo$var.fit)

n <- ncol(lengths(oo$var.fit))
res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)

print(paste0(
  "Actual deaths: ", sum(df_test$deaths),
  ", Expected: ", round(fc_sum_mean),
  ", 95%PI[", round(res$PI[1]), ",", round(res$PI[2]), "]"
))
print(paste0(
  "Excess deaths: ", round(sum(df_test$deaths) - fc_sum_mean),
  ", 95%PI[", sum(df_test$deaths) - round(res$PI[2]), ",",
  sum(df_test$deaths) - round(res$PI[1]), "]"
))

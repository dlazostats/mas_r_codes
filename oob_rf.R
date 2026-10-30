library(ranger)
library(MASS)

set.seed(0)
n <- nrow(Boston)
train_idx <- sample(n, size = 0.75 * n)
train <- Boston[train_idx, ]
test  <- Boston[-train_idx, ]

rg <- ranger(medv ~ ., data = train, num.trees = 500, seed = 0)

# OOB and test RMSE
oob_rmse  <- sqrt(rg$prediction.error)
test_pred <- predict(rg, data = test)$predictions
test_rmse <- sqrt(mean((test$medv - test_pred)^2))

# Bootstrap the test set: resample (truth, prediction) pairs, recompute RMSE
B <- 2000
boot_rmse <- replicate(B, {
  idx <- sample(nrow(test), replace = TRUE)
  sqrt(mean((test$medv[idx] - test_pred[idx])^2))
})
hist(boot_rmse,breaks="FD")
ci <- quantile(boot_rmse, c(0.025, 0.975))

cat(sprintf("OOB RMSE:  %.3f\n", oob_rmse))
cat(sprintf("Test RMSE: %.3f  (95%% CI: %.3f to %.3f)\n", test_rmse, ci[1], ci[2]))

library(boot)

rmse_stat <- function(d, idx) sqrt(mean((d$y[idx] - d$pred[idx])^2))
d <- data.frame(y = test$medv, pred = test_pred)

b <- boot(d, rmse_stat, R = 2000)
boot.ci(b, type = c("perc", "bca"))

library(ISLR)
library(leaps)
library(glmnet)

# Filters out players with missing values
filtered_hitters <- na.omit(Hitters)
# Preprocessing data
x <- model.matrix(Salary ~ ., filtered_hitters)[, -1] # Remove salary column
y <- filtered_hitters$Salary


# Linear Regression
fit_ls <- lm(Salary ~ ., data = filtered_hitters)
ls_summary <- summary(fit_ls)


# Best Subsets
best_sub <- regsubsets(Salary ~ .,
                        data = filtered_hitters,
                        nvmax = 19)
best_sub_summary <- summary(best_sub)


# Step-wise forward
forward <- regsubsets(Salary ~ .,
                        data = filtered_hitters,
                        nvmax = 19,
                        method = "forward")
forward_summary <- summary(forward)


# Step-wise backward
back <- regsubsets(Salary ~ .,
                        data = filtered_hitters,
                        nvmax = 19,
                        method = "backward")
back_summary <- summary(back)


# Ridge Regression
fitr <- glmnet(x = x, y = y, family = "gaussian", alpha = 0)
plot(fitr, col = 1:18)
legend(0, -25,
        legend = names(filtered_hitters)[1:18],
        col = 1:18,
        lty = rep(1, 18))


# LASSO
fitl <- glmnet(x = x, y = y, family = "gaussian", alpha = 1)
plot(fitl, col = 1:18)
names(filtered_hitters)
legend(0, -25,
        legend = names(filtered_hitters)[1:18],
        col = 1:18,
        lty = rep(1, 18))


# Elastic Net
fite1 <- glmnet(x = x, y = y, family = "gaussian", alpha = 0.66)
plot(fite1, col = 1:18)
legend(0, -25,
        legend = names(filtered_hitters)[1:18],
        col = 1:18,
        lty = rep(1, 8))
fite2 <- glmnet(x = x, y = y, family = "gaussian", alpha = 0.33)
plot(fite2, col = 1:18)
legend(0, -25,
        legend = names(filtered_hitters)[1:18],
        col = 1:18,
        lty = rep(1, 18))

# Adaptive Lasso
fitrcv <- cv.glmnet(x = x, y = y,
                       type.measure = "mse",
                       nfold = 10,
                       alpha = 0)
bestrcoef <- as.numeric(coef.glmnet(fitrcv, s = fitrcv$lambda.min))[-1]
fitalasso <- glmnet(x = x, y = y,
                        alpha = 1,
                        type.measure = "mse",
                        penalty.factor = 1 / abs(bestrcoef))

alassocoef <- coef.glmnet(fitalasso, s = fitrcv$lambda.min)

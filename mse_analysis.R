library(ISLR)
library(leaps)
library(glmnet)
library(splitTools)


# Filters out players with missing values
filtered_hitters <- na.omit(Hitters)

# Linear Regression
lsmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)),
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    fit_ls <- lm(Salary ~ ., data = data$train)
    lspredict <- predict.lm(fit_ls,
                        data.frame(c(data$test[1:18], data$test[20])))
    lsmse <- lsmse + mean((data$test$Salary - lspredict)^2)
}
lsmse <- lsmse / 10


# Ridge Regression
ridgemse <- Inf
rlambda <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
y <- data$train$Salary
fitr <- glmnet(x = x, y = y, family = "gaussian", alpha = 0)
# Minimizes validation error
for (i in length(fitr$lambda)) {
    rmat <- data.matrix(data.frame(c(data$validate[1:18], data$validate[20])))
    rpredict <- predict.glmnet(fitr, newx = rmat, lambda = fitr$lambda[i])
    ridgepotmse <- mean((data$validate$Salary - rpredict)^2)
    if (ridgepotmse < ridgemse) {
        ridgemse <- ridgepotmse
        rlambda <- fitr$lambda[i]
    }
}

ridgemse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
    y <- data$train$Salary
    fitr <- glmnet(x = x, y = y, family = "gaussian",
                    alpha = 0, lambda = rlambda)
    rmat <- data.matrix(data.frame(c(data$test[1:18], data$test[20])))
    rpredict <- predict.glmnet(fitr, newx = rmat)
    ridgemse <- ridgemse + mean((data$test$Salary - rpredict)^2)
}
ridgemse <- ridgemse / 10


# Best Subsets
bsmse <- Inf
bsmodelidx <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
best_sub <- regsubsets(Salary ~ .,
                    data = data$train,
                    nvmax = 19)
# Minimizing validation error
for (i in 1:19) {
    bsmat <- model.matrix(as.formula(best_sub$call[[2]]),
                        data.frame(c(data$validate)))
    bscoef <- coefficients(best_sub, id = i)
    bspredict <- bsmat[, names(bscoef)] %*% bscoef
    bspotentialmse <- mean((data$validate$Salary - bspredict)^2)
    if (bspotentialmse < bsmse) {
        bsmse <- bspotentialmse
        bsmodelidx <- i
    }
}

bsmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    best_sub <- regsubsets(Salary ~ .,
                        data = data$train,
                        nvmax = 19)
    bsmat <- model.matrix(as.formula(best_sub$call[[2]]),
                        data.frame(c(data$test)))
    bscoef <- coefficients(best_sub, id = bsmodelidx)
    bspredict <- bsmat[, names(bscoef)] %*% bscoef
    bsmse <- bsmse + mean((data$test$Salary - bspredict)^2)
}
bsmse <- bsmse / 10


# Stepwise forward
sfmse <- Inf
sfmodelidx <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
forward <- regsubsets(Salary ~ .,
                        data = data$train,
                        nvmax = 19,
                        method = "forward")
# Minimizing validation error
for (i in 1:19) {
    sfmat <- model.matrix(as.formula(forward$call[[2]]),
                        data.frame(c(data$validate)))
    sfcoef <- coefficients(forward, id = i)
    sfpredict <- sfmat[, names(sfcoef)] %*% sfcoef
    sfpotentialmse <- mean((data$validate$Salary - sfpredict)^2)
    if (sfpotentialmse < sfmse) {
        sfmse <- sfpotentialmse
        sfmodelidx <- i
    }
}

sfmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    forward <- regsubsets(Salary ~ .,
                        data = data$train,
                        nvmax = 19,
                        method = "forward")
    sfmat <- model.matrix(as.formula(forward$call[[2]]),
                        data.frame(c(data$test)))
    sfcoef <- coefficients(forward, id = sfmodelidx)
    sfpredict <- sfmat[, names(sfcoef)] %*% sfcoef
    sfmse <- sfmse + mean((data$test$Salary - sfpredict)^2)
}
sfmse <- sfmse / 10


# Stepwise backward
sbmse <- Inf
sbmodelidx <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
backward <- regsubsets(Salary ~ .,
                        data = data$train,
                        nvmax = 19,
                        method = "backward")
# Minimizing validation error
for (i in 1:19) {
    sbmat <- model.matrix(as.formula(backward$call[[2]]),
                        data.frame(c(data$validate)))
    sbcoef <- coefficients(backward, id = i)
    sbpredict <- sbmat[, names(sbcoef)] %*% sbcoef
    sbpotentialmse <- mean((data$validate$Salary - sbpredict)^2)
    if (sbpotentialmse < sbmse) {
        sbmse <- sbpotentialmse
        sbmodelidx <- i
    }
}

sbmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    backward <- regsubsets(Salary ~ .,
                        data = data$train,
                        nvmax = 19,
                        method = "backward")
    sbmat <- model.matrix(as.formula(backward$call[[2]]),
                        data.frame(c(data$test)))
    sbcoef <- coefficients(backward, id = sbmodelidx)
    sbpredict <- sbmat[, names(sbcoef)] %*% sbcoef
    sbmse <- sbmse + mean((data$test$Salary - sbpredict)^2)
}
sbmse <- sbmse / 10


# Lasso
lmse <- Inf
llambda <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
y <- data$train$Salary
fitl <- glmnet(x = x, y = y, family = "gaussian", alpha = 1)
# Minimizes validation error
for (i in length(fitl$lambda)) {
    lmat <- data.matrix(data.frame(c(data$validate[1:18], data$validate[20])))
    lpredict <- predict.glmnet(fitl, newx = lmat, lambda = fitl$lambda[i])
    lpotmse <- mean((data$validate$Salary - lpredict)^2)
    if (lpotmse < lmse) {
        lmse <- lpotmse
        llambda <- fitl$lambda[i]
    }
}

lmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
    y <- data$train$Salary
    fitl <- glmnet(x = x, y = y, family = "gaussian",
                    alpha = 1, lambda = llambda)
    lmat <- data.matrix(data.frame(c(data$test[1:18], data$test[20])))
    lpredict <- predict.glmnet(fitl, newx = lmat)
    lmse <- lmse + mean((data$test$Salary - lpredict)^2)
}
lmse <- lmse / 10


# Elastic Net
enmse <- Inf
enlambda <- 0
# Splits data
partition <- c(train = .6, test = .2, validate = .2)
split <- sample(cut(
seq(nrow(filtered_hitters)),
nrow(filtered_hitters) * cumsum(c(0, partition)),
labels = names(partition)
))
data <- split(filtered_hitters, split)
# Fits the model
x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
y <- data$train$Salary
fiten <- glmnet(x = x, y = y, family = "gaussian", alpha = 0.66)
# Minimizes validation error
for (i in length(fiten$lambda)) {
    enmat <- data.matrix(data.frame(c(data$validate[1:18], data$validate[20])))
    enpredict <- predict.glmnet(fiten, newx = enmat, lambda = fiten$lambda[i])
    enpotmse <- mean((data$validate$Salary - enpredict)^2)
    if (enpotmse < enmse) {
        enmse <- enpotmse
        enlambda <- fiten$lambda[i]
    }
}

enmse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
    y <- data$train$Salary
    fiten <- glmnet(x = x, y = y, family = "gaussian",
                    alpha = 1, lambda = enlambda)
    enmat <- data.matrix(data.frame(c(data$test[1:18], data$test[20])))
    enpredict <- predict.glmnet(fiten, newx = enmat)
    enmse <- enmse + mean((data$test$Salary - enpredict)^2)
}
enmse <- enmse / 10


# Adaptive Lasso
almse <- 0
for (i in 1:10) {
    # Splits data
    partition <- c(train = .6, test = .2, validate = .2)
    split <- sample(cut(
    seq(nrow(filtered_hitters)), 
    nrow(filtered_hitters) * cumsum(c(0, partition)),
    labels = names(partition)
    ))
    data <- split(filtered_hitters, split)

    # Fits the model and prediction of test data
    x <- model.matrix(Salary ~ ., data$train)[, -1] # Remove salary column
    y <- data$train$Salary
    fitrcv <- cv.glmnet(x = x, y = y,
                        type.measure = "mse",
                        nfold = 10,
                        alpha = 0)
    bestrcoef <- as.numeric(coef.glmnet(fitrcv, s = fitrcv$lambda.min))[-1]
    fitalasso <- glmnet(x = x, y = y,
                            alpha = 1,
                            type.measure = "mse",
                            penalty.factor = 1 / abs(bestrcoef))
    almat <- data.matrix(data.frame(c(data$test[1:18], data$test[20])))
    alpredict <- predict.glmnet(fiten, newx = almat)
    almse <- almse + mean((data$test$Salary - alpredict)^2)
}
almse <- almse / 10

visualmat <- matrix(c(lsmse, ridgemse, bsmse, sfmse, sbmse, lmse, enmse, almse))
row.names(visualmat) <- c("Least Squares",
                            "Ridge Regression",
                            "Best Subsets",
                            "Stepwise Forward",
                            "Stepwise Backward",
                            "Lasso",
                            "Elastic Net",
                            "Adaptive Lasso")
barplot(t(visualmat),
            cex.axis = 0.7,
            cex.names = 0.7,
            las = 2,
            main = "Types of Models and their MSEs",
            xlab = "Model Type",
            ylab = "Average MSE over 10 trials")

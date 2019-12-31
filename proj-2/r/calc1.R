setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW5/')

nfl <- as.matrix(read.csv("nfl.txt", sep=" "))
ones <- rep(1, nrow(nfl))
X <- cbind(ones, nfl[,2], nfl[,7], nfl[,8])
x.1 <- X[,2]
x.2 <- X[,3]
x.3 <- X[,4]

y <- nfl[,1]
model <- glm(y~x.1+x.2+x.3)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)


fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="Normal Probability Plot Residuals")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 1")

plot(x.1, residuals, xlab="First predictor", ylab="Residuals", main="Residuals vs 1st predictor")

plot(x.2, residuals, xlab="Second predictor", ylab="Residuals", main="Residuals vs 2nd predictor")

plot(x.3, residuals, xlab="Third predictor", ylab="Residuals", main="Residuals vs 3rd predictor")


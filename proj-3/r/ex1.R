setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW6')

data <- as.matrix(read.csv('Bacteria.txt', sep=' '))
bacteria.number <- data[,1]
exposure.time <- data[,2]
plot(x=exposure.time, y=bacteria.number, main="Number of Bacteria vs Exposure Time")

ones <- rep(1, nrow(data))
X <- cbind(ones, data[,2])
x.1 <- X[,2]
y <- data[,1]

model <- glm(y~x.1)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)

fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="Normal Probability Plot Residuals")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 1")

plot(x.1, residuals, xlab="Predictor", ylab="Residuals", main="Residuals vs Predictor")


y.star <- log(y)

model2 <- lm(y.star~x.1)
resul2 <- summary(model2)

anova2 <- aov(model2)
tab2 <- summary(anova2)

fitted2 <- model2$fitted.values
residuals2 <- model2$residuals

sres2 <- rstudent(model2)

qqnorm(sres2, main="Normal Probability Plot Residuals 1c")
lines(-4:4, -4:4)

plot(fitted2, residuals2, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 1c")

plot(x.1, residuals2, xlab="Predictor", ylab="Residuals", main="Residuals vs Predictor 1c")

plot(x=exposure.time, y=log(bacteria.number), main="Transformed Number of Bacteria vs Exposure Time")

SS <- as.matrix(tab2[[1]]["Sum Sq"])###extracts sums of squares
SSR <- SS[1]
SSE <- SS[2]

df <- as.matrix(tab[[1]]["Df"])

df.reg <- df[1]
df.res <- df[2]


MSR <- SSR/df.reg
MSE <- SSE/df.res

crit.f <- qf(0.05, df.reg, df.res, lower.tail=FALSE)
F.stat <- MSR/MSE 
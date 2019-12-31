setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW6')

data <- as.matrix(read.csv('Defects.txt', sep=' '))
weight <- data[,1]
months <- data[,2]
plot(x=weight, y=months, main="Weight vs Months since production")

ones <- rep(1, nrow(data))
X <- cbind(ones, data[,2])
x <- X[,2]

# Centering
x.bar <- mean(x)
centered.x <- x-x.bar
centered.x2 <- centered.x^2

y <- data[,1]

model <- glm(y~centered.x+centered.x2)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)

anova <- aov(model)
tab <- summary(anova)
print(tab)

SS <- tab[[1]]["Sum Sq"]
df <- tab[[1]]["Df"]

SSR <- sum(SS[1:2,1])
df.reg <- sum(df[1:2,1])
SSE <- SS[3,1]
df.res <- df[3,1]

MSR <- SSR/df.reg
MSE <- SSE/df.res
F.stat <- MSR/MSE

crit.f <- qf(0.05, df.reg, df.res, lower.tail=FALSE)
if(F.stat > crit.f){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}


## Test for quadratic term
SSR.2 <- SS[2,1]
df.2 <- df[2,1]
F.stat2 <- SSR.2/MSE
crit.f <- qf(0.05, df.2, df.res, lower.tail=FALSE)
if(F.stat2 > crit.f){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}


## Residual Analysis
fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="Normal Probability Plot Residuals 4")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 4")

plot(centered.x, residuals, xlab="Predictor", ylab="Residuals", main="Residuals vs linear Predictor 4")

plot(centered.x2, residuals, xlab="Predictor", ylab="Residuals", main="Residuals vs quadratic Predictor 4")


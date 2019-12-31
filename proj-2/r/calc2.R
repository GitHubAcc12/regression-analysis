setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW5/')


data <- as.matrix(read.csv("tiretread.txt", sep=" "))

# Modell

ones <- rep(1, nrow(data))
X <- cbind(ones, data[,2], data[,3], data[,4])
x.1 <- X[,2]
x.2 <- X[,3]
x.3 <- X[,4]

y <- data[,1]
model <- glm(y~x.1+x.2+x.3)
result <- summary(model)

# Scatterplot
pairs(data, main="Scatterplot Exercise 2a")


# Significance of Regression
anova <- aov(model)
tab <- summary(anova)

SS <- as.matrix(tab[[1]]["Sum Sq"])###extracts sums of squares
SSR <- SS[1]+SS[2] + SS[3]
SSE <- SS[4]

df <- as.matrix(tab[[1]]["Df"])

df.reg <- df[1]+df[2]+df[3]
df.res <- df[4]


MSR <- SSR/df.reg
MSE <- SSE/df.res

crit.f <- qf(0.05, df.reg, df.res, lower.tail=FALSE)
F.stat <- MSR/MSE 


# Significance einzelner Parameter
# 1: saline coupling agent level and sulfur level accounted for
SSR.ex.1 <- SS[1]
df.ex.1 <- df[1]
F.test.part.1 <- SSR.ex.1/MSE
crit.f.part.1 <- qf(.05, df.ex.1, df.res, lower.tail = F)

# 2: hydrated silica level and sulfur level accounted for
SSR.ex.2 <- SS[2]
df.ex.2 <- df[2]
F.test.part.2 <- SSR.ex.2/MSE
crit.f.part.2 <- qf(.05, df.ex.2, df.res, lower.tail = F)

# 3: hydrated silica level and saline coupling level accounted for
SSR.ex.3 <- SS[3]
df.ex.3 <- df[3]
F.test.part.3 <- SSR.ex.3/MSE
crit.f.part.3 <- qf(.05, df.ex.3, df.res, lower.tail = F)


# residuals
fitted <- model$fitted.values
residuals <- model$residuals

plot(fitted, residuals, xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted Values 2")

sres <- rstudent(model)
qqnorm(sres, main="Normal Probability Plot 2")
lines(-4:4, -4:4)

plot(x.1, residuals, xlab="Hydrated Silica Level", ylab="Residuals",main="Hydrated Silica Level vs Residuals")

plot(x.2, residuals, xlab="Saline Coupling Agent Level", ylab="Residuals", main="Saline Coupling Agent Level vs Residuals")

plot(x.3, residuals, xlab="Sulfur Level", ylab="Residuals", main="Sulfur Level vs Residuals")


model1 <- lm(y~x.1+x.2+x.3)
model2 <- lm(y~factor(x.1)+factor(x.2)+factor(x.3))

lof <- anova(model1, model2)

setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW6')

data <- as.matrix(read.csv('Defects.txt', sep=' '))
defects <- data[,1]
weeks <- data[,2]
plot(x=weeks, y=defects, main="Average Defects vs Weeks since Overhaul")

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

qqnorm(sres, main="Normal Probability Plot Residuals 2a")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 2a")

plot(x.1, residuals, xlab="Predictor", ylab="Residuals", main="Residuals vs Predictor 2a")



## Transformation???? beta0+beta1x+beta2x^2

y.star <- log(y)
x.star <- x.1#sqrt(x.1)

model2 <- lm(y.star~x.star)
resul2 <- summary(model2)

anova2 <- aov(model2)
tab2 <- summary(anova2)

fitted2 <- model2$fitted.values
residuals2 <- model2$residuals

sres2 <- rstudent(model2)

qqnorm(sres2, main="Normal Probability Plot Residuals 2b")
lines(-4:4, -4:4)

plot(fitted2, residuals2, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 2b")

plot(x.1, residuals2, xlab="Predictor", ylab="Residuals", main="Residuals vs Predictor 2b")

plot(x=weeks, y=log(defects), main="Transformed average defects vs weeks since overhaul 2b")

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


##############Leverage
H <- X%*%solve(t(X)%*%X)%*%t(X)
p <- 2
n <- length(x.1)
threshold <- 2*p/n
leverage <- NULL
diags <- diag(H)
for(i in 1:n){
  if(diags[i] > threshold){
    leverage <- c(leverage, 1)
  }else{
    leverage <- c(leverage, 0)
  }
}

number.leverage <- sum(leverage)
lev.points <- which(leverage==1)
lev.points.obs <- data[lev.points, ]


##########Influence
cooks.d <- cooks.distance(model)
alpha <- 0.5
influence <- NULL
inf.thresh <- qf(alpha, p, n-p, lower.tail=FALSE)
for(i in 1:n){
  if(cooks.d[i] > inf.thresh){
    influence <- c(influence, 1)
  }else{
    influence <- c(influence, 0)
  }
}

inf.points <- which(influence==1)
inf.points.obs <- data[inf.points,]

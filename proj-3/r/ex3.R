setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW6')

data <- as.matrix(read.csv('HeatFlux.txt', sep=' '))

pairs(data, main="Scatterplot Matrix")

ones <- rep(1, nrow(data))
X <- cbind(ones, data[,2], data[,3], data[,4], data[,5], data[,6])
x.1 <- X[,2]
x.2 <- X[,3]
x.3 <- X[,4]
x.4 <- X[,5]
x.5 <- X[,6]
y <- data[,1]

model <- glm(y~x.1+x.2+x.3+x.4+x.5)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)

## Significance with all points
SS <- as.matrix(tab[[1]]["Sum Sq"])###extracts sums of squares
SSR <- SS[1]+SS[2]+SS[3]+SS[4]+SS[5]
SSE <- SS[6]

df <- as.matrix(tab[[1]]["Df"])

df.reg <- df[1]+df[2]+df[3]+df[4]+df[5]
df.res <- df[6]


MSR <- SSR/df.reg
MSE <- SSE/df.res

crit.f <- qf(0.05, df.reg, df.res, lower.tail=FALSE)
F.stat <- MSR/MSE 


fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="Normal Probability Plot Residuals 2a")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted values 3a")

plot(x.1, residuals, xlab="Insolation", ylab="Residuals", main="Residuals vs Insolation 3a")

plot(x.2, residuals, xlab="East Position", ylab="Residuals", main="Residuals vs East Position 3a")

plot(x.3, residuals, xlab="South Position", ylab="Residuals", main="Residuals vs South Position 3a")

plot(x.4, residuals, xlab="North Position", ylab="Residuals", main="Residuals vs North Position 3a")

plot(x.5, residuals, xlab="Time of Day", ylab="Residuals", main="Residuals vs Time of Day 3a")



## Leverage
H <- X%*%solve(t(X)%*%X)%*%t(X)
p <- 5
n <- length(y)
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


## Influence
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



## Without influential point
data2 <- data[-inf.points,]

ones2 <- rep(1, nrow(data2))
X2 <- cbind(ones2, data2[,2], data2[,3], data2[,4], data2[,5], data2[,6])
x.12 <- X2[,2]
x.22 <- X2[,3]
x.32 <- X2[,4]
x.42 <- X2[,5]
x.52 <- X2[,6]
y2 <- data2[,1]

model2 <- glm(y2~x.12+x.22+x.32+x.42+x.52)
result2 <- summary(model2)


anova2 <- aov(model2)
tab2 <- summary(anova2)

## Significance with all points
SS2 <- as.matrix(tab2[[1]]["Sum Sq"])###extracts sums of squares
SSR2 <- SS2[1]+SS2[2]+SS2[3]+SS2[4]+SS2[5]
SSE2 <- SS2[6]

df2 <- as.matrix(tab2[[1]]["Df"])

df.reg2 <- df2[1]+df2[2]+df2[3]+df2[4]+df2[5]
df.res2 <- df2[6]


MSR2 <- SSR2/df.reg2
MSE2 <- SSE2/df.res2

crit.f2 <- qf(0.05, df.reg2, df.res2, lower.tail=FALSE)
F.stat2 <- MSR2/MSE2


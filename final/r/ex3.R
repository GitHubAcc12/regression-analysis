setwd("/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Final")

data <- as.matrix(read.table("Bluegill.txt"))
plot(data, main="")

f <- function(x) {
  return((x-5)^3 + 500)
}

plot(0:6, f(0:6), main="")

ones <- rep(1, nrow(data))

X <- cbind(ones, data[,1])
x.1 <- X[,2]

y <- data[,2]

centered.x <- x.1 - mean(x.1)
x3 <- centered.x^3
x2 <- centered.x^2
x1 <- centered.x

model <- glm(y~x3+x2+x1)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)

SS <- as.matrix(tab[[1]]["Sum Sq"])
df <- as.matrix(tab[[1]]["Df"])

SSR <- SS[1]+SS[2]+SS[3]
SSE <- SS[4]

df.reg <- df[1]+df[2]+df[3]
df.res <- df[4]

MSR <- SSR/df.reg
MSE <- SSE/df.res

SSR1 <- SS[1]
df1 <- df[1]
F.test.1 <- SSR1/MSE
crit.f.1 <- qf(.05, df1, df.res, lower.tail = F)

SSR2 <- SS[2]
df2 <- df[2]
F.test.2 <- SSR2/MSE
crit.f.2 <- qf(.05, df2, df.res, lower.tail = F)



## Leverage
H <- X%*%solve(t(X)%*%X)%*%t(X)
p <- 3
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


data2 <- data[-lev.points,]
ones <- rep(1, nrow(data2))

X2 <- cbind(ones, data2[,1])
x.12 <- X2[,2]

y2 <- data2[,2]

centered.x2 <- x.12 - mean(x.12)
x32 <- centered.x2^3
x22 <- centered.x2^2
x12 <- centered.x2

model2 <- glm(y2~x32+x22+x12)
result2 <- summary(model2)


anova2 <- aov(model2)
tab2 <- summary(anova2)



# Part d
fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="")

plot(x3, residuals, xlab="x^3", ylab="Residuals", main="")

plot(x2, residuals, xlab="x^2", ylab="Residuals", main="")

plot(x1, residuals, xlab="x", ylab="Residuals", main="")

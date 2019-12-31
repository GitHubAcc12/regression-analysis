setwd("/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Final")
data <- as.matrix(read.table("SignDistance.txt"))
plot(data, main="")

ones <- rep(1, nrow(data))

X <- cbind(ones, data[,1])
x.1 <- X[,2]

y <- data[,2]

model <- glm(y~x.1)
result <- summary(model)

anova <- aov(model)
tab <- summary(anova)

SS <- as.matrix(tab[[1]]["Sum Sq"])
SSR <- SS[1]
SSE <- SS[2]

df <- as.matrix(tab[[1]]["Df"])
df.reg <- df[1]
df.res <- df[2]

MSR <- SSR/df.reg
MSE <- SSE/df.res

SXX <- sum((x.1-mean(x.1))^2)

T.stat <- abs((model$coefficients[2])/(sqrt(MSE/SXX)))
crit.t <- qt(0.025, df.res, lower.tail = F)

hat.matrix <- X%*%solve(t(X)%*%X)%*%t(X) #hat matrix
hat.beta <- solve(t(X)%*%X)%*%t(X)%*%y #coefficients
fitted.vals <- hat.matrix%*%y 
inv.xtx <- solve(t(X)%*%X)
diagonals <- diag(inv.xtx)
t.conf <- qt(0.025, df.res, lower.tail=FALSE)
for(i in 1:2){
  ci.low <- hat.beta[i] - t.conf*sqrt(MSE*diagonals[i])
  ci.up <- hat.beta[i] + t.conf*sqrt(MSE*diagonals[i])
  ci <- c(ci.low, ci.up)
  print(ci)
}


x.0 <- c(1, 37)
#######Make it a matrix to avoid cross-class problems
x.0 <- as.matrix(x.0)

#######compute MSE multiplier
multiplier <- t(x.0)%*%inv.xtx%*%x.0

########new fitted value
y.0 <- t(x.0)%*%hat.beta

########t value
t.conf.mr <- qt(0.005, df.res, lower.tail=FALSE)

#########construct ci
ci.low.mr <- y.0 - t.conf.mr*sqrt(MSE*multiplier)
ci.up.mr <- y.0 + t.conf.mr*sqrt(MSE*multiplier)
ci.mr <- c(ci.low.mr, ci.up.mr)
print(ci.mr)

# pi
pi.low <- y.0 - t.conf.mr*sqrt(MSE*(1+multiplier))
pi.up <- y.0 + t.conf.mr*sqrt(MSE*(1+multiplier))
pi <- c(pi.low, pi.up)
print(pi)


# Part d
fitted <- model$fitted.values
residuals <- model$residuals

sres <- rstudent(model)

qqnorm(sres, main="")
lines(-4:4, -4:4)

plot(fitted, residuals, xlab="Fitted", ylab="Residuals", main="")

## Leverage
H <- X%*%solve(t(X)%*%X)%*%t(X)
p <- 1
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


# Part e
model1 <- lm(y~x.1)
model2 <- lm(y~factor(x.1))

lof <- anova(model1, model2)
setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW4')
mileage <- as.matrix(read.csv("mileage.txt", sep=" "))
ones <- rep(1, nrow(mileage))
X <- cbind(ones, mileage[,2], mileage[,6])

x.1 <- X[,2]
x.2 <- X[,3]
y <- mileage[,1]
model <- glm(y~x.1+x.2)
result <- summary(model)

anova <- aov(model)
tab <- summary(anova)

inv.xtx <- solve(t(X)%*%X)
df <- as.matrix(tab[[1]]["Df"])
df.reg <- df[1]+df[2]
df.res <- df[3]
SS <- as.matrix(tab[[1]]["Sum Sq"])
SSR <- SS[1]+SS[2]
SSE <- SS[3]

MSR <- SSR/df.reg
MSE <- SSE/df.res

diagonals <- diag(inv.xtx)
t.conf <- qt(.025, df.res, lower.tail = F)
ci.low <- hat.beta[2]-t.conf*sqrt(MSE*diagonals[2])
ci.up <- hat.beta[2]+t.conf*sqrt(MSE*diagonals[2])
ci <- c(ci.low, ci.up)
print(ci)

for(i in 1:2){
  hat.beta <- solve(t(X)%*%X)%*%t(X)%*%y
  T <- hat.beta[i]/sqrt(MSE*solve(t(X)%*%X)[i+1, i+1])
  cat("T=",T,"\n")
}

# new design vector:
x.0 <- as.matrix(c(1, 275, 2))

# get mse multiplier
multiplier <- t(x.0)%*%inv.xtx%*%x.0

# get fitted value
y.0 <- t(x.0)%*%hat.beta

# t value
t.conf.mr <- qt(.025, df.res, lower.tail = F)

# CI
ci.low.mr <- y.0-t.conf.mr*sqrt(MSE*multiplier)
ci.up.mr <- y.0+t.conf.mr*sqrt(MSE*multiplier)
ci.mr <- c(ci.low.mr, ci.up.mr)
print(ci.mr)

pi.low <- y.0-t.conf.mr*sqrt(MSE*(1+multiplier))
pi.up <- y.0+t.conf.mr*sqrt(MSE*(1+multiplier))
pi <- c(pi.low, pi.up)
print(pi)

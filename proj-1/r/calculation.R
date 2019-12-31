setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW4')
nfl <- as.matrix(read.csv("nfl.txt", sep=" "))
ones <- rep(1, nrow(nfl))
X <- cbind(ones, nfl[,2], nfl[,7], nfl[,8])
x.1 <- X[,2]
x.2 <- X[,3]
x.3 <- X[,4]

y <- nfl[,1]
model <- glm(y~x.1+x.2+x.3)
result <- summary(model)

hat.matrix <- X%*%solve(t(X)%*%X)%*%t(X)
hat.beta <- solve(t(X)%*%X)%*%t(X)%*%y
fitted.vals <- hat.matrix%*%y
identity <- diag(1, length(y), length(y))  
residuals <- (identity-hat.matrix)%*%y

anova <- aov(model)
tab <- summary(anova)

SS <- as.matrix(tab[[1]]["Sum Sq"])
SSR <- SS[1]+SS[2]+SS[3]
SSE <- SS[4]

df <- as.matrix(tab[[1]]["Df"])
df.reg <- df[1]+df[2]+df[3]
df.res <- df[4]

MSR <- SSR/df.reg
MSE <- SSE/df.res

crit.f <- qf(.05, df.reg, df.res, lower.tail=F)
F.stat <- MSR/MSE

if(F.stat > crit.f) {
  cat("F.stat=",F.stat, " > crit.f=", crit.f,", so reject H0\n")
} else {
  cat("F.stat=",F.stat, " <= crit.f=", crit.f, ", so do not reject H0\n")
}

for(i in 2:4) {
  beta.1.hat <- hat.beta[i]
  inv.xtx <- solve(t(X)%*%X)
  est.var <- MSE*inv.xtx[i,i]
  t.stat <- abs(beta.1.hat/sqrt(est.var))
  crit.t <- qt(.025, df.res, lower.tail = F)
  
  if(t.stat > crit.t) {
    cat("t.stat=", t.stat, "> crit.t=", crit.t, ", so reject H0\n")
  } else {
    cat("t.stat=",t.stat,"<= crit.t=", crit.t, ", so do not reject H0\n")
  }
}

R.sq <- 1 - SSE/(SSE + SSR)
R.adj <- 1 - (1 - R.sq)*((nrow(nfl)-1)/(nrow(nfl)-ncol(X)-1))

SSR.ex.2 <- SS[2]
df.ex.2 <- df[2]

F.test <- SSR.ex.2/MSE
crit.F <- qf(.05, df.ex.2, df.res, lower.tail = F)

if(F.test > crit.F) {
  cat("F.test=",F.test," > crit.F=",crit.f,", so reject H0")
} else {
  cat("F.test=",F.test," <= crit.F=",crit.f,", so do not reject H0")
}

hat.matrix <- X%*%solve(t(X)%*%X)%*%t(X)
fitted.values <- hat.matrix%*%y
correlation <- cor(y, fitted.values)
correlation.sq <- correlation^2
dif <- abs(R.sq - correlation.sq)
dif
setwd("/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Final")

data <- as.matrix(read.table("Hospitals.txt"))
pairs(data, main="")

ones <- rep(1, nrow(data))

X <- cbind(ones, data[,2], data[,3], data[,4], data[,5], data[,6])
x.1 <- X[,2]
x.2 <- X[,3]
x.3 <- X[,4]
x.4 <- X[,5]
x.5 <- X[,6]

y <- data[,1]

model <- glm(y~x.1+x.2+x.3+x.4+x.5+x.1*x.3+x.1*x.4+x.1*x.5+x.2*x.3+x.2*x.4+x.2*x.5)
result <- summary(model)

anova <- aov(model)
tab <- summary(anova)

## Significance with all points
SS <- as.matrix(tab[[1]]["Sum Sq"])###extracts sums of squares
SSR <- SS[1]+SS[2]+SS[3]+SS[4]+SS[5]+SS[6]+SS[7]+SS[8]+SS[9]+SS[10]+SS[11]
SSE <- SS[12]

df <- as.matrix(tab[[1]]["Df"])

df.reg <- df[1]+df[2]+df[3]+df[4]+df[5]+df[6]+df[7]+df[8]+df[9]+df[10]+df[11]
df.res <- df[12]


MSR <- SSR/df.reg
MSE <- SSE/df.res

crit.f <- qf(0.05, df.reg, df.res, lower.tail=FALSE)
F.stat <- MSR/MSE 

## Significance of interaction terms

# x1x3
SSR13 <- SS[6]
df13 <- df[6]
F.test.13 <- SSR13/MSE
crit.f.13 <- qf(.05, df13, df.res, lower.tail = F)

# x1x4
SSR14 <- SS[7]
df14 <- df[7]
F.test.14 <- SSR14/MSE
crit.f.14 <- qf(.05, df14, df.res, lower.tail = F)

# x1x5
SSR15 <- SS[8]
df15 <- df[8]
F.test.15 <- SSR15/MSE
crit.f.15 <- qf(.05, df15, df.res, lower.tail = F)

# x2x3
SSR23 <- SS[9]
df23 <- df[9]
F.test.23 <- SSR23/MSE
crit.f.23 <- qf(.05, df23, df.res, lower.tail = F)

# x2x4
SSR24 <- SS[10]
df24 <- df[10]
F.test.24 <- SSR24/MSE
crit.f.24 <- qf(.05, df24, df.res, lower.tail = F)

# x2x5
SSR25 <- SS[11]
df25 <- df[11]
F.test.25 <- SSR25/MSE
crit.f.25 <- qf(.05, df25, df.res, lower.tail = F)


# x1
SSR1 <- SS[1]
df1 <- df[1]
F.test.1 <- SSR1/MSE
crit.f.1 <- qf(.05, df1, df.res, lower.tail = F)

# x2
SSR2 <- SS[2]
df2 <- df[2]
F.test.2 <- SSR2/MSE
crit.f.2 <- qf(.05, df2, df.res, lower.tail = F)

# x3
SSR3 <- SS[3]
df3 <- df[3]
F.test.3 <- SSR3/MSE
crit.f.3 <- qf(.05, df3, df.res, lower.tail = F)

# x4
SSR4 <- SS[4]
df4 <- df[4]
F.test.4 <- SSR4/MSE
crit.f.4 <- qf(.05, df4, df.res, lower.tail = F)

# x5
SSR5 <- SS[5]
df5 <- df[5]
F.test.5 <- SSR5/MSE
crit.f.5 <- qf(.05, df5, df.res, lower.tail = F)


## Part c
meddl <- lm(y~x.1+x.3+x.2*x.5)
res <- summary(meddl)

fitted <- meddl$fitted.values
residuals <- meddl$residuals

plot(fitted, residuals, xlab="Fitted Values", ylab="Residuals", main="")

plot(x.1, residuals, xlab="Average Length of Stay", ylab="Residuals", main="")

plot(x.3, residuals, xlab="Hospital in North Region", ylab="Residuals", main="")

plot(x.2*x.5, residuals, xlab="Frequency of X-Ray use if Hospital in West Region", ylab="Residuals", main="")


sres <- rstudent(meddl)

qqnorm(sres, main="")
lines(-4:4,-4:4)

plot(x.2, residuals, xlab="Frequency of X-Ray Use", ylab="Residuals", main="")

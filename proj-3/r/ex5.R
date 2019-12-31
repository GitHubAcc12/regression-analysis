setwd('/Users/jankretschmann/Documents/Master/Milwaukee/Semester 1/Regression Analysis/Homework/HW6')

data <- as.matrix(read.csv('Mileage.txt', sep=' '))
data <- cbind(data[,1], data[,2], data[,4])
pairs(data, main="Scatterplot Matrix")

ones <- rep(1, nrow(data))
X <- cbind(ones, data[,2], data[,3])
x.1 <- X[,2]
x.2 <- X[,3]
y <- data[,1]

model <- glm(y~x.1+x.2)
result <- summary(model)


anova <- aov(model)
tab <- summary(anova)

print(tab)

SS <- as.matrix(tab[[1]]["Sum Sq"])###extracts sums of squares
df <- as.matrix(tab[[1]]["Df"])

df.res <- df[3]
SSE <- SS[3]
MSE <- SSE/df.res

SSR.2 <- SS[2]
df.2 <- df[2]
F.stat.2 <- SSR.2/MSE
crit.f.2 <- qf(.05, df.2, df.res, lower.tail = F)

x1x2 <- x.1*x.2
model2 <- glm(y~x.1+x.2+x1x2)
summ2 <- summary(model2)
anova2 <- aov(model2)
tab2 <- summary(anova2)


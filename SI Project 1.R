setwd("C:\\Users\\Kun\\Desktop\\work\\SI")

## set seed and parameter
set.seed(3)
lambda = 0.2
n = 40
N = 1000
## theoratical mean
tmean = 1/lambda
## theoratical standard deviation
tsd = 1/lambda

## Simulate 40 exponentials 1000 times
vexp = rexp(n*N,lambda)
vmat = matrix(vexp,n,N)
dim(vmat)

## Calculate mean sample of distribution
meanSample = apply(vmat,2,mean)

hist(meanSample, breaks=50, prob=TRUE,main="Distribution of averages of samples",xlab="")
# density of the averages of samples
lines(density(meanSample))
# theoretical center of distribution
abline(v=1/lambda, col="red")
# theoretical density of the averages of samples
xfit <- seq(min(meanSample), max(meanSample), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(n)))
lines(xfit, yfit, pch=22, col="red", lty=2)
# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), col=c("black", "red"))

## Check Normality
qqnorm(meanSample) 
qqline(meanSample)

## average mean
mean(meanSample)
## theoretical mean
tmean

## sd of mean
sd(meanSample)
## theoretical sd
tsd

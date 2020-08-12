A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
boxplot(A,B)
t.test(A,B)
t.test(A,B, var.equal=F)
t.test(A,B, var.equal=T)
x <- c(70, 80, 72, 76, 76, 76, 72, 78, 82, 64, 74, 92, 74, 68, 84)
y <- c(68, 72, 62, 70, 58, 66, 68, 52, 64, 72, 74, 60, 74, 72, 74)
t.test(x,y, paired=T, conf.level=0.95)
d<-x-y
t.test(d)
# 모집단 1: 시행횟수 n1, 성공횟수, x1
# 모집단 2: 시행횟수 n2, 성공횟수, x2
# n1=100, x1 ==88; n2=150, x2=126인 경우
prop.test(x=c(88,126),n=c(100,150))
x = c(3,3,4,5,6,6,7,8,8,9)
y = c(9,5,12,9,14,16,22,18,24,22)
plot(x,y)
cor(x,y)

#단순선형회귀모형에 적합
fit <- lm(y~x) #lm[linear model] <- 단순성형회귀분석을 하라
summary(fit)

#각 관측치의 잔차를 알고 싶을 때
resid(fit)
rr <- y - fitted(fit)
sum[resid(fit)**2]
sqrt(sum(resid(fit)**2)/8)

#회귀계수를 알고 싶을 때
coef(fit)
fit$coefficients

#회귀계수의 신뢰구간
confint(fit, level=0.95)#신뢰구간

#회귀모형의 ANOVA
anova(fit)

#산점도와 회귀직선을 동시에 그리고 싶을 때
plot(x,y)
abline(fit)

data()
stackloss

y <- stackloss$stack.loss
x1 <- stackloss$Air.Flow
x2 <- stackloss$Water.Temp
x3 <- stackloss$Acid.Conc.
X <- cbind(x1,x2,x3)
pairs(X) #산전도 행렬
stackfit <- lm(y ~ x1 + x2 + x3)
anova(stackfit)
plot(stackfit)
              # residual vs fitted
              # normal qq plot
              # standardized residual vs fitted
              # Cook's distance
summary(stackfit)
anova(stackfit)
deviance(stackfit) #RSS
deviance(lm(y~1))  #SST
residuals(stackfit)#y-y^hat
vcov(stackfit)
coef(stackfit)
step(stackfit)

x <- faithful$eruption
y <- faithful$waiting
plot(x,y)
x1 = x[1:136]
x2 = x[137:272]
y1 = y[1:136]
y2 = y[137:272]
plot(c(x1,x2), c(y1,y2), type="n")
points(x1,y1, col="red")
points(x2,y2, col="blue")
abline(lm(y1~x1)) #그림위에 직선을 그림
abline(lm(y2~x2)) #그림위에 직선을 그림
abline(lm(y~x))   #그림위에 직선을 그림

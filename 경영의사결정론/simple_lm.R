x <- c(20,40,20,30,10,10,20,20,20,30)
y <- c(30,60,40,60,30,40,40,50,30,70)

rs <- lm(y~x)
summary(rs)

cbind(y, fitted(rs), resid(rs))
t <- 1:10
tf <- 1:30

Sales <- c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)

Bass.nls <- nls(Sales ~ m*((p+q)^2/p)*exp(-(p+q)*t)/(1+(q/p)*exp(-(p+q)*t))^2,
                           start = c(list(m=sum(Sales), p = 0.1, q = 0.2)))
                
summary(Bass.nls)

m = coef(Bass.nls)[1]
p = coef(Bass.nls)[2]
q = coef(Bass.nls)[3]

Bass.f <- ((p+q)^2/p)*exp(-(p+q)*tf)/(1+(q/p)*exp(-(p+q)*tf))^2
Bass.F <- (1-exp(-(p+1)*tf))/(1+(q/p)*exp(-(p+1)*tf))

plot(tf, m*Bass.f, type="l", ylab="Sales")
n = length(Sales)
lines(1:n, Sales, col="red", lwd=2, lty=2)
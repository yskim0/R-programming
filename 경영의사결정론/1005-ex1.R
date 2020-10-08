Sales <- c(840, 1470, 2110, 4000, 5890, 7790, 7590, 9470, 10950, 10530) 
cum_Sales = cumsum(Sales)
cum_Sales2 = cum_Sales^2

Bass.ls <- lm(Sales ~ cum_Sales + cum_Sales2)  # Regression
print(summary(Bass.ls))

b= Bass.ls$coefficients

#FIT THE MODEL
m1 = (-b[2]+sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3])
m2 = (-b[2]-sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3]) 
print(c(m1,m2))
m = max(m1,m2); print(m)
p = b[1]/m
q = -m*b[3]
print(c("p,q=",p,q))

FF = expression(p*(exp((p+q)*t)-1)/(p*exp((p+q)*t)+q))

ff = D(FF,"t") 
fn_f = eval(ff)*m

plot(t,fn_f,type="l")
n = length(Sales) 
lines(1:n,Sales,col="red",lwd=2,lty=2)
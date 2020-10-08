t <- 1:16
tf <- 1:30

Sales <- c(4.9, 4.3, 5.3, 10.9, 9.2, 2.5, 4.2, 8.3, 9.3, 6.2, 4.3, 2.8, 3.2, 3.5, 3.7, 4.5)

Bass.nls <- nls(Sales ~ m*((p+q)^2/p)*exp(-(p+q)*t)/(1+(q/p)*exp(-(p+q)*t))^2, start = c(list(m=sum(Sales),p=0.1, q=0.2)))

summary(Bass.nls)

m=coef(Bass.nls)[1] 
p=coef(Bass.nls)[2] 
q=coef(Bass.nls)[3]

Bass.f <- ((p+q)^2/p)*exp(-(p+q)*tf)/(1+(q/p)*exp(-(p+q)*tf))^2 
Bass.F <- (1-exp(-(p+1)*tf))/(1+(q/p)*exp(-(p+1)*tf))
plot(tf,m*Bass.f,type="l",ylab="Sales") 
# plot(tf,m*Bass.F,type="l",ylab="Sales")  # 누적값 그래프 
n = length(Sales) 
lines(1:n,Sales,col="red",lwd=2,lty=2)

# 빨간색 선이 실제 데이터 
# 검은 선이 bass diffustion 

cbind(Sales, m*Bass.f)

# Assignment
  # R script
  # graph 
  # MAPE, MAE, MSE 결과 제출
# write.csv()

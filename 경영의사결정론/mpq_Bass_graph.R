# p,q 계수의 값을 알고 있는 경우
#  + 계수 m 이 주어졌을 경우 

graphs <- function(m, p,q,t.max) {
  t <- seq(from=1, to=t.max)
  Bass.f <- ((p+q)^2/p)*exp(-(p+q)*t)/(1+(q/p)*exp(-(p+q)*t))^2
  Bass.F <- (1-exp(-(p+1)*t))/(1+(q/p)*exp(-(p+1)*t))
  par(mfrow=c(1,2))
  plot(t, m*Bass.f, type = "l"); plot(t, m*Bass.F, type="l")
}

graphs(100000, 0.01, 0.2, 20)
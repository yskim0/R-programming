# p,q 계수의 값을 알고 있는 경우

graphs <- function(p,q,t.max) {
  t <- seq(from=0, to=t.max, by = 0.01)
  Bass.f <- ((p+q)^2/p)*exp(-(p+q)*t)/(1+(q/p)*exp(-(p+q)*t))^2
  Bass.F <- (1-exp(-(p+1)*t))/(1+(q/p)*exp(-(p+1)*t))
  par(mfrow=c(1,2))
  plot(t, Bass.f, type = "l"); plot(t, Bass.F, type="l")
}

graphs(0.016,0.409,20)
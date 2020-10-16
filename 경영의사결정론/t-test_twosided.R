x <- read.csv(file.choose())
# h0 : mu_M = mu_F, h1 : neq
attach(x)

var.test(cGPA~gender, alternative="two.sided")
# => H0 Accept : var_F = var_M
# 기본 가정(귀무 가설)이 분산이 같다임. 

t.test(cGPA~gender, alternative = "two.sided", paired=FALSE, var.equal=TRUE)
t.test(cGPA, hGPA, alternative = "two.sided", paired=TRUE) # 종속 표본이니까 

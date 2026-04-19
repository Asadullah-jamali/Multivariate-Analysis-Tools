data = matrix(c(10,2,3,4,5,6,7,8),nrow=4)
data
i=(rep(1,nrow(data)))
mean = (1/nrow(data))*(t(data)%*%i)
mean
i
v = cov(data)
v
d= diag(v)
d
d[1]
d[2]
D=matrix(c(1/sqrt(d[1]),0,0,1/sqrt(d[2])), nrow=2)
D
cr=D%*%v%*%D
cr
cor(data)
5*qf(0.95,3,3)

x1=c(61,38,33,62,74,60,11,11,15,14)
x2=c(18,26,91,63,62,24,29,8,35,77)
x3=c(15,55,76,28,45,77,49,84,52,59)
x=cbind(x1,x2,x3)
is.matrix(x)
n=nrow(x)
I = rep(1,n)
mu=(1/n)*(t(x))%*%(I)
mu
v = (1/(n-1))*(t(x)%*%(x)-n*(mu%*%t(mu)))
v
cov(x)
mu_vector=c(70,68,73)
t2=(n-1)*(t(mu-mu_vector)%*%(solve(v))%*%(mu-mu_vector))
t2
f = ((3*(9)/10*(7)))*qf(0.95,3,7)

if (t2>=f)
  cat("reject Ho")

sim = data.frame(
  variable = colnames(x),
  lower = mu - sqrt((f)*diag(v)),
  upper = mu + sqrt((f)*diag(v))
)
sim
Hotelling::hotelling.test(x)

eig
eig$values
eig$vectors

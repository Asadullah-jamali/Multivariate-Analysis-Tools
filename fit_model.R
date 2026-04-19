
data=read.csv(file.choose())
head(data)

y = data[c('Performance_Score','Monthly_Salary')]
is.matrix(y)
y=as.matrix(y)
x=as.matrix(cbind(1,data[c('Team_Size','Training_Hours','Overtime_Hours','Work_Hours_Per_Week')]))
x
Bhat=solve(t(x)%*%(x))%*%t(x)%*%y
Bhat
yhat = x%*%Bhat
yhat

e=y-yhat
colSums(e)
n=nrow(y)
k=ncol(x)-1
sigma=t(e)%*%(e)/(n-k-1)
sigma


fit = lm(cbind(Performance_Score,Monthly_Salary) ~ Team_Size+ Training_Hours + Overtime_Hours + Work_Hours_Per_Week ,data = data )
summary(fit)

library(car)
Manova(fit)

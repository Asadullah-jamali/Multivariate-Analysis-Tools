g1=matrix(c(47,53,31,67,67,54,58,47,61,60,54,39,37,58,50,58,48,72,61,49,50,50,54,61,38,51,48,69,34,30),ncol=3)

g2=matrix(c(61,61,57,61,38,54,48,71,60,60,39,58,73,70,73,68,51,81,79,60,44,48,48,81,80,79,81,69,68,75),ncol=3)

g3=matrix(c(35,39,42,39,49,54,58,51,50,47,47,60,60,58,51,51,61,43,38,38,30,50,51,51,46,38,48,49,57,56),ncol = 3)

g4=matrix(c(60,58,58,61,67,66,66,72,70,68,43,48,51,57,43,49,60,57,60,60,50,60,57,53,43,43,57,60,59,60),ncol = 3)

data=rbind(g1,g2,g3,g4)
n= nrow(g1)
#means of groups

m1 = (colMeans(g1))
m2 = (colMeans(g2))
m3 = (colMeans(g3))
m4 = (colMeans(g4))


# variance covarince matrix divide by (n-1)
v1 = cov(g1)
v2 = cov(g2)
v3 = cov(g3)
v4 = cov(g4)

#within group variation

W = ((9)*(v1+v2+v3+v4))

# between group

xbar=colMeans(data)
x1=(m1-xbar)
x2=(m2-xbar)
x3=(m3-xbar)
x4=(m4-xbar)


B=n*(((x1)%*%t(x1))+((x2)%*%t(x2))+((x3)%*%t(x3))+((x4)%*%t(x4)))



lamda = det(W)/det(B+W)

#d.f 
p=3
g=4

v1= (p*(g-1))

s=sqrt((p*p*(g-1)*(g-1)-4)/(p*p+(g-1)*(g-1)-5))
m=n*g -1 - (p*(g-1))/2
v2=round((m*s-(p*(g-1)/2)+1),0)

v1
v2

F = (v2/v1)*((1-lamda^(1/s))/lamda^(1/s))

Ft= qf(0.95,v1,v2)

if(F >= Ft ) print('reject Ho') else print("do no reject Ho")

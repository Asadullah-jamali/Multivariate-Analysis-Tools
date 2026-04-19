# data link :  https://www.kaggle.com/datasets/mexwell/employee-performance-and-productivity-data?resource=download

data=read.csv(file.choose())
method1= subset(data,Training_Hours<=25)
method2= subset(data,Training_Hours > 25 & Training_Hours<=50)
method3=subset(data,Training_Hours>50 & Training_Hours<=75)
method4= subset(data,Training_Hours>75)



names(data)
control=method1[,c("Monthly_Salary","Performance_Score" ,"Employee_Satisfaction_Score")]
technical=method2[,c("Monthly_Salary","Performance_Score" ,"Employee_Satisfaction_Score")]
soft_skill=method3[,c("Monthly_Salary","Performance_Score" ,"Employee_Satisfaction_Score")]
leadership=method4[,c("Monthly_Salary","Performance_Score" ,"Employee_Satisfaction_Score")]

#means of data

mean1=colMeans(control)
is.matrix(mean1)
mean1=as.matrix(mean1)
mean2=colMeans(technical)
is.matrix(mean2)
mean2=as.matrix(mean2)
mean3=colMeans(soft_skill)
is.matrix(mean3)
mean3=as.matrix(mean3)
mean4=colMeans(leadership)
is.matrix(mean4)
mean4=as.matrix(mean4)


#variance cov variance matrix

var1=cov(control)
var2=cov(technical)
var3=cov(soft_skill)
var4=cov(leadership)

#Total no of observation

N=nrow(control)+nrow(technical)+nrow(soft_skill)+nrow(leadership)


#pooled Average 

meandot = (nrow(control)*mean1 + nrow(technical)*mean2 + nrow(soft_skill)*mean3 + nrow(leadership)*mean4)/N
meandot


# differences from mean

d1=mean1-meandot
d2=mean2-meandot
d3=mean3-meandot
d4=mean4-meandot


# Between SS

B= nrow(control)*(d1%*%t(d1)) + nrow(technical)*(d2%*%t(d2)) + nrow(soft_skill)*(d3%*%t(d3)) + nrow(leadership)*(d4%*%t(d4))

# within SS

W=(nrow(control)-1)*var1+(nrow(technical)-1)*var2 +(nrow(soft_skill)-1)*var3+ (nrow(leadership)-1)*var4

#lamda

lamda=det(W)/(det(W+B))
lamda

# F calculated

fcal = (N - 3-1)*(1-lamda)/(3*(N-2) * lamda)
fcal


# f table value
f=qf(0.05,2*3,2*(N-3-2))
f

if(fcaq>f){
  print('reject HO')
}else{
  print('Do not reject Ho')
}


#MC example----
x = rnorm(1e7)
mean(x)
mean(x^2)
mean(x^3)
mean(x^4)

#sampling from exp(beta)------
u = runif(1e5)
beta=1
x = -beta*log(u)
options(warn=-1)
hist(x,nclss=1000) #when you use histogram, please use more than 100 bins!
### OW the professor will take off your point!!!





#HW1. generate 10000 rs from gamma5,1-------
x=NULL
beta=1;n=5
for(i in 1:10000){
  u = runif(n)
  x[i]=-beta*log(cumprod(u))
}
hist(x,nclss=1000)


#rejection sample for generating normal------
#enough sample 을 만족할때까지 뽑아야 하므로 while loop을 이용한다.
n = 1e5
x = rep(NA,n)
index=1

while(index<=n){
  y=rexp(1)#generate sample from exp(1)
  r=exp(-(y-1)^2/2) #this is our ratio r
  u=runif(1) # unif for evaluation
  
  if(u < r){
    u2 = runif(1)#unif2 for making negative value
    if(u2<0.5) x[index] = -y
    else x[index] = y
    index = index + 1 #going next step
  }
}
hist(x,nclass=100)


# HW2.posterior만들기----
n = 1e5
lambda = rep(NA,n)
data=c(8,3,4,3,1,7,2,6,2,7)
index=1

#precalculate because it is fixed value
denorm=cumprod(dpois(x=data,lambda = mean(data)))
while(index<=n){
  #step1
  lambda_candi = exp(rnorm(1,mean = log(4),sd = (0.5)))
  
  #step2
  # r = (cumprod(dpois(x=data,lambda = lambda_candi))
  #   /cumprod(dpois(x=data,lambda = mean(data))))
  r = (cumprod(dpois(x=data,lambda = lambda_candi))
       /denorm)
  
  #step3
  u=runif(1) # unif for evaluation
  
  #step4
  if(u < r){
    lambda[index] = lambda_candi #accept
    index = index + 1 #going next step
  }
}
hist(lambda,nclass=100)

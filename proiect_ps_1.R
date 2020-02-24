library(ggplot2)
poisson_ex = rpois(1000,350)
binomial_ex = rbinom(1000, 10, 0.3)
exponential_ex = rexp(1000,15)
norm_ex = rnorm(1000,350)

var(binomial_ex)
mean(binomial_ex)

var(poisson_ex)
mean(poisson_ex)

var(exponential_ex)
mean(exponential_ex)

var(norm_ex)
mean(norm_ex)

graph_binom <- function(n,prob1,prob2,prob3,prob4,prob5){

  
  x=n[length(n)]
bin1 = data.frame(b_density=dbinom(n,x, prob1), b_mass=pbinom(n,x, prob1))
bin2 = data.frame(b_density=dbinom(n,x, prob2), b_mass=pbinom(n,x, prob2))
bin3 = data.frame(b_density=dbinom(n,x, prob3), b_mass=pbinom(n,x, prob3))
bin4 = data.frame(b_density=dbinom(n,x, prob4), b_mass=pbinom(n,x, prob4))
bin5 = data.frame(b_density=dbinom(n,x, prob5), b_mass=pbinom(n,x, prob5))

 plot(bin1$b_density,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de densitate pentru distributia binomiala") 
lines(bin2$b_density,col="black")
lines(bin3$b_density,col="green")
lines(bin4$b_density,col="purple")
lines(bin5$b_density,col="orange")
legend("topright",legend=c("Fct. dens. 1","Fct. dens. 2","Fct. dens. 3","Fct. dens. 4", "Fct. dens. 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)

plot(bin1$b_mass,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de masa pentru distributia binomiala") 
lines(bin2$b_mass,col="black")
lines(bin3$b_mass,col="green")
lines(bin4$b_mass,col="purple")
lines(bin5$b_mass,col="orange")
legend("topright",legend=c("Fct. masa 1","Fct. masa 2","Fct. masa 3","Fct. masa 4", "Fct. masa 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)

}
graph_binom(0:20,0.1,0.2,0.5,0.65,0.9)


graph_poisson <- function(n,prob1,prob2,prob3,prob4,prob5){
  

  poiss1 = data.frame(p_density=dpois(n,prob1), p_mass=ppois(n,prob1))
  poiss2 = data.frame(p_density=dpois(n,prob2), p_mass=ppois(n,prob2))
  poiss3 = data.frame(p_density=dpois(n,prob3), p_mass=ppois(n,prob3))
  poiss4 = data.frame(p_density=dpois(n,prob4), p_mass=ppois(n,prob4))
  poiss5 = data.frame(p_density=dpois(n,prob5), p_mass=ppois(n,prob5))
  
  plot(poiss1$p_density,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de densitate pentru distributia Poisson") 
  lines(poiss2$p_density,col="black")
  lines(poiss3$p_density,col="green")
  lines(poiss4$p_density,col="purple")
  lines(poiss5$p_density,col="orange")
  legend("topright",legend=c("Fct. dens. 1","Fct. dens. 2","Fct. dens. 3","Fct. dens. 4", "Fct. dens. 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
  
  plot(poiss1$p_mass,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de masa pentru distributia Poisson") 
  lines(poiss2$p_mass,col="black")
  lines(poiss3$p_mass,col="green")
  lines(poiss4$p_mass,col="purple")
  lines(poiss5$p_mass,col="orange")
  legend("topright",legend=c("Fct. masa 1","Fct. masa 2","Fct. masa 3","Fct. masa 4", "Fct. masa 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
  
}

graph_poisson(0:10,0.1,1,5,10,0.75)



graph_exponential <- function(n,prob1,prob2,prob3,prob4,prob5){
  

  exp1 = data.frame(e_density=dexp(n,prob1), e_mass=pexp(n,prob1))
  exp2 = data.frame(e_density=dexp(n,prob2), e_mass=pexp(n,prob2))
  exp3 = data.frame(e_density=dexp(n,prob3), e_mass=pexp(n,prob3))
  exp4 = data.frame(e_density=dexp(n,prob4), e_mass=pexp(n,prob4))
  exp5 = data.frame(e_density=dexp(n,prob5), e_mass=pexp(n,prob5))
  
  plot(exp1$e_density,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de densitate pentru distributia exponentiala") 
  lines(exp2$e_density,col="black")
  lines(exp3$e_density,col="green")
  lines(exp4$e_density,col="purple")
  lines(exp5$e_density,col="orange")
  legend("topright",legend=c("Fct. dens. 1","Fct. dens. 2","Fct. dens. 3","Fct. dens. 4", "Fct. dens. 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
  
  plot(exp1$e_mass,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de masa pentru distributia exponentiala") 
  lines(exp2$e_mass,col="black")
  lines(exp3$e_mass,col="green")
  lines(exp4$e_mass,col="purple")
  lines(exp5$e_mass,col="orange")
  legend("topright",legend=c("Fct. masa 1","Fct. masa 2","Fct. masa 3","Fct. masa 4", "Fct. masa 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
    
}

graph_exponential(0:20,0.25,0.75,1,0.15,7)



graph_normal <- function(n,prob1,x1,prob2,x2,prob3,x3,prob4,x4,prob5,x5){
  
  norm1 = data.frame(n_density=dnorm(n,x1, prob1), n_mass=pnorm(n,x1, prob1))
  norm2 = data.frame(n_density=dnorm(n,x2, prob2), n_mass=pnorm(n,x2, prob2))
  norm3 = data.frame(n_density=dnorm(n,x3, prob3), n_mass=pnorm(n,x3, prob3))
  norm4 = data.frame(n_density=dnorm(n,x4, prob4), n_mass=pnorm(n,x4, prob4))
  norm5 = data.frame(n_density=dnorm(n,x5, prob5), n_mass=pnorm(n,x5, prob5))
  
  plot (norm1$n_density,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de densitate pentru distributia normala") 
  lines(norm2$n_density,col="black")
  lines(norm3$n_density,col="green")
  lines(norm4$n_density,col="purple")
  lines(norm5$n_density,col="orange")
  legend("topright",legend=c("Fct. dens. 1","Fct. dens. 2","Fct. dens. 3","Fct. dens. 4", "Fct. dens. 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
  
  plot (norm1$n_mass,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de masa pentru distributia normala") 
  lines(norm2$n_mass,col="black")
  lines(norm3$n_mass,col="green")
  lines(norm4$n_mass,col="purple")
  lines(norm5$n_mass,col="orange")
  legend("topright",legend=c("Fct. masa 1","Fct. masa 2","Fct. masa 3","Fct. masa 4", "Fct. masa 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
  
}

graph_normal(0:20,1,0,5,0,1,5,1,10,10,2)



aproximarea_poisson <- function(n,p,k){

  lambda=n*p #calculam lambda
  result=0
  for(x in 1:k){
    fraction=lambda^x
    fraction=fraction/factorial(x)
    result= result + (exp(-lambda))*fraction}

  return(result)
    }

aproximare_normala<- function(n,p,k){
  
  numitor=k-n*p
  numarator=sqrt(n*p-(1-p))
  result=pnorm(numitor/numarator)
  return(result)
  
}

aproximare_normala_cu_factor_de_corectie <- function(n,p,k){
  
  
  numitor=k+0.5-n*p
  numarator=sqrt(n*p-(1-p))
  result=pnorm(numitor/numarator)
  return(result)
  
}

aproximare_camp_paulson<- function (n,p,k){
  
  a_numitor=9*(n-k)
  
  a=1/a_numitor
  
  b_numitor=9*(k+1)
  
  b=1/b_numitor
  
  r_numarator=(k+1)*(1-p)
  
  r_numitor=p*(n-k)
  
  r=r_numarator/r_numitor
  
  c=(1-b)*r^(1/3)
  
  miu= 1-a
  
  sigma = sqrt(a+ b*r^(2/3))

  result=pnorm((c-miu)/sigma)
  
  return(result)
}

i=1
k=10
#for(i in 1:10)
#aprox_1=cbind(i,
#              rbinom(25,k,0.05),
#              aproximarea_poisson(25,0.05,k),
#              aproximare_normala(25,0.05,k),
#              aproximare_normala_cu_factor_de_corectie(25,0.05,k),
#              aproximare_camp_paulson(25,0.05,k)
#              )
#print(aprox_1)

#a<-aproximare_camp_paulson(25,0.05,i)
#print(a)
i=1
aproximari <- function(n,p){
while(i<=10) 
 { if(i==1)vect1<- c(pbinom(n,i,p),
           aproximarea_poisson(n,p,i),
           aproximare_normala(n,p,i),
           aproximare_normala_cu_factor_de_corectie(n,p,i),
           aproximare_camp_paulson(n,p,i))
 
if(i==2)vect2<- c(pbinom(n,i,p),
           aproximarea_poisson(n,p,i),
           aproximare_normala(n,p,i),
           aproximare_normala_cu_factor_de_corectie(n,p,i),
           aproximare_camp_paulson(n,p,i))

if(i==3)vect3<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==4)vect4<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==5)vect5<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==6)vect6<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==7)vect7<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==8)vect8<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==9)vect9<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


if(i==10)vect10<- c(pbinom(n,i,p),
                  aproximarea_poisson(n,p,i),
                  aproximare_normala(n,p,i),
                  aproximare_normala_cu_factor_de_corectie(n,p,i),
                  aproximare_camp_paulson(n,p,i))


 i=i+1
}
  
 
 matrixB <- matrix(c(vect1,vect2,vect3,vect4,vect5,vect6,vect7,vect8,vect9,vect10),ncol=5,byrow=TRUE)
 colnames(matrixB) <- c("Binomiala","Poison","Normala","Normala corectie","Camp-Paulson")
 rownames(matrixB) <- c("1","2","3","4","5","6","7","8","9","10")
 matrixB <- as.table(matrixB)
 matrixB
}


aproximari(25,0.05)
aproximari(50,0.05)
aproximari(100,0.05)

aproximari(25,0.1)
aproximari(50,0.1)
aproximari(100,0.1)






#1.6


skew_normal <- function(x,miu,sigma,lambda)
{
   
  #ddorm densitate 
  #pnorm fc de repartitie https://www.r-bloggers.com/normal-distribution-functions/
  param=(x-miu)/sigma
  densitatea=dnorm(param)
  repartitia=pnorm(lambda*param)
  
  result=(2/sigma)*densitatea*repartitia
  return(result)
}


graph_skew <- function(x1,x2,x3,x4,x5,miu,sigma,lambda){
  
  
  sk1 = data.frame(res=skew_normal(x1,miu,sigma,lambda))
  sk2 = data.frame(res=skew_normal(x2,miu,sigma,lambda))
  sk3 = data.frame(res=skew_normal(x3,miu,sigma,lambda))
  sk4 = data.frame(res=skew_normal(x4,miu,sigma,lambda))
  sk5 = data.frame(res=skew_normal(x5,miu,sigma,lambda))
  
  plot(sk1$res,xlab="n",ylab="Valori",col="red",type="l",main="Combinarea functiilor de densitate pentru repartitia normala-asimetrica") 
  lines(sk2$res,col="black")
  lines(sk3$res,col="green")
  lines(sk4$res,col="purple")
  lines(sk5$res,col="orange")
  legend("topright",legend=c("Fct. dens. 1","Fct. dens. 2","Fct. dens. 3","Fct. dens. 4", "Fct. dens. 5"),col=c("red","black","green","purple","orange"),lty=1:5,cex=0.8)
}
graph_skew(1,2,3,4,5,5,10,2)

# 1.7
f <- function(lambda,n,p)
{
  ((1-(2/pi)*(lambda^2)/(1+lambda^2))^3)/((2/pi)*((4/pi)-1)^2*(lambda^2/(1+lambda^2))^3)=(n*p*(1-p))/(1-2*p)^2
}
sig <- function(n,p,lambda)
{
  result=sqrt(n*p*(1-p)/(1-(2/pi)*lambda^2/(1+lambda^2)))
  return(result)
}
mi <- function(n,p,lambda,sigma)
{
  result=n*p-sigma*sqrt((2/pi)*lambda^2/(1+lambda^2))
  return(result)
}
new_function <- function(n,p)
{
  result<-uniroot(f,c(0,1),n,p) 
  lambda=sign(1-2*p)*sqrt(result^2)
  sigma=sig(n,p,lambda)
  miu=mi(n,p,lambda,sigma)
}

new_function(25,0.05)
new_function(25,0.1)


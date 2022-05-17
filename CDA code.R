# CDA code (Simple Linear Regression : beta = (beta_0, beta_1))
CDA_lm = function(x,y,max_iter=1000,epsilon=1e-5)
{
   # x,y : n x 1 vector, n= sample size
   # add intercept
   B = cbind(1,x)  # basis(1,x)
   J = ncol(B)  # number of basis
   
   # initialization
   beta = rep(0,J)  # initial value of beta
   BB = colSums(B^2)   # sum of B^2
   residuals = y   # initial value of residuals
   RSS = sum(residuals)/J
   
   #CDA
   for(r in 1 : max_iter){
      cat(r,"th CDA","\n")
      for(j in 1:J){
         partical_residuals = residuals + beta[j] * B[,j]
         beta[j] = sum(partical_residuals * B[,j]) / BB[j]
         residuals = partical_residuals - beta[j] * B[,j]
         cat(j,"the beta = ",beta,"\n")
      }
      update_RSS = sum(residuals^2) / J
      if(abs(update_RSS - RSS) <= epsilon)   # stopping rule
         break
      else
         RSS = update_RSS
   }
   fitted_values = y - residuals
   # list of result
   results = list()
   results$coefficients = beta
   results$fitted_values = fitted_values
   results$residuals = residuals
   results$iteration = r   # number of cycle
   results$n = length(y)
   results$p = J - 1
   return(results)
}

# example 
set.seed(1)
n = 50
x = seq(0,1,length.out = n)
f = 2 + 3 * x
y = f + rnorm(n, sd = 0.5)

# fit
fit_CDA <- CDA_lm(x, y)
plot(x,y,bty="n",col="red",cex=1) #bty : 박스 종류 col : 색, cex : 점 크기 pch : point 종류
t
lines(x,f,col="purple") # 실제 선
lines(x,fit_CDA$fitted_values,col="blue") # 코딩결과 선
title("CDA example")
legend("topright",legend=c("true function","estimated function"),
       col=c("purple","blue"), lty=c(1,1) ,cex=1.5
       ,bty="n") # cex: 크기
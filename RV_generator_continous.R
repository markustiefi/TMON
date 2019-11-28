# We create a sequence of pseudo random numbers by using the method of
# congruential. 
# x_{n+1} = ax_n+b mod m

x_0 = 123456789
m = 2**31-1
a = 16807
b = 0
n = 10000
uniforme <- function(n, x_0, m , a, b){
  x = array(0, c(n+1,1))
  x[1] = x_0
  for(i in seq(1, n)){
    x[i+1] = (a*x[i]+b) %% m
  }
  return(x[-1]/m) # Rescale the values between [0,1]
}



# Function that creates n exponencial distributed random variables with parameter lambda
rv.exp <- function(n, lambda, u){
  exp.vec = array(0, c(n,1))
  for(i in seq(1,n))
    exp.vec[i] <- -(log(u[i]))/lambda
  return(exp.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
n = 100000
lambda = 1/4
u = uniforme(n, x_0, m, a, b)
exp.vec <- rv.exp(n, lambda, u)

abs(mean(exp.vec)-1/lambda) # expectation value of exponential distribution
abs(var(exp.vec)- 1/lambda**2) # Varianz of exponential distribution

abs(mean(rexp(n,lambda))-1/lambda)
abs(var(rexp(n,lambda))- 1/lambda**2)


# Function that creates n uniformly distributed variables in the inteval (a,b)
rv.uni <- function(n, a, b, u){
  uni.vec = array(0, c(n,1))
  for(i in seq(1,n)){
    uni.vec[i] <- a +(b-a)*u[i]
  }
  return(uni.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
n = 100
a.uni = 2
b.uni = 5
u = uniforme(n,x_0,m,a,b)

uni.vec <- rv.uni(n, a.uni, b.uni,u)
abs(mean(uni.vec)-(a.uni+b.uni)/2)
abs(var(uni.vec)-(a.uni-b.uni)**2/12)

abs(mean(runif(n,a.uni,b.uni))-(a.uni+b.uni)/2)
abs(var(runif(n,a.uni,b.uni))-(a.uni-b.uni)**2/12)


# Function that creates n Weibull distributed random variables with parameter alpha and beta
rv.weibull <- function(n, alpha, beta, u){
  weibull.vec <- array(0, c(n,1))
  for(i in seq(1,n)){
    weibull.vec[i] <- (1/beta)*(-log(u[i]))**(1/alpha)
  }
  return(weibull.vec)
}

# Test mean and varianz and compare it with the implemented random generator
n = 10000
alpha = 5
beta = 0.4
u = uniforme(n,x_0,m,a,b)

weibull.vec <- rv.weibull(n,alpha,beta,u)
abs(mean(weibull.vec)-1/beta*gamma(1/alpha+1))
abs(var(weibull.vec)-(1/beta)**2*(gamma(2/alpha+1)-(gamma(1/alpha+1))**2))

abs(mean(rweibull(n,alpha,beta))-1/beta*gamma(1/alpha+1))
abs(var(rweibull(n,alpha,beta))-(1/beta)**2*(gamma(2/alpha+1)-(gamma(1/alpha+1))**2))








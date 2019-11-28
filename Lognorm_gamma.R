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


# Create n  normal distributed random variables with Box-Müller
rv.norm.BM <- function(n, u){
  norm.vec <- array(0,c(n,1))
  for(i in seq(1,n/2)){
    norm.vec[2*(i-1)+1] = sqrt(-2*log(u[2*(i-1)+1]))*cos(2*pi*u[2*i])
    norm.vec[2*i] = sqrt(-2*log(u[2*(i-1)+1]))*sin(2*pi*u[2*i])
  }
  return(norm.vec)
}

n = 100
u = uniforme(n,x_0,m,a,b)
norm.vec <- rv.norm.BM(n,u)
# mu = 0 and sigma = 1, if we want another expectation value/variance we use the transformation
# x = mu+ sigma*norm.vec


# Create n lognormal distributed random variables
rv.lognorm <- function(n, norm.vec, mu, sigma){
  lognorm.vec <- array(0,c(n,1))
  for(i in seq(1,n)){
    lognorm.vec[i] <- exp(mu + sigma*norm.vec[i])
  }
  return(lognorm.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
n = 10000
u = uniforme(n,x_0,m,a,b)
norm.vec <- rv.norm.BM(n,u)
mu = 0
sigma = 1

lognorm.vec <- rv.lognorm(n, norm.vec, mu,sigma)
abs(mean(lognorm.vec)-exp(mu+sigma**2/2))
abs(var(lognorm.vec)-(exp(sigma**2)-1)*exp(2*mu+sigma**2))



# Create n gamma distributed random variables
rv.Gamma <- function(n, a, p, u){
  gamma.vec <- array(0, c(n,1))
  for(i in seq(1,n)){
    gamma.vec[i] = - sum(log(u[(p*(i-1)+1):(p*i)]))/a
  }
  return(gamma.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
n = 100
p = 3
a.log = 0.5
u = uniforme(3*n,x_0,m,a,b)

gamma.vec <- rv.Gamma(n, a.log, p,u)
abs(mean(gamma.vec)-p/a.log)
abs(var(gamma.vec)-p/(a.log)**2)















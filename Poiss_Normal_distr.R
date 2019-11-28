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


# Create n poisson distrubed random variables wiith parameter lambda
rv.poiss <- function(n, lambda, u){
  vec.poiss <- array(0,c(n,1))
  j = 1
  for(i in seq(1,n)){
    Q <- 0
    x <- 0
    while(Q <= 1){
      Q = Q-(1/lambda)*log(u[j])
      j = j+1
      x = x+1
    }
    vec.poiss[i] <- x-1 # Because we made one step to much
  }
  return(vec.poiss)
}


# Test mean and varianz and compare it with the implemented random generator.
n = 100
lambda = 3
u = uniforme(100*n, x_0, m, a, b)

vec.poiss <- rv.poiss(n,lambda,u)
abs(mean(vec.poiss)-lambda)
abs(var(vec.poiss)-lambda)

abs(mean(rpois(n,lambda))-lambda)
abs(var(rpois(n,lambda))-lambda)


# Create n normal distributed random variables  with the central limit theorem.
rv.norm.TCL <- function(n,u,k){
  norm.vec <- array(0,c(n,1))
  for(i in seq(1,n)){
    print(k*(i-1)+1)
    norm.vec[i] <- (sum(u[((k*(i-1))+1):(k*i)])-k/2)/sqrt(k/12)
  }
  return(norm.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
k = 1000
n = 100
u = uniforme(2*k*n,x_0,m,a,b)
norm.vec <- rv.norm.TCL(n,u,k)
mean(norm.vec)
mean(rnorm(100))
var(norm.vec)
var(rnorm(100))

# Create n  normal distributed random variables with Box-Müller
rv.norm.BM <- function(n, u){
  norm.vec <- array(0,c(n,1))
  for(i in seq(1,n/2)){
    norm.vec[2*(i-1)+1] = sqrt(-2*log(u[2*(i-1)+1]))*cos(2*pi*u[2*i])
    norm.vec[2*i] = sqrt(-2*log(u[2*(i-1)+1]))*sin(2*pi*u[2*i])
  }
  return(norm.vec)
}

# Test mean and varianz and compare it with the implemented random generator.
n = 100
u = uniforme(n,x_0,m,a,b)
norm.vec <- rv.norm.BM(n,u)
mean(norm.vec)
mean(rnorm(100))
var(norm.vec)
var(rnorm(100))





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


# Simple algorithm of acception and rechazo.
Acept.Rech <- function(n,a1,a2,c,f,u){
  acept.vec <- array(0,c(n,1))
  i = 0 # Only make a step in the while loop if
  # we accept
  j = 1 # Make a step within the generated uniformly
  # distributed variables
  g = 0
  un = 0
  while (i < n) {
    g = 2*j
    un = 2*j+1
    x = a1+ (a2-a1)*u[un]
    y = c*u[g]
    if(y <= f(x)){
      i = i+1
      acept.vec[i] <- x
    }
    j = j+1
  }
  return(acept.vec)
}

# Function to test the aceptacion-rechazo with.
# Is the classic hat function within (0,2).
f <- function(x){
  if(x < 0)
    return(0)
  else if(0 <= x & x <= 1)
    return(x)
  else if(1 <= x & x <= 2)
    return(2-x)
  else
    return(0)
}

n = 100
a1 = 0
a2 = 2
c = 1
u = uniforme(10*n, x_0, m, a, b)

# Create n random values of our distribution.
acept.vec <- Acept.Rech(n, a1, a2, c, f, u)
acept.vec
# Mean and Var
mean(acept.vec)
var(acept.vec)


# Generalised version of Aceptacion-Rechazo
Acept.Rech.gen <- function(n,a,G,g,f,u){
  acept.vec <- array(0,c(n,1))
  i = 0 # Only make a step in the while loop if
  # we accept
  j = 1 # Make a step within the generated uniformly
  # distributed variables
  ge = 0
  un = 0
  while (i < n) {
    ge = 2*j
    un = 2*j+1
    x = G(u[un]) # Function G gives us g distributed
    # random variables.
    y = a*g(x)*u[ge]
    if(y <= f(x, 0, 1)){
      i = i+1
      acept.vec[i] <- x
    }
    j = j+1
  }
  return(acept.vec)
}

# Distribution function of logistic distribution
log.ver.fkt <- function(uni){
  return(-log(abs(1-uni)/uni))
}

# Density function of logistic distribution
log.dicht.fkt <- function(x){
  return(exp(-x)/((1+exp(-x))**2))
}

# Density function of normal distribution
nor.dicht.fkt <- function(x, mu, sigma){
  return(1/(sqrt(2*pi*sigma**2))*exp(-((x-mu)**2/(2*sigma**2))))
}

n = 100
a = 4

u = uniforme(100*n, x_0, m, a, b)

# Create n random values of our distribution.
acept.vec <- Acept.Rech.gen(n,a,log.ver.fkt,log.dicht.fkt,nor.dicht.fkt,u)
# Mean and Var
mean(acept.vec)
var(acept.vec)



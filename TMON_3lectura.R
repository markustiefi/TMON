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

u = uniforme(n, x_0, m, a, b)


# With the uniform distributed vector, which was initialised before we are generating 100 binomially distributed 
# random variables with m = 10 and p = 0.3. 
p = 0.3
m = 10
n = 100

# Function which gives us random binomial distributed variables.
binomi <- function(n, p, m, u){
  b <- array(0, c(n, 1))
  for(i in seq(1,n)){
    bin <- 0
    for(j in seq(1,m)){
      k = j+10*(i-1)
      if(u[k] <= p){
        
        bin = bin+1
      }
    }
    b[i] = bin
  }
  return(b)
}

# Test the results
bin.vec = binomi(n,p,m,u)
bin.vec.prob <- as.matrix(table(bin.vec)/n)
as.array(bin.vec.prob)[1]

bin.vec
bin.vec.prob <- as.matrix(table(bin.vec)/n)
bin.vec.prob

# And compare it with the theoretical values of the binomial distribution.
theo.bin <- array(0,c(10,0))
for(i in seq(0,m)){
  theo.bin[i+1] <- choose(m,i)*p^i*(1-p)^(m-i)
}
theo.bin

cbind(theo.bin, bin.vec.prob[,1], abs(theo.bin[1:length(bin.vec.prob[,1])]-bin.vec.prob[,1]))


# Function which gives us random geometric distributed variables.
geometric <- function(n, p, u){
  geom <- array(0, c(n,1))
  for(i in seq(1,n)){
    geom[i] <- floor(log(1-u[i])/log(1-p))
    # Don't forget to round the value, geomtric distribution only can take integers.
  }
  return(geom)
}

p = 0.7
n = 100000
u = uniforme(n, x_0, m, a, b)
mean(geometric(n, p, u))
((1-p)/p)
mean(geometric(n, p, u)-((1-p)/p))

var(geometric(n,p,u))-((1-p)/(p**2))
var(geometric(n,p,u))
((1-p)/(p**2))
rg <- rgeom(100000, 0.7)
var(rg)
mean(rg)


# We create a sequence of pseudo random numbers by using the method of
# congruential. 
# x_{n+1} = ax_n+b mod m

x_0 = 123456789
m = 2**31-1
a = 16807
b = 0
uniforme <- function(n, x_0, m , a, b){
  x = array(0, c(n+1,1))
  x[1] = x_0
  for(i in seq(1, n)){
    x[i+1] = (a*x[i]+b) %% m
  }
  return(x[-1]/m) # Rescale the values between [0,1]
}

u = uniforme(100, x_0, m, a, b)

# Take a look on how close
# we are to the mean/var of the equal distribution.
abs(mean(u)-0.5)
abs(var(u)-1/12)

# Given the vector of pseudo equal distributed numbers, we can
# generate a vector of discrete random variables.
# Note that the biggest probabilities come first out of 
# computational reasons.
rv = array(0, c(100,1))
for(i in seq(1, 100)){
  if(u[i] < .5){
    rv[i]= 2
  }
  else if(u[i] < .7){
    rv[i] = 1 
  }
  else if(u[i] < .9){
    rv[i] = 3
  }
  else{
    rv[i] = 0
  }
}
# Create a table frame to analyse our vector of random variables.
# Further, calculate the relative probability of each value.
table.rv <- as.data.frame(table(rv))
table.rv$probability <- table.rv$Freq/100
table.rv

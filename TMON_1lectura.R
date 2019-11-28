##########################
# Metodo de 2n quadratico
##########################

x_0 = 3708
n = 8
valores = array(0,c(n,1))
x_squ = array(0,c(n,1))
i=0
x = x_0
while(i <n){
  i = i + 1
  x = x**2
  x_squ[i] = x
  x = (floor(x/100))%%10000
  valores[i] = x
}
comb = cbind(valores, x_squ)

##########################
# Metodo de Lehmer
##########################
x_0 = 4122
K = 76
k = 2
n = 10
valores_lehm = array(0,c(n,1))
x_lehm = array(0,c(n,1))
i=0
x = x_0
while(i < n){
  i = i + 1
  x = x*K
  x_lehm[i] = x
  x = (x%%10000)-floor(x/10000)
  valores_lehm[i] = x
}
comb_lehm = cbind(valores_lehm, x_lehm)
comb_lehm





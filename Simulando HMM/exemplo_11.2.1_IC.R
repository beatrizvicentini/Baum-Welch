#install.packages('HiddenMarkov')
library(HiddenMarkov)

#objetivo: simular um HMM a partir da matriz de transicao e da matriz de emissao

# A: matriz de prob. de transicao k por k
# i : estado atual da cadeia
# retornar um novo estado
f.update <- function(A,i){
    # numero de estados ocultos
    # k <- dim(A)[1]
    return(which(rmultinom(1,1,A[i,])==1))
}

# B: matriz de emissao
# linha de B: estado oculto ; coluna de B: observacoes
# i : estado oculto atual
f.obs <- function(B,i){
  return(which(rmultinom(1,1,B[i,])==1))
}


barplot_simbolos <- function(q, r, titulo) {
  A <- matrix(c(0.5, 0.5, q, 1-q), byrow=TRUE, nrow=2)
  B <- matrix(c(0.5, 0.5, r, 1-r), byrow=TRUE, nrow=2)
  print(A)
  print(B)
  #SIMULANDO A CADEIA OCULTA
  n <- 1000
  x0 <- 1
  
  #valores da cadeia simulada
  cadeia.sim <- vector()
  #valores da sequencia observada
  cadeia.obs <- vector()
  cadeia.sim[1] <- x0
  cadeia.obs[1] <- f.obs(B,x0)
  for(i in 2:n){
    cadeia.sim[i] <- f.update(A,cadeia.sim[i-1])
    cadeia.obs[i] <- f.obs(B,cadeia.sim[i])
  }
  
  cadeia.sim
  cadeia.obs
  #para ter uma cadeia com estados 0 e 1
  #cadeia.sim - 1
  
  table(cadeia.sim)
  table(cadeia.sim)/n
  prop.obs <- table(cadeia.obs)/n
            
  names(prop.obs) <- c("a","b")          
  barplot(prop.obs, width=c(1,1),
          ylim = c(0,1),
          main=titulo)
}

#alterando os valores de q e r
q <- 0.1
r <- 0.1
barplot_simbolos(q,r,"Proporção de símbolos emitidos pela cadeia - q=0.1 e r=0.1")

q <- 0.1
r <- 0.9
barplot_simbolos(q,r,"Proporção de símbolos emitidos pela cadeia - q=0.1 e r=0.9")

q <- 0.9
r <- 0.1
barplot_simbolos(q,r,"Proporção de símbolos emitidos pela cadeia - q=0.9 e r=0.1")

q <- 0.9
r <- 0.9
barplot_simbolos(q,r,"Proporção de símbolos emitidos pela cadeia - q=0.9 e r=0.9")

#######-----------------------
# Emissao: Normal
  #vamos assumir que a distribuicao da sequencia observada seja uma normal(mu,sigma^2)
# i estado oculto atual
f.obs <- function(mu,sigma,i){
  return( rnorm(1,mu[i],sigma[i]) )
}

#parâmetros, matrizes, n e x0
mu <- c(0,8)
sigma <- c(1,1)
q <- 0.2
A <- matrix(c(0.5, 0.5, q, 1-q), byrow=TRUE, nrow=2)
n <- 1000
x0 <- 1

#valores da cadeia simulada
cadeia.sim <- vector()
#valores da sequencia observada
cadeia.obs <- vector()
cadeia.sim[1] <- x0
cadeia.obs[1] <- f.obs(mu,sigma,x0)
for(i in 2:n){
  cadeia.sim[i] <- f.update(A,cadeia.sim[i-1])
  cadeia.obs[i] <- f.obs(mu,sigma,cadeia.sim[i])
}

cadeia.sim
cadeia.obs

table(cadeia.sim)/n

hist(cadeia.obs, 
     main="Histograma de Símbolos Observados - HMM - Normal",
     freq = FALSE,
     xlab="observações",
     col="lightblue")
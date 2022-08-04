#objetivo: simular um HMM a partir da matriz de transicao e da matriz de emissao
# 1)definir A com q=0.1, 0.5 e 0.9
# 2)definir B com r=0.1, 0.5 e 0.9
# 3)simular 3 ou mais HMM com as diferentes probs nas matrizes
# 4)comparar as simulacoes por graficos


#install.packages('HiddenMarkov')
library(HiddenMarkov)

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


#q e r = 0.1
#definindo A com q=0.1

q <- 0.2
r <- 0.02
A <- matrix(c(0.5, 0.5, q, 1-q), byrow=TRUE, nrow=2)
B <- matrix(c(0.5, 0.5, r, 1-r), byrow=TRUE, nrow=2)

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
table(cadeia.obs)/n


barplot(table(cadeia.obs)/n,
        ylim = c(0,1),
        main="Quantidade de estados ocultos da simulação do HMM quando", bquote(r==.(r)))
##erro: Error in rep_len(width, NR) : attempt to replicate non-vector
#In addition: Warning message:
#In mean.default(width) : argumento não é numérico nem lógico: retornando NA



#> table(cadeia.sim)
#cadeia.sim
#0  1 
#37 64 
#> table(cadeia.sim)/n
#cadeia.sim
#0    1 
#0.37 0.64
#essa fun??o mostra que a cadeia n?o fica absorvente no estado 1 (ou 2 no livro)
#como esper?vamos

#GERANDO AS OBSERVA??ES
#para um i percorrendo a cadeia, se o estado ? 0 ou 1, vai gerar uma observa??o
#O1 ou O2 que provavelmente vai sair como 0 e 1. N?o sei exatamente como
#implementar mas acredito que seja p?ss?vel com essa fun??o se adaptar um pouco
#peguei ela do trabalho de processos onde geramos uma cadeia. preciso adaptar e
#tirar a dependencia do estado anterior, s? n?o sei como ainda

#######-----------------------
#######-----------------------
# Emissao: Normal
  #vamos assumir que a distribuicao da sequencia observada seja uma normal(mi,sigma^2)
# i estado oculto atual
f.obs <- function(mu,sigma,i){
  return( rnorm(1,mu[i],sigma[i]) )
}

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


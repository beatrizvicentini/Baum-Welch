## Definindo bibliotecas
library(HiddenMarkov)
library(HMM)

### BAUM-WELCH PARA HMM INICIAL DIFERENTE DO VERDADEIRO

## hmm verdadeiro cujo vamos obter a sequencia observada
q <- 0.2
r <- 0.02
A <- matrix(c(0.5, 0.5, q, 1-q), byrow=TRUE, nrow=2)
B <- matrix(c(0.5, 0.5, r, 1-r), byrow=TRUE, nrow=2)
n <- 5000

hmm = initHMM(c("0","1"), c("a","b"), c(.5,.5), transProbs=A, emissionProbs=B)

f = simHMM(hmm, n)
sim = f$states
obs <- f$observation
print(obs)

## hmm diferente ao verdadeiro que usaremos para testar o baum-welch
p <- 0.9
o <- 0.09
Af <- matrix(c(0.9, 0.1, p, 1-p), byrow=TRUE, nrow=2)
Bf <- matrix(c(0.99, 0.01, o, 1-o), byrow=TRUE, nrow=2)

hmmF = initHMM(c("0","1"), c("a", "b"), c(.4,.6), transProbs = Af,
               emissionProbs = Bf)

# definindo BW com HMM "VERDADEIRO"
  # note que estamos usando as observações verdadeira -> obs

bwF = baumWelch(hmmF,obs,100)
bwFstates = (bwF$hmm$states)
bwFsymbols = (bwF$hmm$symbols)
bwFstartprobs = (bwF$hmm$startProbs)
bwFtransprobs = (bwF$hmm$transProbs)
bwFemissionprobs = (bwF$hmm$emissionProbs)


hmmBW = initHMM(c("0","1"), c("a", "b"), c(.1,.9), transProbs = bwF$hmm$transProbs,
                emissionProbs = bwF$hmm$emissionprobs)


## agora vamos rodar o BW
# variando k
# n. de repetições do Baum-Welch
K <- c(50,75,100,150,200)

#tamanho da amostra
n <- 1000

# R numero de repetiçõees
R <- 10
#cada linha é referente a um estado/obs
#cada coluna é referente a uma escolha de K
dif.emission0a <- matrix(nrow=R,ncol=length(K))
dif.emission1a <- matrix(nrow=R,ncol=length(K))
dif.trans00 <-  matrix(nrow=R,ncol=length(K))
dif.trans10 <-  matrix(nrow=R,ncol=length(K))


for(k in 1:length(K)){
  for(r in 1:R){
    amostra <- simHMM(hmm, n)
    bwF = baumWelch(hmmF,obs,K[k])
    dif.trans00[r,k] <- ((bwF$hmm$transProbs - hmmF$transProbs)[,1])[1]
    dif.trans10[r,k] <- ((bwF$hmm$transProbs - hmmF$transProbs)[,1])[2]
    dif.emission0a[r,k] <- ((bwF$hmm$emissionProbs - hmmF$emissionProbs)[,1])[1]
    dif.emission1a[r,k] <- ((bwF$hmm$emissionProbs - hmmF$emissionProbs)[,1])[2]
    print(c(k,r))
  }
}

med.trans00 <- apply(dif.trans00,2,mean)
med.trans10 <- apply(dif.trans10,2,mean)
med.emission0a <- apply(dif.emission0a,2,mean)
med.emission1a <- apply(dif.emission1a,2,mean)


# plotando gráficos
ymax <- max(abs(med.trans00),abs(med.trans10),abs(med.emission0a),abs(med.emission1a))
ymin <- min(abs(med.trans00),abs(med.trans10),abs(med.emission0a),abs(med.emission1a))

par(mfrow=c(1,2))
plot(K,abs(med.trans00),type="b",xlab="K",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="prob. transição")
lines(K,abs(med.trans10),type="b",col="red3",pch=17)
legend(140,0.25, legend=c("0-0","1-0"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

plot(K,abs(med.emission0a),type="b",xlab="K",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="prob. emissão")
lines(K,abs(med.emission1a),type="b",col="red3",pch=17)
legend(140,0.28, legend=c("0-a","1-a"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

write.table(dif.trans00, file="dif_trans00", row.names=FALSE, col.names=FALSE)

#############################################################################
# variando n
# n. de repetições do Baum-Welch
k <- 100

#tamanho da amostra
N <- c(1000, 1500, 2000, 2500, 3000)

# R numero de repetiçõees
R <- 10

for(i in 1:length(N)){
  for(r in 1:R){
    amostra <- simHMM(hmm, N[i])
    bwF = baumWelch(hmmF,obs,k)
    dif.trans00[r,i] <- ((bwF$hmm$transProbs - hmmF$transProbs)[,1])[1]
    dif.trans10[r,i] <- ((bwF$hmm$transProbs - hmmF$transProbs)[,1])[2]
    dif.emission0a[r,i] <- ((bwF$hmm$emissionProbs - hmmF$emissionProbs)[,1])[1]
    dif.emission1a[r,i] <- ((bwF$hmm$emissionProbs - hmmF$emissionProbs)[,1])[2]
    print(c(i,r))
  }
}

med.trans00 <- apply(dif.trans00,2,mean)
med.trans10 <- apply(dif.trans10,2,mean)
med.emission0a <- apply(dif.emission0a,2,mean)
med.emission1a <- apply(dif.emission1a,2,mean)

# plotando gráficos
ymax <- max(abs(med.trans00),abs(med.trans10),abs(med.emission0a),abs(med.emission1a))
ymin <- min(abs(med.trans00),abs(med.trans10),abs(med.emission0a),abs(med.emission1a))

par(mfrow=c(1,2))
plot(N,abs(med.trans00),type="b",xlab="N",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="prob. transição")
lines(N,abs(med.trans10),type="b",col="red3",pch=17)
legend(2200,0.25, legend=c("0-0","1-0"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

plot(N,abs(med.emission0a),type="b",xlab="N",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="prob. emissão")
lines(N,abs(med.emission1a),type="b",col="red3",pch=17)
legend(2200,0.6, legend=c("0-a","1-a"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

write.table(dif.trans00, file="dif_trans00", row.names=FALSE, col.names=FALSE)

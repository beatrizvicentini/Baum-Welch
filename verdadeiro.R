## Definindo bibliotecas
library(HiddenMarkov)
library(HMM)

### BAUM-WELCH PARA HMM INICIAL PRÓXIMO AO VERDADEIRO

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

## hmm próximo ao verdadeiro que usaremos para testar o baum-welch
l <- 0.3
m <- 0.03
Av <- matrix(c(0.35, 0.65, l, 1-l), byrow=TRUE, nrow=2)
Bv <- matrix(c(0.35, 0.65, m, 1-m), byrow=TRUE, nrow=2)

hmmV = initHMM(c("0","1"), c("a","b"), c(.5,.5), transProbs=Av, emissionProbs=Bv)

# definindo BW com HMM "VERDADEIRO"
  # note que estamos usando as observações verdadeira -> obs

#bwV = baumWelch(hmmV,obs,100)
#bwVstates = (bwV$hmm$states)
#bwVsymbols = (bwV$hmm$symbols)
#bwVstartprobs = (bwV$hmm$startProbs)
#bwVtransprobs = (bwV$hmm$transProbs)
#bwVemissionprobs = (bwV$hmm$emissionProbs)


## agora vamos rodar o BW
# n. de repetições do Baum-Welch k variando
K <- c(50,75,100,150,200)
#k <- c(5, 10, 15, 20, 25)
#tamanho da amostra
n <- 1000
#n <- 100
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
    bwV = baumWelch(hmmV,amostra$observation,K[k])
    bwVstates = (bwV$hmm$states)
    bwVsymbols = (bwV$hmm$symbols)
    bwVstartprobs = (bwV$hmm$startProbs)
    bwVtransprobs = (bwV$hmm$transProbs)
    bwVemissionprobs = (bwV$hmm$emissionProbs)
    dif.trans00[r,k] <- ((bwV$hmm$transProbs - hmm$transProbs)[,1])[1]
    dif.trans10[r,k] <- ((bwV$hmm$transProbs - hmm$transProbs)[,1])[2]
    dif.emission0a[r,k] <- ((bwV$hmm$emissionProbs - hmm$emissionProbs)[,1])[1]
    dif.emission1a[r,k] <- ((bwV$hmm$emissionProbs - hmm$emissionProbs)[,1])[2]
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
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="probabilidade de transição",cex.main=0.9)
lines(K,abs(med.trans10),type="b",col="red3",pch=17)
legend(130,0.013, legend=c("0-0","1-0"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

plot(K,abs(med.emission0a),type="b",xlab="K",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="probabilidade de emissão",cex.main=0.9)
lines(K,abs(med.emission1a),type="b",col="red3",pch=17)
legend(140,0.045, legend=c("0-a","1-a"),
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
    bwV = baumWelch(hmmV,amostra$observation,k)
    bwVstates = (bwV$hmm$states)
    bwVsymbols = (bwV$hmm$symbols)
    bwVstartprobs = (bwV$hmm$startProbs)
    bwVtransprobs = (bwV$hmm$transProbs)
    bwVemissionprobs = (bwV$hmm$emissionProbs)
    dif.trans00[r,i] <- ((bwV$hmm$transProbs - hmm$transProbs)[,1])[1]
    dif.trans10[r,i] <- ((bwV$hmm$transProbs - hmm$transProbs)[,1])[2]
    dif.emission0a[r,i] <- ((bwV$hmm$emissionProbs - hmm$emissionProbs)[,1])[1]
    dif.emission1a[r,i] <- ((bwV$hmm$emissionProbs - hmm$emissionProbs)[,1])[2]
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
legend(2250,0.20, legend=c("0-0","1-0"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

plot(N,abs(med.emission0a),type="b",xlab="N",
     ylab="diferença",ylim=c(ymin,ymax),col="blue3",pch=19,main="prob. emissão")
lines(N,abs(med.emission1a),type="b",col="red3",pch=17)
legend(2250,0.20, legend=c("0-a","1-a"),
       col=c("blue3","red3"), lty=c(1,1), cex=0.8, pch=c(19,17),pt.cex=1,
       box.lty=0,seg.len=3)

write.table(dif.trans00, file="dif_trans00", row.names=FALSE, col.names=FALSE)

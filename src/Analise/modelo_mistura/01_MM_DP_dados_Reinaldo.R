rDiscreta<-function(p){
  u<-runif(1)
  P<-cumsum(p)
  val<-sum(P<u)+1
  val
}
###
################
### gera valores da posteriori de alpha
#
gera_eta_alpha<-function(alpha,a_alph_prior,b_alph_prior,K,m){
	eta_aux<-rbeta(1,alpha+1,m)
	aux_prob<-(a_alph_prior+K-1)/(m*(b_alph_prior-log(eta_aux)))
	prob_alpha<-aux_prob/(1+aux_prob)
	unif<-runif(1)
	if (unif<=prob_alpha) alpha<-rgamma(1,a_alph_prior+K,b_alph_prior-log(eta_aux)) else alpha<-rgamma(1,a_alph_prior+K-1,b_alph_prior-log(eta_aux))
	return(alpha)}
##
###### Lendo dados
##
caminho<-"/home/reinaldo/dp_daiane"
#caminho<-"/home/daiane/Reinaldo/" #onde está salva a sua base e onde serão salvos os arquivos com as cadeias MCMC
#dados<-readRDS("dtm_filtrada_tf_30.rds")
#dados<- readRDS("/home/reinaldo/dtm_filtrada_tf_30_2.rds")
#dados<- readRDS("/home/reinaldo/dtm_hibrida.rds")
dados<- readRDS("/home/reinaldo/dtm_final_unicas.rds")
dim(dados)
#
palavras<-dados$dimnames$Terms
y_ap<-matrix(0,nrow=dados$nrow,ncol=dados$ncol)
#y_ap[dados$i,dados$j]<-dados$v
library(compiler)
enableJIT(3)
for (i in 1:length(dados$i)) y_ap[dados$i[i],dados$j[i]]<-dados$v[i]
#
#
amostrasfin<-100
burnin<-200
saltos<-5
AmostrasTotal<-burnin+amostrasfin*saltos
Nt<-nrow(y_ap)
#
library(compiler)
enableJIT(3)
#
# Initialize Sj and posterior parameters
#
set.seed(100)
alpha<-1
beta0<-rep(1,ncol(y_ap)) # G0
a_alph_prior<-b_alph_prior<-0.01 # parâmetros da dist gama para o parâmetro de concentração
Sj<-rep(1,nrow(y_ap)) # chute inicial para os grupos. Aqui é um único grupo com todas as músicas dentro. Não precisa alterar porque o método busca novos grupos
nk<-table(Sj)
K<-length(nk)
betapost<-matrix(0,nrow=ncol(y_ap),ncol=K)
#
for (k in 1:K) betapost[,k]<-apply(y_ap[Sj==k,],2,sum)+beta0
#
# Gibbs sampling of cluster indicator
#
library(MGLM)
#
for (int in 1:AmostrasTotal){
	cat('\n',int,K, alpha)
	for (i in 1:Nt){
		nk<-numeric(K)
		for (k in 1:K) 	nk[k]<-sum(Sj[-i]==k)
		betapostnew<-betapost
		if (nk[Sj[i]]>0) betapostnew[,Sj[i]]<-betapostnew[,Sj[i]]-y_ap[i,]
		prob<-numeric(K)
		for (k in 1:K) prob[k]<-log(nk[k])+ddirmn(y_ap[i,], betapostnew[,k])
		prob<-c(prob,log(alpha)+ddirmn(y_ap[i,], beta0))
		prob<-prob-max(prob)
		prob<-exp(prob)		
		Snew<-rDiscreta(prob/sum(prob))
#
		if (Snew != Sj[i] & Snew <= K){
			Sj[i]<-Snew
			nk[Sj[i]]<-nk[Sj[i]]+1
			betapost<-betapostnew
			betapost[,Sj[i]]<-betapost[,Sj[i]]+y_ap[i,]}
#
		if (Snew != Sj[i] & Snew > K){
			Sj[i]<-Snew
			nk<-c(nk,1)
			betapost<-betapostnew
			betapost<-cbind(betapost,y_ap[i,]+beta0)}
#
		while (length(table(Sj))<max(Sj)){ # exclude empty clusters
			categr<-as.numeric(as.character(data.frame(table(Sj))[,1]))
			categd<-seq(1:length(table(Sj)))
			dif<-which(categr!=categd)
			Sj[which(Sj>dif[1])]<-Sj[which(Sj>dif[1])]-1
			betapost<-matrix(c(betapost[,-dif[1]]),nrow=ncol(y_ap))
			K<-ncol(betapost)}
#
		if (length(table(Sj))<K){
			betapost<-matrix(c(betapost[,-K]),nrow=ncol(y_ap))
			K<-ncol(betapost)}
#
		K<-ncol(betapost)}
	#
	alpha<-gera_eta_alpha(alpha,a_alph_prior,b_alph_prior,K,Nt)
	#
	if (int>burnin & int%%saltos==0){
		cat('',Sj,file=paste(caminho,"Sj.txt",sep=""),append=T)
		cat('',alpha,file=paste(caminho,"Alpha.txt",sep=""),append=T)}
}
# toda vez que for rodar de novo o código por algum motivo, tem que apagar esses dois arquivos do diretório Sj.txt e Alpha.txt


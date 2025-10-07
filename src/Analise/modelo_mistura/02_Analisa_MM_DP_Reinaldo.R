# Seu script de pós-processamento corrigido
iter<-amostrasfin
#
##################
# Avalia o número de grupos
##################
#
#caminho<-"/home/reinaldo/dp_daiane"
caminho<-"/home/reinaldo/simulacao_antiga/dp_daiane"
#
Sj<-scan(file=paste(caminho,"Sj.txt",sep=""))
alpha<-scan(file=paste(caminho,"Alpha.txt",sep=""))
#
library(coda) ### isso aqui é pra analisar a convergência da cadeia, nem precisa rodar
plot(alpha,type='l')
log_v<-mcmc(alpha)
effectiveSize(log_v)
geweke.diag(log_v)
mean(alpha)
#
#3013
m<-2984 # <--- CORRIGIDO: Este deve ser o número de linhas da sua DTM filtrada
S<-matrix(Sj,ncol=m,nrow=iter,byrow=TRUE)
#
K<-NULL
for (i in 1:nrow(S)) K[i]<-length(table(S[i,]))
plot(K)
#
Sj.j<-S #matriz de agrupamento a posteriori
prob.eq<-matrix(0,nrow=ncol(Sj.j),ncol=ncol(Sj.j))
library(compiler)
enableJIT(3)
for (i in 1:ncol(Sj.j)){
  for (j in 1:ncol(Sj.j)){
    prob.eq[i,j]<-round(sum(Sj.j[,i]==Sj.j[,j])/length(Sj.j[,i]),5)*100}}
#
thresh<-0.50*100 # definindo os grupos finais
clust_f<-c(1,rep(0,(ncol(Sj.j)-1)))
for (i in 2:ncol(Sj.j)){
  #for (i in 310:514){
  if (max(prob.eq[i,1:(i-1)])>thresh) clust_f[i]<-clust_f[which(prob.eq[i,1:(i-1)]==max(prob.eq[i,1:(i-1)]))[1]] else clust_f[i]<-max(clust_f[1:(i-1)]+1)}
#
thesing<-0.05*100 # juntando outliers que aparecem pelo menos 30% das vezes juntos
singl<-which(clust_f %in% which(table(clust_f)==1))
prob.eq.sin<-prob.eq[singl,]
for (i in 1:nrow(prob.eq.sin)){
  prob.eq.sin[i,singl[i]]<-0
  if (max(prob.eq.sin[i,])>thesing) clust_f[singl[i]]<-clust_f[which(prob.eq.sin[i,]==max(prob.eq.sin[i,]))[1]]}
while (length(table(clust_f))<max(clust_f)){ # exclude empty clusters
  categr<-as.numeric(as.character(data.frame(table(clust_f))[,1]))
  categd<-seq(1:length(table(clust_f)))
  dif<-which(categr!=categd)
  clust_f[which(clust_f>dif[1])]<-clust_f[which(clust_f>dif[1])]-1}
#

#cat('',clust_f,"\n",file=paste(caminho,"Grupos_MM_DP_3.txt",sep=""),append=T)
saveRDS(clust_f, file = paste0(caminho, "Grupos_MM_DP_4.rds"))
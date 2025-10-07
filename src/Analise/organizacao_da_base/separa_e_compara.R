library(readr)
musicas_diferentes_artistas <- read_csv("~/Reinaldo/data/musicas_diferentes.csv")
#musicas_diferentes_artistas<-musicas_diferentes_artistas[1:481,]
df_m_dif<-rbind(musicas_diferentes_artistas,df_diferentes_2)
View(df_m_dif)


df_m_dif_2<-cbind(df_m_dif$Nome_Limpo,df_m_dif$Artista.x,df_m_dif$Ano,df_m_dif$Codigo)

View(df_m_dif_2)

colunas_padrao <- c("Nome", "Artista", "Ano", "Codigo")
colnames(df_m_dif_2)<-colunas_padrao


df_m_dif_2<-as.data.frame(df_m_dif_2)
write_csv(df_m_dif_2, "/home/daiane/Reinaldo/musicas_diferentes.csv")

##########################################################################################
df_na_json_2<-df_na_json[,1:4]
colnames(df_na_json_2)<-colunas_padrao
write_csv(df_na_json_2, "/home/daiane/Reinaldo/musicas_na2.csv")
View(df_na_json_2)

############################################################################################

df_diferentes_3<-df_diferentes_2[,1:4]
colnames(df_diferentes_3)<-colunas_padrao
View(df_diferentes_3)
write_csv(df_diferentes_3, "/home/daiane/Reinaldo/musicas_dif_2.csv")
d<-c(8575)





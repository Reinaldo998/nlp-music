library(dplyr)

# Carregando os data.frames exportados para cada ano

nomes_musicas_58<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_58_2000.csv",header = TRUE)
nomes_musicas_91<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_91.csv",header = TRUE)
nomes_musicas_01<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_01.csv",header = TRUE)
nomes_musicas_02<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_02.csv",header = TRUE)
nomes_musicas_03<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_03.csv",header = TRUE)
nomes_musicas_04<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_04.csv",header = TRUE)
nomes_musicas_05<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_05.csv",header = TRUE)
nomes_musicas_06<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_06.csv",header = TRUE)
nomes_musicas_07<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_07.csv",header = TRUE)
nomes_musicas_08<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_08.csv",header = TRUE)
nomes_musicas_09<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_09.csv",header = TRUE)
nomes_musicas_10<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_10.csv",header = TRUE)
nomes_musicas_11_14<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_11_14.csv",header = TRUE)
nomes_musicas_15<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_15.csv",header = TRUE)
nomes_musicas_16<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_16.csv",header = TRUE)
nomes_musicas_17<- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_17.csv",header = TRUE)

colunas_padrao <- c("Nome", "Artista", "Ano", "Codigo")

colnames(nomes_musicas_11_14) <- colunas_padrao
colnames(nomes_musicas_58)<-colunas_padrao
colnames(nomes_musicas_91)<-colunas_padrao
colnames(nomes_musicas_01)<-colunas_padrao
colnames(nomes_musicas_02)<-colunas_padrao
colnames(nomes_musicas_03)<-colunas_padrao
colnames(nomes_musicas_04)<-colunas_padrao
colnames(nomes_musicas_05)<-colunas_padrao
colnames(nomes_musicas_06)<-colunas_padrao
colnames(nomes_musicas_07)<-colunas_padrao
colnames(nomes_musicas_08)<-colunas_padrao
colnames(nomes_musicas_09)<-colunas_padrao
colnames(nomes_musicas_10)<-colunas_padrao
colnames(nomes_musicas_15)<-colunas_padrao
colnames(nomes_musicas_16)<-colunas_padrao
colnames(nomes_musicas_17)<-colunas_padrao

#nomes_musicas_17 <- read.csv("C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_17.csv")

#View(nomes_musicas_07)


# Juntando todos os data.frames em um único
musicas_completas <- bind_rows(nomes_musicas_58,nomes_musicas_91,nomes_musicas_01,nomes_musicas_02,nomes_musicas_03,nomes_musicas_04,nomes_musicas_05,nomes_musicas_06,nomes_musicas_07,nomes_musicas_08,nomes_musicas_09,nomes_musicas_10,nomes_musicas_11_14,nomes_musicas_15,nomes_musicas_16,nomes_musicas_17)


# Exibindo o resultado final
#print(todos_dados) 2036

musicas_completas[2036, "Artista"] <- "Sidney Magal"
musicas_completas[2036, "Nome"] <-"Sandra Rosa Madalena A Cigana"
musicas_completas[3226, "Artista"] <- "Engenheiros do Hawaii"
musicas_completas[3226, "Nome"] <-"Era Um Garoto Que Como Eu Amava os Beatles e os Rolling Stones"
musicas_completas[4561, "Artista"] <- "Marina Lima & Alvin L"
musicas_completas[4561, "Nome"] <- "SUGAR"
musicas_completas[4560, "Artista"] <- "Zeca Pagodinho"
musicas_completas[4560, "Nome"] <- "PAGO PRA VER"
musicas_completas[4548, "Artista"] <- "Madonna"
musicas_completas[4548, "Nome"] <- "AMERICAN LIFE"
musicas_completas[4449, "Artista"] <- "Iio"
musicas_completas[4449, "Nome"] <- "Rapture"
musicas_completas[4414, "Artista"] <- "Nickelback"
musicas_completas[4414, "Nome"] <- "HOW YOU REMIND ME"
musicas_completas[4478, "Artista"] <- "No Doubt"
musicas_completas[4478, "Nome"] <- "HEY BABY"
musicas_completas[5931, "Artista"] <- "luan Santana"
musicas_completas[4845, "Artista"] <- "O Rappa"
musicas_completas$Nome <- gsub("\\((Ao vivo|ao vivo|medley|acústico|remix|Live In London)\\)", "", musicas_completas$Nome)
musicas_completas$Nome <- gsub(" – Live", "", musicas_completas$Nome)
musicas_completas$Nome <- trimws(musicas_completas$Nome)

View(musicas_completas)
# Caso queira salvar o arquivo combinado
#write.csv(todos_dados, "todos_dados_combined.csv", row.names = FALSE)
write.csv(musicas_completas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas.csv", row.names = FALSE)


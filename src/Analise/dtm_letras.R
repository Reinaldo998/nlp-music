library(readr)
library(tidyverse)
library(tm)
library(Matrix)
library(textcat)
library(cld2)
library(franc)

###########################################################################################################################
############### Extração do conjunto de dados ###################
data_final <- read_csv("~/Reinaldo/data_final.csv")

######################## Ajustando os dados faltantes #####################

data_final[139,"Letra"]<-"Instrumental"
data_final[139,"Nome.y"]<-data_final$Nome.x[139]
data_final[139,"Artista.y"]<-data_final$Artista.x[139]


data_final[1079,"Letra"]<-"Kid games and nursery rhymes
I've been loving you a long time
And I just wanna let you know today, baby
That I still feel the same way

Shirley, Shirley I'll put you up high
I'm gonna kiss you and never make you cry
How could Alfred sit on a wall
I cannot get you if you fall

Do it Do it
*One potato, two potato, three potato, four
It's just impossible to love you anymore
Five potatoes, six potatoes, seven then eight
Oh your love makes me feel so great

Remember Shirley, the game we used to play
Fat Ava used to tease us everyday
And now that you're older, I hope you don't mind
And you can tease me any old time, oh yeah"
data_final[1079,"Nome.y"]<-data_final$Nome.x[1079]
data_final[1079,"Artista.y"]<-data_final$Artista.x[1079]

data_final[2363,"Letra"]<-"Look at what's happened to me
I can't believe it myself
Suddenly I'm up on top of the world
It should've been somebody else

Believe it or not
I'm walking on air
I never thought I could feel so free, eh, eh
Flying away on a wing and a prayer
Who could it be?
Believe it or not it's just me

Just like the light of a new day
It hit me from out of the blue
Breaking me out of the spell I was in
Making all of my wishes come true ue ue

Believe it or not
I'm walking on air
I never thought I could feel so free eh, eh
Flying away on a wing and a prayer
Who could it be?
Believe it or not it's just me

This is too good to be true
Look at me, falling for you

Believe it or not
Believe it or not
Believe it or not
Believe it or not

Believe it or not
I'm walking on air
I never thought I could feel so free eh, eh
Flying away on a wing and a prayer
Who could it be?
Believe it or not it's just me"
data_final[2363,"Nome.y"]<-"Believe It Or Not"
data_final[2363,"Artista.y"]<-"Joey Scarbury"

data_final[2579,"Letra"]<-"Ich schau dich an, du bist so wunderschön
Ich schau dich an, doch leider muß ich gehn
Ja ich möchte dich so gern berühren
Und dich in meinem Appartement verführen
Doch viel zu kurz ist das Minutenglück
Und ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück

Ich schau dich an, du bist so wunderschön
Ich schau dich an, ich muß dich wiedersehn
Jede Stunde möcht ich bei dir sein
Schon wieder werf ich eine Münze rein
Doch viel zu kurz ist das Minutenglück
Und ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück

Peep-peep, Show-ow-ow-ow
Peep-peep, Show-ow-ow-ow
Peep-peep, Show-ow-ow-ow
Peep! Peep!

Ich schau dich an, du bist so wunderschön
Ich schau dich an, ich muß dich wiedersehn
Jede Stunde möcht ich bei dir sein
Schon wieder werf ich eine Münze rein
Doch viel zu kurz ist das Minutenglück
Und ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück
Ja, ich muß wieder auf die Straße zurück

Peep-peep, Show-ow-ow-ow
Peep-peep, Show-ow-ow-ow
Peep-peep, Show-ow-ow-ow
Peep! Peep!"
data_final[2579,"Nome.y"]<-"Ich Schau Dich An"
data_final[2579,"Artista.y"]<-"Spider Murphy Gang"


#######################################################################################
data_final[201,"Letra"]<-"Instrumental"
data_final[1187,"Letra"]<-"Instrumental"
data_final[1225,"Letra"]<-"Instrumental"
data_final[1786,"Letra"]<-"Instrumental"
data_final[2427,"Letra"]<-"Instrumental"
data_final[3379,"Letra"]<-"Instrumental"


################### Transformando dados textuais no bag of words ###################
corp<-data_final$Letra %>% VectorSource()%>%VCorpus()

################### Usando pesos por frequência ##########
dtm<-corp %>%  DocumentTermMatrix(control=list(tolower=T,
                                               removePunctuation=T,
                                               removeNumbers=T,
                                               weighting=weightTf))

dtm

################### Usando pesos binário ##########
dtm2<-corp %>%  DocumentTermMatrix(control=list(tolower=T,
                                                removePunctuation=T,
                                                removeNumbers=T,
                                                weighting=weightBin))


qtde2<-rowSums(as.matrix(dtm2))
length(qtde2)


###################### Idiomas ###########################
################## Idiomas 1 ##############
textos <- sapply(corp, as.character)

idiomas <- cld2::detect_language(textos)
length(idiomas)

################## Idiomas 2 ##############
idio <- textcat(textos)

length(idio)

################## Idiomas 3 ##############
idiomas_franc <- sapply(textos, franc)

length(idiomas_franc)

################## Idiomas 4 ##############
devtools::install_github("marcellodesales/cld3")
idiomas_cld3 <- detect_language(textos)

########################################################################
############### Organizar a Matriz de dados ##################

Mqtdade<-cbind(data_final[1:4],qtde,qtde2,qtde-qtde2,idiomas,idio,idiomas_franc,idiomas_cld3)

colnames(Mqtdade)<-c("Nome","Artista","Ano","ID","N_palavras","N_palavras_bin","Dif","Idioma_1","Idioma_2","Idioma_3","Idioma_4")

View(Mqtdade)

#write.csv(Mqtdade,"~/Reinaldo/N_palavras_i.csv")

library(dplyr)
# 3. Definir o grupo que você quer filtrar
grupo_desejado <- 21# Substitua por qualquer nome de grupo que você tenha

# 4. Realizar o filtro
df_filtrado <- data_dp %>%
  filter(Grupos_MM_DP == grupo_desejado)

# 5. Visualizar o resultado
View(df_filtrado)

df_filtrado$ID
# Você também pode ver as dimensões do novo dataframe filtrado
cat("\nDimensões do DataFrame filtrado:", dim(df_filtrado), "\n")

######################################################################################
codigos_para_inspecionar <- c(6709,6757,6868,6901,7615,7647,7925, 8019,8126,8337,8414,8550,101,207,372,422,572,623, 1260,1301)
linhas_selecionadas <- df_final_para_analise_total %>%
  filter(Codigo %in% codigos_para_inspecionar)
View(linhas_selecionadas)
#===========================================================================================

library(dplyr) # Certifique-se que dplyr está carregado

# --- CRIAR UM DATAFRAME COM AS ATUALIZAÇÕES MANUAIS ---
# Crie este dataframe com as colunas Codigo e Nova_Letra
updates_manuais <- data.frame(
  Codigo = c(6709,6757,6868,6901,7615,7647,7925, 8019,8126,8337,8414,8550,101,207,372,422,572,623, 1260,1301), # Substitua pelos Códigos das músicas a serem atualizadas
  Nova_Letra = c(           # Substitua pelas Novas Letras correspondentes
    'Quando você se separou de mim
Quase que a minha vida teve fim
Sofri, chorei, tanto que nem sei
Tudo o que chorei por você, por você
Quando você se separou de mim
Eu pensei que ia até morrer
Depois lutei tanto pra esquecer
Tudo o que passei com você, com você, com você

E mesmo assim ainda eu não vou
Dizer que já te esqueci
Se alguém vier me perguntar
Nem mesmo sei o que vou falar

Eu posso até dizer
Ninguém te amou o tanto quanto eu te amei
Mas você não mereceu
O amor que eu te dei

Quando você se separou de mim
Quase que a minha vida teve fim
Agora eu nem quero lembrar
Que um dia eu te amei e sofri e chorei
Eu te amei e chorei

E mesmo assim ainda eu não vou
Dizer que já te esqueci
Se alguém vier me perguntar
Nem mesmo sei o que vou falar

Eu posso até dizer
Ninguém te amou o tanto quanto eu te amei
Mas você não mereceu
O amor que eu te dei

Quando você se separou de mim
Quase que a minha vida teve fim
Agora eu nem quero lembrar
Que um dia eu te amei e sofri e chorei

Por você eu chorei

Por você eu chorei

Eu sofri...',
    'Eu sou terrível e é bom parar
Que desse jeito me provocar
Você não sabe de onde eu venho
O que eu sou e o que tenho
Eu sou terrível, vou lhe dizer
Que ponho mesmo pra derreter
Estou com a razão no que digo
Não tenho medo nem do perigo
Minha caranga é máquina quente
Eu sou terrível, e é bom parar
Porque agora vou decolar
Não é preciso nem avião
Eu vôo mesmo aqui do chão

Eu sou terrível, vou lhe contar
Não vai ser mole me acompanhar
Garota que andar do meu lado
Vai ver que eu ando mesmo apressado
Minha caranga é máquina quente
Eu sou terrível, eu sou terrível

Eu sou terrível e é bom parar
De desse jeito me provocar
Você não sabe de onde eu venho
O que eu sou e o que tenho
Eu sou terrível, vou lhe dizer
Que ponho mesmo pra derreter

Estou com a razão no que digo
Não tenho medo nem do perigo
Minha caranga é máquina quente
Eu sou terrível, Eu sou terrível

Eu sou terrível', 'Eu sou terrível e é bom parar
Que desse jeito me provocar
Você não sabe de onde eu venho
O que eu sou e o que tenho
Eu sou terrível, vou lhe dizer
Que ponho mesmo pra derreter
Estou com a razão no que digo
Não tenho medo nem do perigo
Minha caranga é máquina quente
Eu sou terrível, e é bom parar
Porque agora vou decolar
Não é preciso nem avião
Eu vôo mesmo aqui do chão

Eu sou terrível, vou lhe contar
Não vai ser mole me acompanhar
Garota que andar do meu lado
Vai ver que eu ando mesmo apressado
Minha caranga é máquina quente
Eu sou terrível, eu sou terrível

Eu sou terrível e é bom parar
De desse jeito me provocar
Você não sabe de onde eu venho
O que eu sou e o que tenho
Eu sou terrível, vou lhe dizer
Que ponho mesmo pra derreter

Estou com a razão no que digo
Não tenho medo nem do perigo
Minha caranga é máquina quente
Eu sou terrível, Eu sou terrível

Eu sou terrível', '

Se você pretende saber quem eu sou
Eu posso lhe dizer
Entre no meu carro na Estrada de Santos
E você vai me conhecer
Você vai pensar que eu
Não gosto nem mesmo de mim
E que na minha idade
Só a velocidade anda junto a mim

Só ando sozinho e no meu caminho
O tempo é cada vez menor
Preciso de ajuda, por favor me acuda
Eu vivo muito só

Se acaso numa curva
Eu me lembro do meu mundo
Eu piso mais fundo, corrijo num segundo
Não posso parar

Eu prefiro as curvas da Estrada de Santos
Onde eu tento esquecer
Um amor que eu tive e vi pelo espelho
Na distância se perder

Mas se amor que eu perdi
Eu novamente encontrar, oh
As curvas se acabam e na Estrada de Santos
Não vou mais passar
Não, não vou mais passar, oh

Eu prefiro as curvas da Estrada de Santos
Onde eu tento esquecer
Um amor que eu tive e vi pelo espelho
Na distância se perder

Mas se amor que eu perdi
Eu novamente encontrar, oh, oh
As curvas se acabam e na Estrada de Santos
Eu não vou mais passar
Não, não, não, não, não, não

Na Estrada de Santos as curvas se acabam
E eu não vou mais passar
Não, não, não
Oh, na Estrada de Santos as curvas se acabam', 'Os botões da blusa
que você usava
E meio confusa
Desabotoava
Iam pouco a pouco
me deixando ver
No meio de tudo
Um pouco de você
Nos lençois macios
Amantes se dão
Travesseiros soltos
Roupas pelo chão
Braços que se abraçam
Bocas que murmuram
Palavras de amor
Enquanto se procuram
Chovia lá fora
E a capa pendurada
Assistia a tudo
E não dizia nada
E aquela blusa que você usava
Num canto qualquer
Tranquila esperava', 'Hei!
Vivo condenado a fazer
O que eu não quero
Então bem comportado
Às vezes eu me desespero
Se faço alguma coisa
Sempre alguém vem me dizer
Que isso ou aquilo
Não se deve fazer
Restam os meus botões
Já não sei mais
O que é certo...

E como vou saber
O que eu devo fazer
Que culpa tenho eu?
Me diga amigo meu
Será que tudo
O que eu gosto
É imoral, é ilegal
Ou engorda...

Hei!
Há muito me perdi
Entre mil filosofias
Virei homem calado
E até desconfiado
Procuro andar direito
E ter os pés no chão
Mas certas coisas
Sempre me chamam atenção
Cá com os meus botões
Bolas!
Eu não sou de ferro...

Paro prá pensar
Mas eu não posso mudar
Que culpa tenho eu?
Me diga amigo meu
Será que tudo que eu gosto
É imoral, é ilegal
Ou engorda
Eh! Eh!...

Hei!
Se eu conheço alguém
Num encontro casual
E tudo anda bem
Num bate-papo informal
Uma noite quente
Sugere desfrutar
No meu terraço a vista
De frente para o mar
A noite é uma criança
Delícias no café da manhã...

Então o que fazer
Já não quero mais saber
Se como alguma coisa
Que não devo comer
Será que tudo o que eu gosto
É imoral, é ilegal
Ou engorda
Hei! Hei!
Será que tudo o que eu gosto
É imoral, é ilegal
Ou engorda
Heeei!
Será que tudo o que eu gosto
É imoral...', 'Na paz do seu sorriso
Meus sonhos realizo
E te beijo feliz

E na ânsia mais louca
No céu da sua boca
Do alto as estrelas me dizem, meu bem
Que a vida é isso
Que eu vivo por isso
Que você me dá, me dá

Na paz do seu sorriso
Meus sonhos realizo
e te beijo feliz

E a beleza é nada
Se for comparada
Com tudo que eu vejo em você, meu bem
O amor é perfeito
Me amarro no jeito que você me dá, me dá

Tudo isso que você meu bem me dá
Tudo isso que você meu bem me dá
Tudo isso que você meu bem me dá
Tudo isso que você meu bem me dá

Na paz do teu sorriso
meus sonhos realizo
e te beijo feliz

E nós dois num abraço
Rolamos no espaço
Me perco no amor com você, meu bem
E perco o juízo
Pois o paraíso é o que você me dá, me dá

Tudo isso que você meu bem me dá
Tudo isso que você meu bem me dá
Tudo isso que você meu bem me dá', 'Eu sou aquele amante à moda antiga
Do tipo que ainda manda flores
Aquele que no peito ainda abriga
Recordações de seus grandes amores

Eu sou aquele amante apaixonado
Que curte a fantasia dos romances
Que fica olhando o céu de madrugada
Sonhando abraçado à namorada

Eu sou do tipo de certas coisas
Que já não são comuns nos nossos dias
As cartas de amor, o beijo na mão
Muitas manchas de batom daquele amasso no portão

Apesar de todo o progresso
Conceitos e padrões atuais
Sou do tipo que na verdade
Sofre por amor e ainda chora de saudade

Porque sou aquele amante à moda antiga
Do tipo que ainda manda flores
Apesar do velho tênis e da calça desbotada
Ainda chamo de querida a namorada

Eu sou aquele amante à moda antiga
Do tipo que ainda manda flores
Apesar do velho tênis e da calça desbotada
Ainda chamo de querida a namorada

Ainda chamo de querida a minha namorada
A minha namorada, a namorada
A minha namorada', 'Eu quero ser sua canção, eu quero ser seu tom
Me esfregar na sua boca, ser o seu batom
O sabonete que te alisa embaixo do chuveiro
A toalha que desliza no seu corpo inteiro
Eu quero ser seu travesseiro e ter a noite inteira
Pra te beijar durante o tempo que você dormir
Eu quero ser o sol que entra no seu quarto adentro
Te acordar devagarinho, te fazer sorrir

Quero estar na maciez do toque dos seus dedos
E entrar na intimidade desses seus segredos
Quero ser a coisa boa, liberada ou proibida
Tudo em sua vida

Eu quero que você me dê o que você quiser
Quero te dar tudo que um homem dá pra uma mulher
E além de todo esse carinho que você me faz
Fico imaginando coisas, quero sempre mais

Você é o doce que eu mais gosto
Meu café completo, a bebida preferida e o prato predileto
Eu como e bebo do melhor e não tenho hora certa
De manhã, de tarde, à noite, não faço dieta

Esse amor que alimenta minha fantasia
É meu sonho, minha festa, é minha alegria
A comida mais gostosa, o perfume e a bebida
Tudo em minha vida

Todo homem que sabe o que quer
Sabe dar e querer da mulher
O melhor e fazer desse amor
O que come, o que bebe, o que dá e recebe

Mas o homem que sabe o que quer
E se apaixona por uma mulher
Ele faz desse amor sua vida
A comida, a bebida, na justa medida

O homem que sabe o que quer
Sabe dar e querer da mulher
O melhor e fazer desse amor
O que come, o que bebe, o que dá e recebe

Mas o homem que sabe o que quer
Sabe dar e querer da mulher
O melhor e fazer desse amor
O que come, o que bebe, o que dá e recebe

Mas o homem que sabe o que quer
E se apaixona por uma mulher
Ele faz desse amor sua vida
A comida, a bebida, na justa medida','Nosso amor é demais,
E quando o amor se faz
Tudo é bem mais bonito.
Nele a gente se dá
Muito mais do que está
E o que não está escrito.

Quando a gente se abraça
Tanta coisa se passa
Que não dá para falar.
Nesse encontro perfeito
Entre o seu e o meu peito
Nossa roupa não dá.

Nosso amor é assim,
Pra você e pra mim,
Como manda a receita
Nossas curvas se acham
Nossas formas se encaixam
Na medida perfeita.

Este amor é pra nos
A loucura que traz
Esse sonho de paz
E é bonito demais,
Quando a gente se beija
Se ama e se esquece
Da vida lá fora

Cada parte de nos
Tem a forma ideal
Quando juntas estão,
Coincidência total
Do côncavo e convexo
Assim é nosso amor,
No sexo.

Este amor é pra nos
A loucura que traz
Esse sonho de paz
E é bonito demais,
Quando agente se beija
Se ama e se esquece
Da vida lá fora

Cada parte de nos
Tem a forma ideal
Quando juntas estão,
Coincidência total
Do côncavo e convexo
Assim é nosso amor,
No sexo.', 'Todo dia quando eu pego a estrada
Quase sempre é madrugada
E o meu amor aumenta mais
Porque eu penso nela no caminho
imagino seu carinho
E todo o bem que ela me faz

A saudade então aperta o peito
Ligo o rádio e dou um jeito
De espantar a solidão
Se é dia eu ando mais veloz
e à noite todos os faróis
Iluminando a escuridão

Eu sei
Tô correndo ao encontro dela
Coração tá disparado
Mas eu ando com cuidado
Não me arrisco na banguela
Eu sei
Todo dia nessa estrada
No volante eu penso nela
Já pintei no para-choque
Um coração e o nome dela

Já rodei o meu país inteiro
E como bom caminhoneiro
Peguei chuva e cerração
Quando chove o limpador desliza
vai e vem o para-brisa
Bate igual meu coração

Doido pelo doce do seu beijo
Olho cheio de desejo
Seu retrato no painel
É no acostamento dos seus braços
Que eu desligo meu cansaço
E me abasteço desse mel

Todo dia quando eu pego a estrada
Quase sempre é madrugada
E o meu amor aumenta mais
Olho o horizonte e vou em frente
tô com Deus e tô contente
O meu caminho eu sigo em paz

O nome dela o nome dela', 'Verde e amarelo, verde e amarelo
Boto fé, não me iludo
Nessa estrada ponho o pé, vou com tudo
Terra firme, livre
Tudo o que eu quis no meu país
Onde eu vou, vejo a raça
Forte no sorriso da massa
A força desse grito que diz: "É meu país"
Verde e amarelo
Sou daqui, sei da garra
De quem encara o peso da barra
Vestindo essa camisa feliz do meu país
Tudo bom, tudo belo
Tudo azul e branco, verde e amarelo
Toda a natureza condiz com o meu país
Verde e amarelo, verde e amarelo
Só quem leva no peito esse amor, esse jeito
Sabe bem o que é ser brasileiro
Sabe o que é:
Verde e amarelo, verde e amarelo
Verde e amarelo, verde e amarelo
Bom no pé, deita e rola
Ele é mesmo bom de samba e de bola
Que beleza de mulher que se vê no meu país
É Brasil, é brazuca
Esse cara bom de papo e de cuca
Tiro o meu chapéu, peço bis pro meu país
Verde e amarelo, verde e amarelo
Verde e amarelo
Boto fé, não me iludo
Nessa estrada ponho o pé, vou com tudo
Terra firme, tudo o que eu quis, é o meu país
É Brasil, é brazuca
Esse cara bom de papo e de cuca
Tiro o meu chapéu, peço bis pro meu país
Verde e amarelo, verde e amarelo
Verde e amarelo é a camisa que eu visto
Verde e amarelo azul e branco também
Verde e amarelo é Brasil, é brazuca
Verde e amarelo, verde e amarelo
Verde e amarelo boto fé, não me iludo
Verde e amarelo
Nessa estrada ponho pé, vou com tudo', 'Quem De Nós Dois (La Mia Storia Tra Le Dita)
Ana Carolina

Eu e você
Não é assim tão complicado
Não é difícil perceber
Quem de nós dois
Vai dizer que é impossível
O amor acontecer

Se eu disser que já nem sinto nada
Que a estrada sem você é mais segura
Eu sei você vai rir da minha cara
Eu já conheço o teu sorriso, leio o teu olhar
Teu sorriso é só disfarce
Que eu já nem preciso

Sinto dizer
Que amo mesmo, tá ruim pra disfarçar
Entre nós dois
Não cabe mais nenhum segredo
Além do que já combinamos

No vão das coisas que a gente disse
Não cabe mais sermos somente amigos
E quando eu falo que eu já nem quero
A frase fica pelo avesso
Meio na contra-mão
E quando finjo que esqueço
Eu não esqueci nada

E cada vez que eu fujo, eu me aproximo mais
E te perder de vista assim é ruim demais
E é por isso que atravesso o teu futuro
E faço das lembranças um lugar seguro

Não é que eu queira reviver nenhum passado
Nem revirar um sentimento revirado
Mas toda vez que eu procuro uma saída
Acabo entrando sem querer na sua vida

Eu procurei qualquer desculpa pra não te encarar
Pra não dizer de novo e sempre a mesma coisa
Falar só por falar
Que eu já não tô nem aí pra essa conversa
Que a história de nós dois não me interessa

Se eu tento esconder meias verdades
Você conhece o meu sorriso
Lê o meu olhar
Meu sorriso é só disfarce
Que eu já nem preciso

E cada vez que eu fujo, eu me aproximo mais
E te perder de vista assim é ruim demais
E é por isso que atravesso o teu futuro
E faço das lembranças um lugar seguro

Não é que eu queira reviver nenhum passado
Nem revirar um sentimento revirado
Mas toda vez que eu procuro uma saída
Acabo entrando sem querer na sua vida', 'Eu cheguei a deixar vestígios pra você me achar
Foi assim que entreguei meu coração devagar
Eu tentei te roubar aos poucos pra você notar
Que fui eu, te guardei onde ninguém vai tirar
No fundo dos meus olhos (olhos)
Pra dentro da memória te levei
Amor, você me tentou
Ô, Carla
Eu te amei como jamais
Um outro alguém vai te amar
Antes que o sol pudesse acordar
Eu te amei, ô, Carla
Ô, Carla
Eu cheguei a deixar vestígios pra você me achar
Foi assim que entreguei meu coração devagar
Eu tentei te roubar aos poucos pra você notar
Que fui eu, te guardei onde ninguém vai tirar
No fundo dos meus olhos (olhos)
Pra dentro da memória te levei
Amor, você me tentou
Ô, Carla
Eu te amei como jamais
Um outro alguém vai te amar
Antes que o sol pudesse acordar
Eu te amei, ô, Carla
No fundo dos meus olhos (olhos)
Pra dentro da memória te levei
Amor, você me tentou
Ô, Carla
Eu te amei como jamais
Um outro alguém vai te amar
Antes que o sol pudesse acordar
Eu te amei, ô, Carla
Eu te amei, ô, Carla
Eu te amei, ô, Carla', 'Pra que te espero de braços abertos
Se você caminha pra nunca chegar?
Então vou no fundo, ameaço ir embora
Você diz que prefere, quem sabe, ficar...
Eu queria tanto mudar sua vida
Mas você não sabe se vai ou se fica
Eu tenho coragem, já tô de saída
Você diz que é pouco, e pouco pra mim não é bobagem
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
Então me lanço, me atiro em frente ao seu carro
E aí você decide se é guerra ou perdão
Se na vida eu apanho, outras vezes eu bato
Mas trago a minha blusa aberta e uma rosa em botão
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
O tempo do passado tá em outro tempo
Lembrando de nós dois num instante que não pára
Viver é um livro de esquecimento
Eu só quero lembrar de você até perder a memória
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
E subo bem alto pra gritar que é amor
Eu vou de escada pra elevar a dor
E subo bem alto pra gritar que é amor... yeah!','Eu quero te roubar pra mim
Eu que não sei pedir nada
Meu caminho é meio perdido
Mas que perder seja o melhor destino
Agora não vou mais mudar
Minha procura por si só
Já era o que eu queria achar
Quando você chamar meu nome
Eu que também não sei aonde estou
Pra mim que tudo era saudade
Agora seja lá o que for
Eu só quero saber em qual rua
Minha vida vai encostar na tua
Eu só quero saber em qual rua
Minha vida vai encostar na tua
Eu quero te roubar pra mim
Eu que não sei pedir nada
Meu caminho é meio perdido
Mas que perder seja o melhor destino
Agora não vou mais mudar
Minha procura por si só
Já era o que eu queria achar
Quando você chamar meu nome
Eu que também não sei aonde estou
Pra mim que tudo era saudade
Agora seja lá o que for
Eu só quero saber em qual rua
Minha vida vai encostar na tua
Eu só quero saber em qual rua
Minha vida vai encostar na tua
E saiba que forte eu sei chegar
Mesmo se eu perder o rumo, êê
E saiba que forte eu sei chegar
Se for preciso eu sumo
Eu só quero saber em qual rua
Minha vida vai encostar na tua
Eu só quero saber em qual rua
Minha vida vai encostar na tua', 'Olho a cidade ao redor
E nada me interessa
Eu finjo ter calma
A solidão me apressa
Tantos caminhos sem fim
De onde você não vem
Meu coração na curva
Batendo a mais de cem
Eu vou sair nessas horas de confusão
Gritando seu nome entre os carros que vêm e vão
Quem sabe, então assim
Você repara em mim
Corro de te esperar
De nunca te esquecer
As estrelas me encontram
Antes de anoitecer
Olho a cidade ao redor
Eu nunca volto atrás
Já não escondo a pressa
Já me escondi demais
Eu vou contar pra todo mundo
Eu vou pichar sua rua
Vou bater na sua porta de noite
Completamente nua
Quem sabe, então assim
Você repara em mim','Amigo arrasta uma cadeira
Chega mais pra perto e fale o que quiser
Fale o que tiver vontade
De amor, de saudade, fale de mulher
Na minha vida existe uma
Que é a coisa mais linda que eu já conheci
Não sei se ainda assim tão jovem
Você tem história pra contar pra mim
Aí é que você se engana
Quando a gente ama o coração se aquece
E por amor se faz de tudo
E se faz muito mais quando a mulher merece
Amigo eu penso tanto nela
Mas ela também vive pensando em mim
O sol da minha vida é ela
Eu não pensei que alguém pudesse amar assim
Amigo eu também tenho história
Eu tenho alguém que um dia entrou no meu caminho
Suave como a flor do campo
Ela me deu amor, ela me deu carinho
Assim é a mulher que eu amo
Minha doce amada amiga e companheira
Pois cuide dela muito bem
Porque um grande amor é para a vida inteira
Se a conversa é boa o tempo logo passa
Quando se ama a gente não disfarça
E sempre fala da mulher amada
E a prosa tem mais graça
E nessa conversa vai passando a hora
Se alguém espera a gente não demora
E o coração no peito bate forte
E a gente vai embora
Amigo arrasta uma cadeira
Chega mais pra perto e fale o que quiser
Fale o que tiver vontade
De amor de saudade, fale de mulher
Eu só falei da minha amada
Que é tudo pra mim que é o sol da minha vida
E eu falei do meu amor
A flor que faz a minha estrada mais florida
Amigo a gente não se engana
Quando a gente ama o coração se aquece
E por amor se faz de tudo
E se faz muito mais quando a mulher merece
Se a conversa é boa o tempo logo passa
Quando se ama a gente não disfarça
E sempre fala da mulher amada
E a prosa tem mais graça
E nessa conversa vai passando a hora
Se alguém espera a gente não demora
E o coração no peito bate forte
E a gente vai embora
Se a conversa é boa o tempo logo passa
Quando se ama a gente não disfarça
E sempre fala da mulher amada
E a prosa tem mais graça
E nessa conversa vai passando a hora
Se alguém espera a gente não demora
E o coração no peito bate forte
E a gente vai embora','O cara que pensa em você toda hora
Que conta os segundos se você demora
Que está todo o tempo querendo te ver
Porque já não sabe ficar sem você
E no meio da noite te chama
Pra dizer que te ama
Esse cara sou eu
O cara que pega você pelo braço
Esbarra em quem for que interrompa seus passos
Está do seu lado pro que der e vier
O herói esperado por toda mulher
Por você ele encara o perigo
Seu melhor amigo
Esse cara sou eu
O cara que ama você do seu jeito
Que depois do amor você se deita em seu peito
Te acaricia os cabelos, te fala de amor
Te fala outras coisas, te causa calor
De manhã você acorda feliz
Num sorriso que diz
Esse cara sou eu
Esse cara sou eu
Eu sou o cara certo pra você
Que te faz feliz e que te adora
Que enxuga seu pranto quando você chora
Esse cara sou eu
Esse cara sou eu
O cara que sempre te espera sorrindo
Que abre a porta do carro quando você vem vindo
Te beija na boca, te abraça feliz
Apaixonado te olha e te diz
Que sentiu sua falta e reclama
Ele te ama
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu', 'O cara que pensa em você toda hora
Que conta os segundos se você demora
Que está todo o tempo querendo te ver
Porque já não sabe ficar sem você
E no meio da noite te chama
Pra dizer que te ama
Esse cara sou eu
O cara que pega você pelo braço
Esbarra em quem for que interrompa seus passos
Está do seu lado pro que der e vier
O herói esperado por toda mulher
Por você ele encara o perigo
Seu melhor amigo
Esse cara sou eu
O cara que ama você do seu jeito
Que depois do amor você se deita em seu peito
Te acaricia os cabelos, te fala de amor
Te fala outras coisas, te causa calor
De manhã você acorda feliz
Num sorriso que diz
Esse cara sou eu
Esse cara sou eu
Eu sou o cara certo pra você
Que te faz feliz e que te adora
Que enxuga seu pranto quando você chora
Esse cara sou eu
Esse cara sou eu
O cara que sempre te espera sorrindo
Que abre a porta do carro quando você vem vindo
Te beija na boca, te abraça feliz
Apaixonado te olha e te diz
Que sentiu sua falta e reclama
Ele te ama
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu
Esse cara sou eu'
  ),
  stringsAsFactors = FALSE
)

# Inspecionar as atualizações que serão aplicadas (opcional)
print("Atualizações manuais a serem aplicadas:")
View(updates_manuais)


# --- APLICAR AS ATUALIZAÇÕES USANDO rows_update ---
# rows_update é uma função poderosa que atualiza linhas no primeiro dataframe
# com base em correspondências no segundo dataframe (pela coluna 'by').
# As colunas do segundo dataframe que tiverem o mesmo nome que no primeiro serão atualizadas.
df_final_para_analise_total <- df_final_para_analise_total %>%
  rows_update(
    updates_manuais %>% rename(Letra_Site = Nova_Letra), # Renomeia 'Nova_Letra' para 'Letra_Site' para combinar
    by = "Codigo", # Coluna usada para fazer a correspondência (o ID da música)
    unmatched = "ignore" # Ignora se algum Codigo em updates_manuais não for encontrado
  )

# Inspecionar as músicas atualizadas (opcional)
print("Músicas atualizadas após rows_update:")
View(df_final_para_analise_total %>% filter(Codigo %in% updates_manuais$Codigo))

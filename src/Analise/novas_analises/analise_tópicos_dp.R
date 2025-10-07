
# Carregue todos eles no início do seu script:
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
# Seu código inicial, incluindo o cbind:

dtm_filtrada<- readRDS("/home/reinaldo/dtm_filtrada_tf_30_4.rds")
dim(dtm_filtrada)
# O caminho para o diretório dos arquivos de saída
#caminho<-"/home/reinaldo/dp_daiane"
caminho<-"/home/reinaldo/simulacao_antiga/dp_daiane"
# Lê o arquivo de texto, que cria um data.frame de uma coluna
#dp_daianeGrupos_MM_DP <- read.table(paste(caminho,"Grupos_MM_DP_3.txt",sep=""), quote="\"", comment.char="")
dp_daianeGrupos_MM_DP<- readRDS(file = paste0(caminho, "Grupos_MM_DP_3.rds"))
#dim(dp_daianeGrupos_MM_DP)
# Lê o arquivo de texto, que cria um data.frame com linhas e colunas invertidas
#dp_daianeGrupos_MM_DP_transposto <- read.table(paste(caminho,"Grupos_MM_DP_3.txt",sep=""), quote="\"", comment.char="")

# Transpõe o dataframe para a orientação correta
#dp_daianeGrupos_MM_DP <- as.data.frame(t(dp_daianeGrupos_MM_DP_transposto))

#print(dp_daianeGrupos_MM_DP[1:100,])
#View(dp_daianeGrupos_MM_DP)
length(dp_daianeGrupos_MM_DP)
#dim(dp_daianeGrupos_MM_DP)
## --- CORREÇÃO AQUI ---
# Converte o data.frame em um vetor numérico simples
#Grupos_MM_DP <- dp_daianeGrupos_MM_DP$V3
Grupos_MM_DP <- unlist(dp_daianeGrupos_MM_DP)
length(dp_daianeGrupos_MM_DP)
df_id_grupo <- data.frame(
  ID_musica = as.numeric(row.names(dados)), # <--- CORREÇÃO AQUI as.numeric dtm_filtrada
  Grupo_final = Grupos_MM_DP
)

# Opcional: Visualizar a estrutura e as primeiras linhas do novo dataframe
#str(df_id_grupo)
#head(df_id_grupo)
#dim(df_id_grupo)
#dim(dtm_filtrada)
#dim(data_final_pt)
#"6666" %in% df_id_grupo$ID_musica
# Combina os dados
#data_dp<-cbind(data_final_pt,Grupos_MM_DP)
data_dp <- data_final_pt %>%
  left_join(df_id_grupo, by = c("ID" = "ID_musica"))

# Opcional: Visualizar a estrutura do novo dataframe
#print("Novo dataframe 'data_dp' criado com sucesso.")
#View(data_dp)
#print(dim(data_dp))
#print(colnames(data_dp))
colnames(data_dp)<-c("Nome.x","Artista.x","Ano","ID" ,"Nome.y","Artista.y","Letra","Idioma","Grupos_MM_DP")

table(data_dp$Grupos_MM_DP)
# Agora o View(data_dp) e as outras operações devem funcionar
#View(data_dp)

saveRDS(data_dp, file = "data_mm_dp.rds")

View(data_dp %>% dplyr::filter(Grupos_MM_DP == 10))
#=========================================================================================================
# --- 1. Identificar os 10 tópicos com mais músicas ---
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(numero_de_musicas)) %>% # Ordena os tópicos pela contagem de músicas (maior para menor)
  head(10) %>% # Pega os 10 primeiros (os com mais músicas)
  pull(Grupos_MM_DP) # Extrai apenas os números dos tópicos

cat(paste0("--- Os 10 tópicos com mais músicas são: ", paste(top_10_topicos_por_musicas, collapse = ", "), " ---\n"))

# Inicializar uma lista vazia para armazenar os data.frames de artistas de cada um desses tópicos
lista_artistas_top_10_musicas <- list()

# --- 2. Iterar APENAS sobre os 10 tópicos selecionados ---
for (i in top_10_topicos_por_musicas) { # O loop agora itera sobre os tópicos que selecionamos!
  # Filtrar as músicas que pertencem ao tópico atual
  musicas_no_topico_dp <- data_dp %>%
    filter(Grupos_MM_DP == i)
  
  # Calcular os top 10 artistas para este tópico
  artistas_por_topico_corrente_dp <- musicas_no_topico_dp %>%
    group_by(Artista.y) %>% # Assumindo 'Artista.y' é a coluna do nome do artista
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do tópico (importante para o alinhamento)
  colnames(artistas_por_topico_corrente_dp)[colnames(artistas_por_topico_corrente_dp) == "Artista.y"] <- paste0("Tópico ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_top_10_musicas[[as.character(i)]] <- artistas_por_topico_corrente_dp # Usar as.character para nomes de lista
}

# --- 3. Consolidar em uma única tabela alinhada ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES NOS 10 TÓPICOS COM MAIS MÚSICAS (ALINHADA)\n")
cat("=============================================\n")

if (length(lista_artistas_top_10_musicas) > 0) {
  tabela_artistas_alinhada_top_musicas <- lista_artistas_top_10_musicas %>%
    reduce(full_join, by = "posicao") %>%
    arrange(posicao) %>%
    mutate_all(~replace_na(., "")) # Preencher NAs com strings vazias para uma exibição mais limpa
  
  View(tabela_artistas_alinhada_top_musicas)
  # Opcional: Visualizar em uma janela interativa no RStudio
  # View(tabela_artistas_alinhada_top_musicas)
} else {
  cat("Não foram encontrados tópicos suficientes ou dados para gerar a tabela alinhada.\n")
}

library(xtable)

xtable(tabela_artistas_alinhada_top_musicas)

#===========================================================================================
# Agora, continue com o restante do seu código para o gráfico:
# (O bloco que você me enviou para gerar 'dados_grafico_proporcao_dp')

# Recriando 'resumo_topicos_estudo' para garantir que usa os tipos corretos
resumo_topicos_estudo <- data_dp %>%
  group_by(Grupos_MM_DP) %>%
  summarise(
    numero_de_musicas = n(),
    media_do_ano = round(mean(Ano, na.rm = TRUE))
  ) %>%
  arrange(Grupos_MM_DP)

View(resumo_topicos_estudo)

top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(numero_de_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

# --- Preparar os dados para o gráfico de proporção por ano (APENAS os 10 tópicos mais frequentes) ---
# O resto deste bloco é o que já tínhamos, e as conversões serão mais suaves aqui.

dados_filtrados_para_grafico_dp <- data_dp %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

total_musicas_por_ano_dp <- data_dp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

musicas_topico_ano_dp_filtrado <- dados_filtrados_para_grafico_dp %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup()

dados_grafico_proporcao_dp <- musicas_topico_ano_dp_filtrado %>%
  left_join(total_musicas_por_ano_dp, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# O mutate que estava dando erro não será mais necessário aqui se as conversões anteriores funcionarem
# dados_grafico_proporcao_dp <- dados_grafico_proporcao_dp %>%
#   mutate(
#     Ano = as.numeric(Ano),
#     proporcao = as.numeric(proporcao),
#     Grupos_MM_DP = as.factor(Grupos_MM_DP)
#   ) # Esta parte agora será implícita ou feita no ggplot

# --- Gerar o gráfico ggplot2 base (linhas E pontos) ---

p_ggplot_dp_top10 <- ggplot(dados_grafico_proporcao_dp,
                            aes(x = Ano, y = proporcao,
                                color = factor(Grupos_MM_DP), # Aqui o factor deve funcionar
                                group = factor(Grupos_MM_DP))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proporção dos 10 Tópicos Mais Frequentes (Data_DP) ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(p_ggplot_dp_top10)
#=================================

library(plotly) # Se ainda não estiver carregado

# O objeto p_ggplot_dp_top10 já foi gerado e está na memória

cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 10 TÓPICOS MAIS FREQUENTES (DATA_DP) POR ANO (PLOTLY)\n")
cat("=============================================\n")

# Converter o objeto ggplot para um gráfico interativo plotly
# A estética 'text' no ggplot2 é automaticamente usada pelo ggplotly para o balão.
plotly_dp_top10_proporcao_final <- ggplotly(p_ggplot_dp_top10, tooltip = "text")

# Exibir o gráfico interativo
print(plotly_dp_top10_proporcao_final)

# Opcional: Salvar o gráfico interativo em HTML
# Certifique-se de que o pacote 'htmlwidgets' está instalado para salvar em HTML
# install.packages("htmlwidgets") # Se ainda não tiver
# library(htmlwidgets)

caminho_arquivo_html_dp <- "grafico_data_dp_top10_proporcao_interativo.html"
saveWidget(plotly_dp_top10_proporcao_final, file = caminho_arquivo_html_dp, selfcontained = TRUE)
cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html_dp, "'\n"))


# O objeto p_ggplot_dp_top10 já foi gerado e está na memória.
# As variáveis dados_grafico_proporcao_dp, top_10_topicos_por_musicas, etc.,
# também devem estar disponíveis na sua sessão.

# Recrie o ggplot2 para incluir a estética 'text'
p_final_plotly_dp_com_balao <- ggplot(dados_grafico_proporcao_dp,
                                      aes(x = Ano, y = proporcao,
                                          color = factor(Grupos_MM_DP),
                                          group = factor(Grupos_MM_DP),
                                          # --- ADICIONADO: Informações para o balão interativo (tooltip) ---
                                          text = paste("Tópico: ", Grupos_MM_DP,
                                                       "<br>Ano: ", Ano,
                                                       "<br>Proporção: ", scales::percent(proporcao)))) + # Formata como porcentagem
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proporção dos 10 Tópicos Mais Frequentes (Data_DP) ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Converter o objeto ggplot para um gráfico interativo plotly
# O argumento 'tooltip = "text"' faz o Plotly usar o conteúdo definido na estética 'text'.
plotly_dp_top10_proporcao_final_com_balao <- ggplotly(p_final_plotly_dp_com_balao, tooltip = "text")

# Exibir o gráfico interativo
print(plotly_dp_top10_proporcao_final_com_balao)

# Opcional: Salvar o gráfico interativo em HTML
# library(htmlwidgets) # Carregue se for salvar
caminho_arquivo_html_dp_balao <- "grafico_data_dp_top10_proporcao_interativo.html"
saveWidget(plotly_dp_top10_proporcao_final_com_balao, file = caminho_arquivo_html_dp_balao, selfcontained = TRUE)
# cat(paste0("\nGráfico interativo salvo com balão como: '", caminho_arquivo_html_dp_balao, "'\n"))

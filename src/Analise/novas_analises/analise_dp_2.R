# Define o caminho para a pasta onde o arquivo foi salvo
caminho <- "/home/reinaldo/Reinaldo"

# Lê os grupos do arquivo de texto para um vetor no R
grupos_finais <- scan(file = paste(caminho, "Grupos_MM_DP_terminal_novo.txt", sep="/"))

# Exibe os primeiros 10 grupos para verificar
head(grupos_finais, 10)

# Mostra o tamanho de cada grupo
table(grupos_finais)


#===================================================================
# Instalação e Carregamento de Pacotes
#===================================================================
# Instala os pacotes necessários caso não estejam instalados
required_packages <- c("dplyr", "ggplot2", "scales", "plotly", "htmlwidgets", "tidyr", "purrr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Carrega todos os pacotes
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
library(tidyr) # Necessário para replace_na
library(purrr) # Necessário para reduce (usado no full_join)

#
# Caminho para a pasta onde o SCRIPT (DP.R) e os arquivos de saída foram salvos
caminho_saida <- "/home/reinaldo/Reinaldo"
# Caminho para a pasta onde os DADOS originais estão
caminho_dados <- "/home/reinaldo"

# 1. Lendo os grupos consolidados do arquivo de saída do terminal
Grupos_MM_DP <- scan(file = paste(caminho_saida, "Grupos_MM_DP_terminal_novo.txt", sep="/"))

# 2. Lendo o data frame original com as informações das músicas
# (Este é o arquivo que você salvou como "data_final_pt_original.csv")
dados_musicas_orig <- read.csv(paste(caminho_dados, "data_final_pt_original.csv", sep="/"))

#
#===================================================================
# Preparação dos Dados para Análise
#===================================================================
# Combina os dados originais com os grupos resultantes do agrupamento
data_agrupado <- cbind(dados_musicas_orig, Grupos_MM_DP) %>%
  mutate(
    # Força para caractere e depois numérico para garantir o tipo correto
    Ano = as.numeric(as.character(Ano)),
    Grupos_MM_DP = as.numeric(as.character(Grupos_MM_DP))
  )

# Cria um resumo dos tópicos para identificar os mais frequentes
resumo_topicos_estudo <- data_agrupado %>%
  group_by(Grupos_MM_DP) %>%
  summarise(
    numero_de_musicas = n(),
    media_do_ano = round(mean(Ano, na.rm = TRUE))
  ) %>%
  arrange(desc(numero_de_musicas)) # Ordena para facilitar a identificação dos top tópicos

View(resumo_topicos_estudo)
#
#===================================================================
# 1. Identificar os 10 tópicos com mais músicas e seus artistas
#===================================================================
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES NOS 10 TÓPICOS COM MAIS MÚSICAS\n")
cat("=============================================\n")

# Identifica os 10 tópicos com o maior número de músicas
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  head(10) %>% # Pega os 10 primeiros (os com mais músicas)
  pull(Grupos_MM_DP) # Extrai apenas os números dos tópicos

cat(paste0("--- Os 10 tópicos com mais músicas são: ", paste(top_10_topicos_por_musicas, collapse = ", "), " ---\n"))

# Inicializa uma lista vazia para armazenar os data.frames de artistas de cada um desses tópicos
lista_artistas_top_10_musicas <- list()

# Itera APENAS sobre os 10 tópicos selecionados
for (i in top_10_topicos_por_musicas) {
  # Filtrar as músicas que pertencem ao tópico atual
  musicas_no_topico_atual <- data_agrupado %>%
    filter(Grupos_MM_DP == i)
  
  # Calcular os top 10 artistas para este tópico
  artistas_por_topico_corrente <- musicas_no_topico_atual %>%
    group_by(Artista.y) %>% # Assumindo 'Artista.y' é a coluna do nome do artista
    summarise(contagem = n(), .groups = 'drop') %>% # .groups = 'drop' para evitar mensagens de agrupamento
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do tópico (importante para o alinhamento)
  colnames(artistas_por_topico_corrente)[colnames(artistas_por_topico_corrente) == "Artista.y"] <- paste0("Tópico ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_top_10_musicas[[as.character(i)]] <- artistas_por_topico_corrente # Usar as.character para nomes de lista
}

# Consolidar em uma única tabela alinhada
if (length(lista_artistas_top_10_musicas) > 0) {
  tabela_artistas_alinhada_top_musicas <- lista_artistas_top_10_musicas %>%
    purrr::reduce(full_join, by = "posicao") %>% # Usa purrr::reduce para combinar múltiplos data.frames
    arrange(posicao) %>%
    mutate_all(~tidyr::replace_na(., "")) # Preencher NAs com strings vazias para uma exibição mais limpa
  
  # Imprimir a tabela no console
  print(tabela_artistas_alinhada_top_musicas)
  
  # Opcional: Visualizar em uma janela interativa no RStudio (descomente se estiver no RStudio)
  # View(tabela_artistas_alinhada_top_musicas)
} else {
  cat("Não foram encontrados tópicos suficientes ou dados para gerar a tabela alinhada.\n")
}

View(tabela_artistas_alinhada_top_musicas)

#================================================================================

#===================================================================
#
#===================================================================
# Análise: Palavras Mais Frequentes nos 10 Tópicos com Mais Músicas
#===================================================================
#===================================================================
# Análise: Palavras Mais Frequentes nos 10 Tópicos com Mais Músicas
#===================================================================
cat("\n\n=============================================\n")
cat(" PALAVRAS MAIS FREQUENTES NOS 10 TÓPICOS COM MAIS MÚSICAS\n")
cat("=============================================\n")

# Inicializa uma lista vazia para armazenar os data.frames de palavras de cada tópico
lista_palavras_top_10_musicas <- list()

# Verifica se a matriz y_ap tem nomes de colunas (vocabulário)
if (is.null(colnames(y_ap)) || length(colnames(y_ap)) == 0) {
  cat("\nERRO: A matriz de documentos (y_ap) não possui nomes de colunas (vocabulário).\n")
  cat("Verifique se o arquivo 'vocab_dtm_filtrada.csv' foi lido corretamente e não está vazio.\n")
  cat("A análise das palavras mais frequentes será pulada.\n")
  # Garante que a lista permaneça vazia para que o bloco de consolidação não falhe
  lista_palavras_top_10_musicas <- list()
} else {
  # Itera sobre cada um dos 10 tópicos mais frequentes
  for (i in top_10_topicos_por_musicas) {
    # Obtém os índices das músicas que pertencem ao tópico atual
    indices_musicas_no_topico <- which(Grupos_MM_DP == i)
    
    # Filtra a DTM (matriz y_ap) para incluir apenas as músicas deste tópico.
    # Usar drop = FALSE garante que o resultado seja sempre uma matriz, mesmo para uma única linha.
    dtm_topico_atual <- y_ap[indices_musicas_no_topico, , drop = FALSE]
    
    # Soma as contagens de palavras para o tópico atual (soma por coluna na DTM filtrada)
    palavras_contagem <- colSums(dtm_topico_atual)
    
    # Converte as contagens de palavras para um data frame para facilitar a manipulação
    palavras_df <- data.frame(
      palavra = colnames(dtm_topico_atual), # Usa os nomes de coluna da DTM filtrada
      contagem = as.numeric(palavras_contagem)
    ) %>%
      # Remove palavras com contagem zero, se houver, para focar nas relevantes
      filter(contagem > 0)
    
    # Obtém as top 10 palavras para este tópico, ordenando pela contagem
    top_palavras_por_topico_corrente <- palavras_df %>%
      arrange(desc(contagem)) %>% # Ordena em ordem decrescente de contagem
      head(10) %>% # Pega as 10 primeiras (as mais frequentes)
      mutate(posicao = row_number()) %>% # Adiciona uma coluna de posição (1º, 2º, etc.)
      select(posicao, palavra) # Seleciona apenas a posição e a palavra
    
    # Renomeia a coluna da palavra para o nome do tópico (ex: "Tópico 1", "Tópico 5")
    # Isso é importante para que a tabela final fique bem alinhada e fácil de ler.
    colnames(top_palavras_por_topico_corrente)[colnames(top_palavras_por_topico_corrente) == "palavra"] <- paste0("Tópico ", i)
    
    # Armazena este data frame na lista, usando o ID do tópico como nome da entrada da lista
    lista_palavras_top_10_musicas[[as.character(i)]] <- top_palavras_por_topico_corrente
  }
}

# Consolidar todos os data frames de palavras em uma única tabela alinhada
if (length(lista_palavras_top_10_musicas) > 0) {
  tabela_palavras_alinhada_top_musicas <- lista_palavras_top_10_musicas %>%
    purrr::reduce(full_join, by = "posicao") %>% # Combina todos os data frames da lista pela coluna 'posicao'
    arrange(posicao) %>% # Garante que a tabela esteja ordenada pela posição (1 a 10)
    mutate_all(~tidyr::replace_na(., "")) # Preenche quaisquer NAs (se um tópico tiver menos de 10 palavras, por exemplo) com strings vazias para uma exibição mais limpa
  
  # Imprime a tabela resultante no console
  print(tabela_palavras_alinhada_top_musicas)
} else {
  cat("Não foram encontrados tópicos suficientes ou dados para gerar a tabela alinhada de palavras.\n")
}
#=======================
# 2. Gráfico da Proporção dos 10 Tópicos Mais Frequentes ao Longo dos Anos
#===================================================================
cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 10 TÓPICOS MAIS FREQUENTES POR ANO\n")
cat("=============================================\n")

# Preparar os dados para o gráfico de proporção por ano (APENAS os 10 tópicos mais frequentes)
dados_filtrados_para_grafico <- data_agrupado %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

# Calcular o total de músicas por ano
total_musicas_por_ano <- data_agrupado %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = 'drop')

# Calcular a contagem de músicas por tópico e ano, e juntar com o total por ano
dados_grafico_proporcao <- dados_filtrados_para_grafico %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = 'drop') %>%
  left_join(total_musicas_por_ano, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# Gerar o gráfico ggplot2 base (linhas e pontos)
p_final_plotly_com_balao <- ggplot(dados_grafico_proporcao,
                                   aes(x = Ano, y = proporcao,
                                       color = factor(Grupos_MM_DP), # Converte Grupo para fator para cores
                                       group = factor(Grupos_MM_DP), # Agrupa por tópico para as linhas
                                       # Informações para o balão interativo (tooltip)
                                       text = paste("Tópico: ", Grupos_MM_DP,
                                                    "<br>Ano: ", Ano,
                                                    "<br>Proporção: ", scales::percent(proporcao)))) + # Formata como porcentagem
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proporção dos 10 Tópicos Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico"
  ) +
  scale_y_continuous(labels = scales::percent) + # Formata o eixo Y como porcentagem
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centraliza e bolda o título
    legend.position = "right" # Posição da legenda
  )

# Converter o objeto ggplot para um gráfico interativo plotly
# O argumento 'tooltip = "text"' faz o Plotly usar o conteúdo definido na estética 'text'.
plotly_final <- ggplotly(p_final_plotly_com_balao, tooltip = "text")

# Exibir o gráfico interativo (abrirá no Viewer do RStudio ou em uma nova janela)
print(plotly_final)

# Salvar o gráfico interativo em HTML
# O arquivo será salvo na pasta definida por 'caminho_saida'
saveWidget(plotly_final, file = paste0(caminho_saida, "/grafico_terminal_novo.html"), selfcontained = TRUE)
cat(paste0("\nGráfico interativo salvo como: '", caminho_saida, "/grafico_terminal_novo.html'\n"))


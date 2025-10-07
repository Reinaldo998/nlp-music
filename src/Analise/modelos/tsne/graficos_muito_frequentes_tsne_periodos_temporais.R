# ===================================
# CRIAÇÃO DE GRÁFICO t-SNE POR GRUPOS TEMPORAIS
# (Para matriz filtrada com palavras >20 ocorrências)
# ===================================

# ===================================
# 1. FUNÇÃO PARA CATEGORIZAR ANOS EM GRUPOS
# ===================================

categorizar_ano_grupo <- function(ano) {
  ifelse(ano >= 1958 & ano <= 1967, "[1958-1967]",
         ifelse(ano >= 1968 & ano <= 1985, "[1968-1985]",
                ifelse(ano >= 1986 & ano <= 1992, "[1986-1992]",
                       ifelse(ano >= 1993 & ano <= 1997, "[1993-1997]",
                              ifelse(ano >= 1998 & ano <= 2007, "[1998-2007]",
                                     ifelse(ano >= 2008 & ano <= 2014, "[2008-2014]",
                                            ifelse(ano > 2014, "[2015-Atual]",
                                                   NA)))))))
}

# ===================================
# 2. CRIAR DATAFRAMES COM COORDENADAS t-SNE E GRUPOS TEMPORAIS
# ===================================

# Para t-SNE baseado em TF (palavras muito frequentes)
df_tsne_tf_grupos <- data.frame(
  TSNE1 = tsne_muito_frequente$Y[, 1],
  TSNE2 = tsne_muito_frequente$Y[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf
)

# Adicionar grupos temporais
df_tsne_tf_grupos$GrupoAno <- as.factor(sapply(df_tsne_tf_grupos$Ano, categorizar_ano_grupo))

# Para t-SNE baseado em TF-IDF (palavras muito frequentes)
df_tsne_tfidf_grupos <- data.frame(
  TSNE1 = tsne_tfidf$Y[, 1],
  TSNE2 = tsne_tfidf$Y[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf
)

# Adicionar grupos temporais
df_tsne_tfidf_grupos$GrupoAno <- as.factor(sapply(df_tsne_tfidf_grupos$Ano, categorizar_ano_grupo))

# ===================================
# 3. CONFIGURAÇÃO DE CORES PARA OS GRUPOS
# ===================================

# Lista ordenada dos grupos
niveis_grupo_ano <- c(
  "[1958-1967]", "[1968-1985]", "[1986-1992]", "[1993-1997]",
  "[1998-2007]", "[2008-2014]", "[2015-Atual]"
)

# Paleta de cores bem distintas para cada grupo
cores_grupos <- c(
  "[1958-1967]" = "#FF0000",  # Vermelho Puro - Bossa Nova
  "[1968-1985]" = "#0000FF",  # Azul Puro - Tropicália/MPB
  "[1986-1992]" = "#00FF00",  # Verde Puro - Rock Nacional
  "[1993-1997]" = "#FFFF00",  # Amarelo Puro - Axé/Pagode
  "[1998-2007]" = "#FF00FF",  # Magenta Puro - Pop Rock
  "[2008-2014]" = "#00FFFF",  # Ciano Puro - Era Digital
  "[2015-Atual]" = "#666666"  # Cinza (alto contraste) - Contemporâneo
)
# ===================================
# 4. FUNÇÃO PARA CRIAR GRÁFICO INTERATIVO POR GRUPOS TEMPORAIS
# ===================================

criar_grafico_tsne_grupos_temporais <- function(df_tsne, titulo_grafico) {
  
  # Verificar se os dados existem
  if (is.null(df_tsne) || nrow(df_tsne) == 0) {
    stop("DataFrame está vazio ou nulo")
  }
  
  # Criar texto para hover
  hover_text <- paste(
    "Música:", df_tsne$Musica,
    "<br>Artista:", ifelse(is.na(df_tsne$Artista), "N/A", df_tsne$Artista),
    "<br>Ano:", ifelse(is.na(df_tsne$Ano), "N/A", df_tsne$Ano),
    "<br>Período:", ifelse(is.na(df_tsne$GrupoAno), "N/A", as.character(df_tsne$GrupoAno)),
    "<br>Total de palavras:", ifelse(is.na(df_tsne$NumPalavras), "N/A", df_tsne$NumPalavras)
  )
  
  # Criar gráfico plotly
  grafico <- plot_ly(
    data = df_tsne,
    x = ~TSNE1,
    y = ~TSNE2,
    color = ~GrupoAno,
    colors = cores_grupos,
    text = hover_text,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 12,
      opacity = 0.8,
      line = list(width = 1.5, color = "white")
    ),
    hovertemplate = paste(
      "%{text}",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = titulo_grafico,
        font = list(size = 18, color = "#2c3e50"),
        x = 0.5
      ),
      xaxis = list(
        title = "t-SNE Componente 1",
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "t-SNE Componente 2", 
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "closest",
      font = list(family = "Arial, sans-serif"),
      legend = list(
        title = list(text = "Períodos Musicais", font = list(size = 14)),
        font = list(size = 12),
        orientation = "v",
        x = 1.02,
        y = 0.5,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "#CCCCCC",
        borderwidth = 1
      )
    )
  
  return(grafico)
}

# ===================================
# 5. GERAR GRÁFICOS POR GRUPOS TEMPORAIS
# ===================================

cat("=== GERANDO GRÁFICOS t-SNE POR GRUPOS TEMPORAIS ===\n")

# Gráfico TF por grupos temporais
grafico_tf_grupos_temporais <- criar_grafico_tsne_grupos_temporais(
  df_tsne = df_tsne_tf_grupos,
  titulo_grafico = "t-SNE (TF): Músicas Agrupadas por Períodos Temporais<br><sub>Baseado em palavras com >20 ocorrências</sub>"
)

# Gráfico TF-IDF por grupos temporais
grafico_tfidf_grupos_temporais <- criar_grafico_tsne_grupos_temporais(
  df_tsne = df_tsne_tfidf_grupos,
  titulo_grafico = "t-SNE (TF-IDF): Músicas Agrupadas por Períodos Temporais<br><sub>Baseado em palavras com >20 ocorrências</sub>"
)

# ===================================
# 6. EXIBIR GRÁFICOS E LEGENDA
# ===================================

cat("\n=== VISUALIZAÇÃO t-SNE POR PERÍODOS TEMPORAIS ===\n")
cat("Cada cor representa um período específico da música brasileira:\n\n")
cat("🔴 ", names(cores_grupos)[1], " - Bossa Nova e início da MPB\n")
cat("🔵 ", names(cores_grupos)[2], " - Tropicália, Rock Nacional, Música de Protesto\n")
cat("🔵 ", names(cores_grupos)[3], " - Rock dos anos 80/90, Pop Nacional\n")
cat("🟢 ", names(cores_grupos)[4], " - Axé Music, Pagode, Rock Alternativo\n")
cat("🟡 ", names(cores_grupos)[5], " - Pop Rock, Sertanejo Universitário\n")
cat("🟣 ", names(cores_grupos)[6], " - Era Digital, Funk Carioca, Sertanejo\n")
cat("🟠 ", names(cores_grupos)[7], " - Trap, Funk Melody, Pop Contemporâneo\n\n")

# Exibir gráficos
print(grafico_tf_grupos_temporais)
print(grafico_tfidf_grupos_temporais)

cat("Gráficos t-SNE por grupos temporais gerados com sucesso!\n\n")

# ===================================
# 7. SALVAR GRÁFICOS (OPCIONAL)
# ===================================

# Criar diretório se não existir
if (!dir.exists("graficos_tsne_grupos")) {
  dir.create("graficos_tsne_grupos")
}

# Salvar gráfico TF
arquivo_tf_grupos <- "graficos_tsne_grupos/tsne_tf_grupos_temporais_filtrado.html"
saveWidget(
  widget = grafico_tf_grupos_temporais,
  file = arquivo_tf_grupos,
  selfcontained = TRUE,
  title = "t-SNE TF - Grupos Temporais"
)

cat("Gráfico t-SNE TF (grupos temporais) salvo em:", arquivo_tf_grupos, "\n")

# Salvar gráfico TF-IDF
arquivo_tfidf_grupos <- "graficos_tsne_grupos/tsne_tfidf_grupos_temporais_filtrados.html"
saveWidget(
  widget = grafico_tfidf_grupos_temporais,
  file = arquivo_tfidf_grupos,
  selfcontained = TRUE,
  title = "t-SNE TF-IDF - Grupos Temporais"
)

cat("Gráfico t-SNE TF-IDF (grupos temporais) salvo em:", arquivo_tfidf_grupos, "\n")

# ===================================
# 8. ESTATÍSTICAS DOS GRUPOS
# ===================================

cat("\n=== ESTATÍSTICAS DOS GRUPOS TEMPORAIS ===\n")

# Para TF
cat("Distribuição por grupos (TF):\n")
print(table(df_tsne_tf_grupos$GrupoAno, useNA = "ifany"))

# Para TF-IDF
cat("\nDistribuição por grupos (TF-IDF):\n")
print(table(df_tsne_tfidf_grupos$GrupoAno, useNA = "ifany"))

cat("\n=== PALETA DE CORES UTILIZADA ===\n")
for (i in 1:length(cores_grupos)) {
  cat("Grupo:", names(cores_grupos)[i], "- Cor:", cores_grupos[i], "\n")
}

cat("\nAnálise t-SNE por grupos temporais concluída!\n")
cat("Os gráficos permitem visualizar como músicas de diferentes épocas se agrupam\n")
cat("baseado na similaridade do conteúdo textual das letras.\n")
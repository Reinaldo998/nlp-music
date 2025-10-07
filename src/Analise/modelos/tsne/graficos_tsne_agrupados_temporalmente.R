# ===================================
# CRIAÇÃO DA NOVA VARIÁVEL DE GRUPOS DE ANOS
# ===================================

# Função para categorizar anos em grupos
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

# Aplica a categorização aos dataframes tsne_tf e tsne_tfidf
if (exists("tsne_tf") && !is.null(tsne_tf$Ano)) {
  tsne_tf$GrupoAno <- as.factor(sapply(tsne_tf$Ano, categorizar_ano_grupo))
} else {
  warning("tsne_tf ou a coluna 'Ano' em tsne_tf não está disponível.")
}

if (exists("tsne_tfidf") && !is.null(tsne_tfidf$Ano)) {
  tsne_tfidf$GrupoAno <- as.factor(sapply(tsne_tfidf$Ano, categorizar_ano_grupo))
} else {
  warning("tsne_tfidf ou a coluna 'Ano' em tsne_tfidf não está disponível.")
}

# ===================================
# CONFIGURAÇÃO DE CORES PARA OS GRUPOS
# ===================================

# Lista ordenada dos grupos
niveis_grupo_ano <- c(
  "[1958-1967]", "[1968-1985]", "[1986-1992]", "[1993-1997]",
  "[1998-2007]", "[2008-2014]", "[2015-Atual]"
)

# Paleta de cores bem distintas para cada grupo
cores_grupos <- c(
  "[1958-1967]" = "#FF6B6B",  # Vermelho coral
  "[1968-1985]" = "#4ECDC4",  # Turquesa
  "[1986-1992]" = "#45B7D1",  # Azul claro
  "[1993-1997]" = "#96CEB4",  # Verde menta
  "[1998-2007]" = "#FFEAA7",  # Amarelo suave
  "[2008-2014]" = "#DDA0DD",  # Roxo claro
  "[2015-Atual]" = "#FFB347"  # Laranja pêssego
)

# ===================================
# FUNÇÃO CORRIGIDA PARA GRÁFICO T-SNE INTERATIVO
# ===================================

criar_grafico_tsne_plotly_agrupado <- function(tsne_data_frame, titulo_grafico, color_by = "GrupoAno") {
  
  # Verificar se os dados existem
  if (is.null(tsne_data_frame) || nrow(tsne_data_frame) == 0) {
    stop("DataFrame está vazio ou nulo")
  }
  
  # Define o hover text
  hover_text <- paste(
    "Música:", tsne_data_frame$Musica,
    "<br>Artista:", ifelse(is.null(tsne_data_frame$Artista) | is.na(tsne_data_frame$Artista), "N/A", tsne_data_frame$Artista),
    "<br>Total de palavras:", tsne_data_frame$NumPalavras,
    "<br>Ano:", ifelse(is.null(tsne_data_frame$Ano) | is.na(tsne_data_frame$Ano), "N/A", tsne_data_frame$Ano),
    "<br>Grupo de Anos:", ifelse(is.null(tsne_data_frame$GrupoAno) | is.na(tsne_data_frame$GrupoAno), "N/A", as.character(tsne_data_frame$GrupoAno))
  )
  
  # Criar gráfico APENAS com coloração por grupo de anos
  if (color_by == "GrupoAno") {
    # Garantir que cada ponto tenha a cor do seu grupo
    grafico <- plot_ly(
      tsne_data_frame,
      x = ~Dim1, 
      y = ~Dim2,
      color = ~GrupoAno,  # Colorir por grupo de anos
      colors = cores_grupos,  # Paleta de cores definida
      text = hover_text,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 10,  # Pontos um pouco maiores para melhor visualização
        opacity = 0.85,
        line = list(width = 1.5, color = "white")  # Borda branca mais visível
      ),
      hoverinfo = 'text'
    )
  } else {
    stop("Esta função agora é especializada apenas para coloração por GrupoAno")
  }
  
  # Configurar layout
  grafico <- grafico %>% 
    layout(
      title = list(
        text = titulo_grafico,
        font = list(size = 16, color = "black")
      ),
      xaxis = list(
        title = "Dimensão 1",
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#f0f0f0"
      ),
      yaxis = list(
        title = "Dimensão 2", 
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#f0f0f0"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "closest",
      legend = list(
        title = list(text = "Períodos Temporais"),
        font = list(size = 12),
        orientation = "v",  # Legenda vertical
        x = 1.02,  # Posição à direita do gráfico
        y = 0.5    # Centralizada verticalmente
      )
    )
  
  return(grafico)
}

# ===================================
# GERAÇÃO DOS GRÁFICOS - APENAS POR GRUPO DE ANOS
# ===================================

# Gráfico TF colorido APENAS por Grupo de Anos
grafico_tf_grupo_anos <- criar_grafico_tsne_plotly_agrupado(
  tsne_data_frame = tsne_tf,
  titulo_grafico = "t-SNE TF — Agrupado por Períodos Temporais",
  color_by = "GrupoAno"
)

# Gráfico TF-IDF colorido APENAS por Grupo de Anos
grafico_tfidf_grupo_anos <- criar_grafico_tsne_plotly_agrupado(
  tsne_data_frame = tsne_tfidf,
  titulo_grafico = "t-SNE TF-IDF — Agrupado por Períodos Temporais",
  color_by = "GrupoAno"
)

# ===================================
# EXIBIÇÃO DOS GRÁFICOS
# ===================================
cat("\n=== Visualização t-SNE por Grupos Temporais ===\n")
cat("Cada cor representa um período específico da música brasileira:\n")
cat("🔴 Vermelho: [1958-1967] - Bossa Nova e início da MPB\n")
cat("🔵 Turquesa: [1968-1985] - Tropicália, Rock Nacional, Música de Protesto\n")
cat("🔵 Azul: [1986-1992] - Rock dos anos 80/90, Pop Nacional\n")
cat("🟢 Verde: [1993-1997] - Axé Music, Pagode, Rock Alternativo\n")
cat("🟡 Amarelo: [1998-2007] - Pop Rock, Sertanejo Universitário\n")
cat("🟣 Roxo: [2008-2014] - Era Digital, Funk Carioca, Sertanejo\n")
cat("🟠 Laranja: [2015-Atual] - Trap, Funk Melody, Pop Contemporâneo\n\n")

print(grafico_tf_grupo_anos)
print(grafico_tfidf_grupo_anos)

# ===================================
# VERIFICAÇÃO DAS CORES (OPCIONAL)
# ===================================
cat("\n=== Paleta de Cores Utilizada ===\n")
for (i in 1:length(cores_grupos)) {
  cat(paste("Grupo:", names(cores_grupos)[i], "- Cor:", cores_grupos[i], "\n"))
}

# ===================================
# SALVANDO OS GRÁFICOS
# ===================================

if (!dir.exists("graficos_tsne")) {
  dir.create("graficos_tsne")
}

cat("Salvando gráficos HTML interativos...\n")
htmlwidgets::saveWidget(
  grafico_tf_grupo_anos, 
  file = "graficos_tsne/tsne_tf_grupos_anos.html",
  selfcontained = TRUE,
  title = "t-SNE TF por Grupos de Anos"
)

htmlwidgets::saveWidget(
  grafico_tfidf_grupo_anos, 
  file = "graficos_tsne/tsne_tfidf_grupos_anos.html",
  selfcontained = TRUE,
  title = "t-SNE TF-IDF por Grupos de Anos"
)

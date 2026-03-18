#===================================================================
# Gerador de Gráficos de Evolução por Pares (Exportação PDF)
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. PREPARAÇÃO GLOBAL (CORES E PARES) ---

# 1.1. Identificar os 10 tópicos com mais músicas
# (Certifique-se de que 'resumo_topicos_estudo' está carregado)
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

# 1.2. Criar a Paleta de Cores Global (Mapeamento ID -> COR)
# Garante que o Tópico X tenha a mesma cor em todos os 5 PDFs
topicos_a_colorir <- sort(top_10_topicos_por_musicas)
paleta_de_cores_global <- scales::hue_pal()(length(topicos_a_colorir))
names(paleta_de_cores_global) <- as.character(topicos_a_colorir)

# 1.3. Preparação dos Dados de Proporção
total_musicas_por_ano_completo <- data_dp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

dados_grafico_proporcao_dp <- data_dp %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas) %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop") %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# 1.4. Criar o Mapa de Pares (1 & 2 -> Par 1, etc.)
mapa_pares <- tibble(
  Grupos_MM_DP = top_10_topicos_por_musicas,
  ordem_global = seq_along(top_10_topicos_por_musicas),
  rotulo_par = paste("Par", ceiling(ordem_global / 2))
)

dados_grafico_proporcao_dp_pares <- dados_grafico_proporcao_dp %>%
  left_join(mapa_pares %>% select(Grupos_MM_DP, rotulo_par), by = "Grupos_MM_DP")

pares_unicos <- unique(mapa_pares$rotulo_par)

# --- 2. LOOP DE SALVAMENTO DOS 5 PDFs ---

cat("\nA iniciar a exportação dos 5 gráficos de evolução para PDF...\n")

for (par_atual in pares_unicos) {
  
  # 2.1. Filtrar dados do par atual
  df_plot_individual <- dados_grafico_proporcao_dp_pares %>%
    filter(rotulo_par == par_atual)
  
  # Identificar os tópicos neste par para o título e legenda
  topicos_presentes <- unique(df_plot_individual$Grupos_MM_DP)
  titulo_formatado <- paste(topicos_presentes, collapse = " e ")
  
  # 2.2. Gerar o gráfico ggplot2
  p <- ggplot(df_plot_individual,
              aes(x = Ano, y = proporcao,
                  color = factor(Grupos_MM_DP, levels = as.character(topicos_a_colorir)),
                  group = factor(Grupos_MM_DP))) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    
    # Rótulos e Títulos
    labs(
      #title = paste0("Proporção de músicas dos tópicos ", titulo_formatado, " ao longo dos anos" ),
      #subtitle = "Proporção de músicas no corpus total por ano de lançamento",
      x = "Ano",
      y = "Proporção de músicas",
      color = "Tópico"
    ) +
    
    # Formatação de Eixos
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
    
    # Aplicação da Cor Manual (Sincronizada e Dinâmica)
    scale_color_manual(
      values = paleta_de_cores_global,
      breaks = as.character(topicos_presentes) # Mostra apenas estes 2 na legenda
    ) +
    
    # Tema para Dissertação
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      # Margem de segurança para o PDF
      plot.margin = margin(15, 10, 15, 10)
    )
  
  # 2.3. Salvamento do Ficheiro
  nome_ficheiro <- paste0("evolucao_proporcao_", gsub(" ", "_", par_atual), ".pdf")
  
  # Guardar em alta resolução vetorial (9x6 é uma excelente proporção para linhas)
  ggsave(nome_ficheiro, plot = p, width = 9, height = 6, device = "pdf")
  
  cat("Ficheiro guardado:", nome_ficheiro, "\n")
}

cat("\nProcesso concluído. Verifique os 5 PDFs na sua pasta de trabalho.\n")
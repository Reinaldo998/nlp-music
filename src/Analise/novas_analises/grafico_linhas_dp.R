library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)

# Assumimos que os dataframes 'data_dp' e 'top_10_topicos_por_musicas' estão na memória.

# --- 1. Preparar os dados para os gráficos de proporção por ano (APENAS os 10 tópicos mais frequentes) ---

dados_filtrados_para_grafico <- data_dp %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

total_musicas_por_ano_dp <- data_dp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

musicas_topico_ano_dp_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

dados_grafico_proporcao_dp <- musicas_topico_ano_dp_filtrado %>%
  left_join(total_musicas_por_ano_dp, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

print("Dados para o gráfico preparados com sucesso.")

# --- 2. Gerar o gráfico interativo plotly ---

cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 10 TÓPICOS MAIS FREQUENTES (PLOTLY)\n")
cat("=============================================\n")

# Crie o objeto ggplot com a estética 'text' para o balão interativo
p_base_plotly_dp <- ggplot(dados_grafico_proporcao_dp,
                           aes(x = Ano, y = proporcao,
                               color = factor(Grupos_MM_DP),
                               group = factor(Grupos_MM_DP),
                               # --- A estética 'text' é crucial para o balão interativo do Plotly ---
                               text = paste("Tópico: ", Grupos_MM_DP,
                                            "<br>Ano: ", Ano,
                                            "<br>Proporção: ", scales::percent(proporcao)))) +
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
plotly_dp_top10_proporcao_final_com_balao <- ggplotly(p_base_plotly_dp, tooltip = "text")

# Exibir o gráfico interativo
print(plotly_dp_top10_proporcao_final_com_balao)

# Opcional: Salvar o gráfico interativo em HTML
# O 'caminho_arquivo_html_dp' deve ser definido
#caminho_arquivo_html_dp <- "grafico_data_dp_top10_proporcao_interativo_hibrid.html"
#saveWidget(plotly_dp_top10_proporcao_final_com_balao, file = caminho_arquivo_html_dp, selfcontained = TRUE)
#cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html_dp, "'\n"))
library(ggplot2)
library(dplyr)

# Assumimos que 'resumo_topicos_estudo' já foi criado e está na memória.

# --- 1. Preparar os dados para o gráfico ---
# Ordenamos os tópicos pelo número de músicas (num_musicas) de forma decrescente.
df_plot_frequencia <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  mutate(
    # Converte o ID do tópico para um fator ordenado pela contagem (para o eixo X)
    Grupo_Fator_Ordenado = factor(Grupos_MM_DP, levels = Grupos_MM_DP) 
  )


# --- 2. Gerar o Gráfico de Barras Ordenado (Estilo Histograma de Frequência) ---

p_frequencia_topicos <- ggplot(df_plot_frequencia, 
                               aes(x = Grupo_Fator_Ordenado, y = num_musicas)) +
  
  # 2.1. Cria as barras (colunas)
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # 2.2. Adiciona o valor exato no topo de cada barra (para clareza acadêmica)
  geom_text(aes(label = num_musicas), vjust = -0.5, size = 3) + 
  
  labs(
    title = "Distribuição do Tamanho dos Tópicos LDA ",
    x = "ID do Tópico (Ordenado por Frequência)",
    y = "Número de músicas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    # Remove o título do eixo X para que os rótulos dos tópicos fiquem mais limpos
    axis.title.x = element_blank(),
    # Alinha os rótulos dos IDs dos tópicos para melhor leitura
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

print(p_frequencia_topicos)


p_frequencia_topicos_lda <- ggplot(df_plot_frequencia, 
                                   aes(x = Grupo_Fator_Ordenado, y = num_musicas)) +
  
  # 1. Cria as barras (colunas)
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # 2. Adiciona o valor exato no topo (ajustado para ser legível)
  geom_text(aes(label = num_musicas), 
            vjust = -0.5, 
            size = 2.5,          # Tamanho menor para não amontoar
            fontface = "bold") +  
  
  labs(
    title = expression(bold("Distribuição do tamanho dos tópicos ") ~ bolditalic("K-means")),
    x = "ID do Tópico (Ordenado por Frequência)",
    y = "Número de Músicas"
  ) +
  
  # 3. Expande o eixo Y para o número no topo não ser cortado
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    
    # AJUSTES CRÍTICO PARA NÃO SOBREPOR:
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,           # Mudança para 90 graus (vertical)
      vjust = 0.5,          # Alinhamento centralizado no tick
      hjust = 1,            # Alinhamento à direita (em relação ao texto)
      size = 8              # Redução do tamanho da fonte dos IDs
    ),
    
    # Remove grades verticais para limpar o visual com 44 barras
    panel.grid.major.x = element_blank()
  )

# Exibe o gráfico
print(p_frequencia_topicos_lda)

# Opcional: Para salvar em alta resolução
# ggsave("distribuicao_frequencia_topicos.png", plot = p_frequencia_topicos, width = 10, height = 6)

library(ggplot2)

# Salve o gráfico em alta resolução (300 DPI é o mínimo acadêmico)
ggsave(
  filename = "distribuicao_frequencia_LDA.png",
  plot = p_frequencia_topicos,
  width = 15, # Largura em polegadas (para acomodar 30 barras)
  height = 7,  # Altura em polegadas
  units = "in",
  dpi = 600 # Resolução alta para publicação/dissertação
)

cat("\nGráfico salvo com sucesso em alta resolução (600 DPI) no diretório de trabalho.\n")
# title = expression(bold("Distribuição do tamanho dos tópicos ") ~ bolditalic("LDA")),
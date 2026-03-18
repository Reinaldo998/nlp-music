#===================================================================
# Gráfico de frequência por tópico LDA (Ajustado para 85 Grupos)
#===================================================================

library(ggplot2)

# Certifique-se de que o objeto 'df_plot_frequencia' tem as 85 linhas
# e as colunas 'Grupo_Fator_Ordenado' e 'num_musicas'

p_frequencia_topicos_lda <- ggplot(df_plot_frequencia, 
                                   aes(x = Grupo_Fator_Ordenado, y = num_musicas)) +
  
  # 1. Criação das barras
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # 2. Valores no topo das barras (Restaurado para o padrão anterior de legibilidade)
  geom_text(aes(label = num_musicas), 
            vjust = -0.5, 
            size = 2.8,          # Valor do 'caso anterior' que você aprovou
            fontface = "bold") +  
  
  labs(
    title = expression(bold("Distribuição do tamanho dos tópicos ") ~ bolditalic("LDA")),
    x = "ID do Tópico (Ordenado por Frequência)",
    y = "Número de Músicas"
  ) +
  
  # 3. Expansão do eixo Y para acomodar os rótulos superiores sem cortar
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  theme_minimal() +
  theme(
    # Centraliza o título
    plot.title = element_text(hjust = 0.5, size = 14),
    
    # AJUSTES PARA O EIXO X (85 GRUPOS A 45 GRAUS):
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,           # Mantendo o padrão visual da dissertação
      hjust = 1,            # Alinha a ponta do texto ao eixo
      size = 5.5            # Tamanho reduzido para evitar a sobreposição dos 85 IDs
    ),
    
    # Limpeza visual: remove as grades verticais
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Visualizar o gráfico
print(p_frequencia_topicos_lda)

#===================================================================
# Dica de Exportação para 85 tópicos:
#===================================================================
# Como a densidade de dados é alta, salve com uma largura maior para 
# garantir que o texto do eixo X não fique "esmagado".
ggsave("frequencia_lda_85_topicos.png", 
      plot = p_frequencia_topicos_lda, 
      width = 16, 
      height = 7, 
      dpi = 300)

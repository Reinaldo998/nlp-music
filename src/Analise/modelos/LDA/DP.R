#===================================================================
# Instalação e Carregamento de Pacotes
#===================================================================
# Instala os pacotes necessários caso não estejam instalados
required_packages <- c("dplyr", "ggplot2", "scales", "plotly", "htmlwidgets", "MGLM", "coda", "compiler", "Matrix")
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
library(MGLM)
library(coda)
library(compiler)
library(Matrix)
enableJIT(3)

#
#===================================================================
# Funções Auxiliares
#===================================================================
# Gerador de números aleatórios de uma distribuição discreta
rDiscreta <- function(p) {
  u <- runif(1)
  P <- cumsum(p)
  val <- sum(P < u) + 1
  val
}

# Gerador para o parâmetro de concentração alpha (Algoritmo 8 de Neal)
gera_eta_alpha <- function(alpha, a_alph_prior, b_alph_prior, K, m) {
  eta_aux <- rbeta(1, alpha + 1, m)
  aux_prob <- (a_alph_prior + K - 1) / (m * (b_alph_prior - log(eta_aux)))
  prob_alpha <- aux_prob / (1 + aux_prob)
  unif <- runif(1)
  
  if (unif <= prob_alpha) {
    alpha <- rgamma(1, a_alph_prior + K, b_alph_prior - log(eta_aux))
  } else {
    alpha <- rgamma(1, a_alph_prior + K - 1, b_alph_prior - log(eta_aux))
  }
  return(alpha)
}

#
#===================================================================
# Leitura e Preparação dos Dados a partir dos arquivos salvos
# ATENÇÃO: Os caminhos foram ajustados para sua configuração.
#===================================================================
#
# Caminho para a pasta onde o SCRIPT (DP.R) e os arquivos de saída serão salvos
caminho_saida <- "/home/reinaldo/Reinaldo"
# Caminho para a pasta onde os DADOS estão
caminho_dados <- "/home/reinaldo"

# 1. Lendo a DTM esparsa do formato .mtx a partir da pasta de dados
dtm_esparsa <- readMM(paste(caminho_dados, "dtm_filtrada.mtx", sep="/"))
# Atribuindo o vocabulário para as colunas
vocab <- read.csv(paste(caminho_dados, "vocab_dtm_filtrada.csv", sep="/"))
colnames(dtm_esparsa) <- vocab$term

# Convertendo a matriz esparsa para a matriz y_ap que o código precisa
y_ap <- as.matrix(dtm_esparsa)

# 2. Lendo o data frame original com as informações das músicas a partir da pasta de dados
dados_musicas_orig <- read.csv(paste(caminho_dados, "data_final_pt_original.csv", sep="/"))

#
#===================================================================
# Configuração e Execução do Gibbs Sampling
#===================================================================
#
amostrasfin <- 100
burnin <- 50
saltos <- 5
AmostrasTotal <- burnin + amostrasfin * saltos
Nt <- nrow(y_ap)
m <- Nt # m é o número de textos na base

set.seed(100)
alpha <- 1
beta0 <- rep(1, ncol(y_ap))
a_alph_prior <- b_alph_prior <- 0.01
Sj <- rep(1, nrow(y_ap))
K <- 1
betapost <- matrix(apply(y_ap, 2, sum) + beta0, ncol = 1)

# Nomes dos arquivos de saída (diferentes para evitar conflito)
file_Sj <- paste(caminho_saida, "Sj_terminal_novo.txt", sep="/")
file_Alpha <- paste(caminho_saida, "Alpha_terminal_novo.txt", sep="/")

# Gibbs sampling
for (int in 1:AmostrasTotal) {
  cat('\n', int, K, alpha)
  
  for (i in 1:Nt) {
    nk <- numeric(K)
    for (k in 1:K) 	nk[k] <- sum(Sj[-i] == k)
    betapostnew <- betapost
    if (nk[Sj[i]] > 0) betapostnew[, Sj[i]] <- betapostnew[, Sj[i]] - y_ap[i, ]
    prob <- numeric(K)
    for (k in 1:K) prob[k] <- log(nk[k]) + ddirmn(y_ap[i, ], betapostnew[, k])
    prob <- c(prob, log(alpha) + ddirmn(y_ap[i, ], beta0))
    prob <- prob - max(prob)
    prob <- exp(prob)
    Snew <- rDiscreta(prob / sum(prob))
    
    if (Snew != Sj[i] & Snew <= K) {
      Sj[i] <- Snew
      nk[Sj[i]] <- nk[Sj[i]] + 1
      betapost <- betapostnew
      betapost[, Sj[i]] <- betapost[, Sj[i]] + y_ap[i, ]
    }
    
    if (Snew != Sj[i] & Snew > K) {
      Sj[i] <- Snew
      nk <- c(nk, 1)
      betapost <- betapostnew
      betapost <- cbind(betapost, y_ap[i, ] + beta0)
    }
    
    while (length(table(Sj)) < max(Sj)) {
      categr <- as.numeric(as.character(data.frame(table(Sj))[, 1]))
      categd <- seq(1:length(table(Sj)))
      dif <- which(categr != categd)
      Sj[which(Sj > dif[1])] <- Sj[which(Sj > dif[1])] - 1
      betapost <- matrix(c(betapost[, -dif[1]]), nrow = ncol(y_ap))
      K <- ncol(betapost)
    }
    
    if (length(table(Sj)) < K) {
      betapost <- matrix(c(betapost[, -K]), nrow = ncol(y_ap))
      K <- ncol(betapost)
    }
    
    K <- ncol(betapost)
  }
  
  alpha <- gera_eta_alpha(alpha, a_alph_prior, b_alph_prior, K, Nt)
  
  if (int > burnin & int %% saltos == 0) {
    if (int == (burnin + saltos)) {
      cat('', Sj, file = file_Sj, append = FALSE)
      cat('', alpha, file = file_Alpha, append = FALSE)
    } else {
      cat('', Sj, file = file_Sj, append = TRUE)
      cat('', alpha, file = file_Alpha, append = TRUE)
    }
  }
}

#
#===================================================================
# Consolidação dos Resultados e Análise Exploratória
#===================================================================
#
cat('\n\n--- Análise e Consolidação dos Resultados ---\n')

Sj_amostras <- scan(file = file_Sj)
alpha_amostras <- scan(file = file_Alpha)
iter <- amostrasfin
m <- Nt 
S <- matrix(Sj_amostras, ncol = m, nrow = iter, byrow = TRUE)

log_v <- mcmc(alpha_amostras)
cat('Média de alpha:', mean(alpha_amostras), '\n')
cat('Número médio de grupos (K):', mean(apply(S, 1, function(x) length(unique(x)))), '\n')

prob.eq <- matrix(0, nrow = ncol(S), ncol = ncol(S))
for (i in 1:ncol(S)) {
  for (j in 1:ncol(S)) {
    prob.eq[i, j] <- round(sum(S[, i] == S[, j]) / nrow(S), 5) * 100
  }
}

thresh <- 50
clust_f <- c(1, rep(0, (ncol(S) - 1)))
for (i in 2:ncol(S)) {
  if (max(prob.eq[i, 1:(i - 1)]) > thresh) {
    clust_f[i] <- clust_f[which(prob.eq[i, 1:(i - 1)] == max(prob.eq[i, 1:(i - 1)]))[1]]
  } else {
    clust_f[i] <- max(clust_f[1:(i - 1)] + 1)
  }
}

thesing <- 30
singl <- which(clust_f %in% which(table(clust_f) == 1))
if (length(singl) > 0) {
  prob.eq.sin <- prob.eq[singl, ]
  for (i in 1:nrow(prob.eq.sin)) {
    prob.eq.sin[i, singl[i]] <- 0
    if (max(prob.eq.sin[i, ]) > thesing) {
      clust_f[singl[i]] <- clust_f[which(prob.eq.sin[i, ] == max(prob.eq.sin[i, ]))[1]]
    }
  }
}

while (length(table(clust_f)) < max(clust_f)) {
  categr <- as.numeric(as.character(data.frame(table(clust_f))[, 1]))
  categd <- seq(1:length(table(clust_f)))
  dif <- which(categr != categd)
  if (length(dif) > 0) {
    clust_f[which(clust_f > dif[1])] <- clust_f[which(clust_f > dif[1])] - 1
  }
}

Grupos_MM_DP <- clust_f
cat('Número de grupos consolidados:', length(unique(Grupos_MM_DP)), '\n')
cat('Tamanho de cada grupo:\n')
print(table(Grupos_MM_DP))

file_Grupos_MM_DP <- paste(caminho_saida, "Grupos_MM_DP_terminal_novo.txt", sep="/")
cat('', Grupos_MM_DP, file = file_Grupos_MM_DP)

cat('\n\n--- Análise Exploratória e Visualização ---\n')
data_agrupado <- cbind(dados_musicas_orig, Grupos_MM_DP) %>%
  mutate(
    Ano = as.numeric(as.character(Ano)),
    Grupos_MM_DP = as.numeric(as.character(Grupos_MM_DP))
  )

resumo_topicos_estudo <- data_agrupado %>%
  group_by(Grupos_MM_DP) %>%
  summarise(numero_de_musicas = n(), media_do_ano = round(mean(Ano, na.rm = TRUE))) %>%
  arrange(desc(numero_de_musicas))

top_10_topicos_por_musicas <- resumo_topicos_estudo %>% head(10) %>% pull(Grupos_MM_DP)
lista_artistas_top_10_musicas <- list()
for (i in top_10_topicos_por_musicas) {
  artistas_por_topico_corrente_dp <- data_agrupado %>%
    filter(Grupos_MM_DP == i) %>%
    group_by(Artista.y) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>%
    mutate(posicao = row_number()) %>%
    select(posicao, Artista.y)
  colnames(artistas_por_topico_corrente_dp)[2] <- paste0("Tópico ", i)
  lista_artistas_top_10_musicas[[as.character(i)]] <- artistas_por_topico_corrente_dp
}

tabela_artistas_alinhada_top_musicas <- lista_artistas_top_10_musicas %>%
  purrr::reduce(full_join, by = "posicao") %>%
  arrange(posicao) %>%
  mutate_all(~replace_na(., ""))

cat("\n--- Tabela de Artistas Mais Frequentes ---\n")
print(tabela_artistas_alinhada_top_musicas)

dados_filtrados_para_grafico_dp <- data_agrupado %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

total_musicas_por_ano_dp <- data_agrupado %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

dados_grafico_proporcao_dp <- dados_filtrados_para_grafico_dp %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = 'drop') %>%
  left_join(total_musicas_por_ano_dp, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

p_final_plotly_com_balao <- ggplot(dados_grafico_proporcao_dp,
                                   aes(x = Ano, y = proporcao,
                                       color = factor(Grupos_MM_DP),
                                       group = factor(Grupos_MM_DP),
                                       text = paste("Tópico: ", Grupos_MM_DP,
                                                    "<br>Ano: ", Ano,
                                                    "<br>Proporção: ", scales::percent(proporcao)))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Proporção dos 10 Tópicos Mais Frequentes ao Longo dos Anos",
       x = "Ano", y = "Proporção de Músicas", color = "Tópico") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "right")

plotly_final <- ggplotly(p_final_plotly_com_balao, tooltip = "text")
saveWidget(plotly_final, file = paste0(caminho_saida, "/grafico_terminal_novo.html"), selfcontained = TRUE)
cat("\nGráfico interativo salvo como 'grafico_terminal_novo.html'\n")
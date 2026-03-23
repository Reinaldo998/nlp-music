library(rvest)
library(dplyr)
library(stringr)
library(tidyr)

# Defina a URL para o ano de 2007
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_07.html"

# Extraia os dados
dados <- read_html(url) %>%
  html_nodes("table tr") %>%
  html_text(trim = TRUE) %>%
  as.data.frame()

# A coluna extraída pode ter um nome diferente. Renomeie a primeira coluna para "V1".
colnames(dados) <- "V1"

# Filtrando dados
dados <- dados %>%
  filter(V1 != "" & !str_detect(V1, "Top Hits")) # Remove cabeçalho e linhas vazias

# Limpeza adicional para remover números no início
dados <- dados %>%
  mutate(V1 = str_squish(V1)) %>%
  mutate(V1 = str_remove(V1, "^[0-9]+\\s+")) # Remove números no início da linha

# Separe a música e o artista
dados <- dados %>%
  separate(V1, into = c("Nome", "Artista"), sep = "\\s(?=[A-Z][a-z])", fill = "right", extra = "merge") %>%
  mutate(Artista = str_squish(Artista)) %>% # Remover espaços desnecessários
  filter(!is.na(Nome) & !is.na(Artista)) # Remover entradas com NAs

# Remover hífen no final do nome da música, se houver
dados <- dados %>%
  mutate(Nome = str_remove(Nome, "\\s*-\\s*$")) # Remove o hífen ao final do nome da música

# Corrigir casos específicos em que o nome do artista foi puxado para o nome da música
dados <- dados %>%
  mutate(Artista = case_when(
    str_detect(Nome, "SE QUISER - Tânia") ~ "Tânia Mara",
    str_detect(Nome, "LEILÃO - César") ~ "César Menotti & Fabiano",
    str_detect(Nome, "CASO POR ACASO César") ~ "César Menotti & Fabiano",
    str_detect(Nome, "RAZÕES E EMOÇÕES - NX") ~ "NX Zero", # Correção específica para "RAZÕES E EMOÇÕES"
    TRUE ~ Artista
  ),
  Nome = case_when(
    str_detect(Nome, "SE QUISER - Tânia") ~ "SE QUISER",
    str_detect(Nome, "LEILÃO - César") ~ "LEILÃO",
    str_detect(Nome, "CASO POR ACASO César") ~ "CASO POR ACASO",
    str_detect(Nome, "RAZÕES E EMOÇÕES - NX") ~ "RAZÕES E EMOÇÕES", # Correção específica para "RAZÕES E EMOÇÕES"
    TRUE ~ Nome
  ))

# Adicione a coluna do ano
dados$Ano <- 2007

# Remova a primeira linha
resultado <- dados %>%
  slice(-1) # Remove a primeira linha

# Selecione as colunas finais
resultado <- resultado %>%
  select(Nome, Artista, Ano)

# Adicionar a coluna "Codigo" com a classificação da música para o ano de 2007
resultado$Codigo <- paste0("07", sprintf("%02d", 1:nrow(resultado)))

# Exiba o data.frame final
#print(resultado)
View(resultado)

#write.csv(resultado, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_07.csv", row.names = FALSE)


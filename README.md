# Modelagem de tópicos e evolução temporal: uma análise das músicas de maior audiência no Brasil

Este repositório contém os códigos e dados desenvolvidos como parte da dissertação de mestrado em Estatística do Programa Interinstitucional de Pós-Graduação em Estatística **UFSCar/ICMC-USP**.

---

## Estrutura do Projeto (`/src`)

O código está organizado em duas grandes frentes:

### 1. [/data](./src/data) (Obtenção e organização dos dados)
Focada na aquisição e tratamento do corpus textual.
* **[pegando_nome/](./src/data/pegando_nome)** (R): Web scraping do nome da música, artistas e ano de lançamento.
* **[pegando_letra/](./src/data/pegando_letra)** (Python/Selenium): Automação para captura do conteúdo textual das letras.
* **[organiza_dados/](./src/data/organiza_dados)**:
    * [conferencia_das_letras/](./src/data/organiza_dados/conferencia_das_letras): Validação da integridade entre os nomes das músicas e letras capturados.
    * [ajuste_dados/](./src/data/organiza_dados/ajuste_dados): Atualização e limpeza dos dados.
    * [processamento_vetorizacao/](./src/data/organiza_dados/processamento_vetorizacao): Pré-processamento e vetorização das letras.

### 2. [/Analise](./src/Analise) (Análise e Modelagem de tópicos)
Focada na aplicação dos métodos e análise dos resultados.
* **[LDA/](./src/Analise/LDA)**: Implementação e diagnóstico do modelo LDA.
* **[MM/](./src/Analise/MM)**: Modelos de Mistura Infinita e análise dos resultados.
* **[kmeans/](./src/Analise/kmeans)**: Agrupamento via métodos hierárquicos e K-means.
* **[modelos/](./src/Analise/modelos)**: Redução de dimensionalidade e visualização (PCA, UMAP e t-SNE).
* **[distancia/](./src/Analise/distancia)**: Cálculos de métricas de distâncias (JSD) e análise dos resultados.
* **[similaridade/](./src/Analise/similaridade)**: Cálculos de métricas de Similaridade de Cosseno e análise dos resultados.

## Linguagens utilizadas
* **R:** Análise estatística e pré-processamento.
* **Python:** Automação de scraping (Selenium).

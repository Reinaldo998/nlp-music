# Análise Estatística e PLN: Letras de Música Brasileira (1958-2017)



Este repositório contém códigos e dados desenvolvidos como parte da dissertação de mestrado em Estatística do Programa Interinstitucional de Pós-Graduação em Estatística UFSCar/ICMC-USP, intitulada: "Modelagem de tópicos e evolução temporal: uma análise das músicas de maior audiência no Brasil".



\## Estrutura do Projeto (`/src`)



O código está organizado em duas grandes frentes:



\### 1. `/data` (Obtenção e organização dos dados)

Focada na aquisição e tratamento do corpus textual.

\* \*\*`pegando\_nome/` (R):\*\* Web scraping do nome da música, artistas e ano de lançamento.

\* \*\*`pegando\_letra/` (Python/Selenium):\*\* Automação para captura do conteúdo textual das letras.

\* \*\*`organiza\_dados/`:\*\*

&#x20;   \* `conferencia\_das\_letras/`: Validação da integridade entre os nomes das músicas e letras capturadas.

&#x20;   \* `ajuste\_dados/`: Atualização e limpeza dos dados.

&#x20;   \* `processamento\_vetorizacao/`: Pré-processamento e vetorização das letras.



\### 2. `/Analise` (Análise e Modelagem de tópicos)

Focada na aplicação dos métodos e análise dos resultados.

\* \*\*`LDA/`:\*\* Implementação e diagnóstico do modelo LDA.

\* \*\*`MM/`:\*\* Modelos de Mistura Infinita e análise dos resultados.

\* \*\*`kmeans/`:\*\* Agrupamento via métodos hierárquicos e K-means.

\* \*\*`modelos/`:\*\* Redução de dimensionalidade e visualização de alta dimensão (PCA, UMAP e t-SNE).

\* \*\*`distancia/`:\*\* Cálculos de métricas de distâncias (Divergência de Jensen-Shannon - JSD) e análise dos resultados.

\* \*\*`similaridade/`:\*\* Cálculos de métricas de Similaridade de Cosseno e análise dos resultados.



\## Linguagens utilizadas

\* \*\*R:\*\* Análise estatística, pré-processamento e modelagem.

\* \*\*Python:\*\* Automação de scraping (Selenium).


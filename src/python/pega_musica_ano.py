import requests
from bs4 import BeautifulSoup
import pandas as pd
import re

# URL da página web
url = "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_75.html"

# Fazendo a requisição para obter o conteúdo HTML da página
response = requests.get(url)
pagina = BeautifulSoup(response.content, "html.parser")

# Extraindo os nomes das músicas
nomes_musicas = pagina.select("table tr td:nth-child(2) font")

# Limpando e processando os dados
nomes_musicas = [re.sub(r"\s+\n\s*", " ", musica.get_text(strip=True)) for musica in nomes_musicas]

# Separando o nome da música do artista
musicas_artistas = [musica.split(" - ", 1) for musica in nomes_musicas]

# Convertendo para DataFrame
musicas_artistas_df = pd.DataFrame(musicas_artistas, columns=["Nome", "Artista"])

# Limpando espaços em branco extras
musicas_artistas_df = musicas_artistas_df.apply(lambda x: x.str.strip())

# Imprimindo o resultado
print(musicas_artistas_df)

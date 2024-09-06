import requests
from bs4 import BeautifulSoup
import urllib.parse

# Definir o nome da música a ser pesquisada
nome_musica = "garoto de pobre - Geraldo filme"

# Codificar o nome da música para ser usado na URL de pesquisa
nome_musica_formatado = urllib.parse.quote(nome_musica)

# Criar a URL de pesquisa
url_pesquisa = f"https://www.letras.mus.br/?q={nome_musica_formatado}"

# Fazer a requisição HTTP para a página de pesquisa
response = requests.get(url_pesquisa)

# Verificar se a requisição foi bem-sucedida
if response.status_code == 200:
    # Carregar o conteúdo HTML da página
    pagina_pesquisa = BeautifulSoup(response.text, 'html.parser')
    
    # Imprimir a URL da página de resultados
    print(f"URL da página de resultados: {url_pesquisa}")
else:
    print(f"Erro ao acessar a página: {response.status_code}")

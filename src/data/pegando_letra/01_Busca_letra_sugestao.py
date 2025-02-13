import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import json
import re
from pathlib import Path
from tqdm import tqdm  # Adicionando a importação do tqdm
import random  # Para gerar pausas aleatórias

# Configurações do Selenium
service = Service('/home/daiane/Reinaldo/geckodriver')  # Atualize o caminho
options = Options()
# options.add_argument('--headless')  # Executar em modo headless, remova para visualizar o navegador
driver = webdriver.Firefox(service=service, options=options)

# Configurar o diretório de saída
output_dir = Path("/home/daiane/Reinaldo/processed")
output_dir.mkdir(parents=True, exist_ok=True)

# Função para sanitizar nomes de arquivos
def sanitize_filename(filename):
    return re.sub(r'[<>:"/\\|?*]', '', filename)

# Função para buscar a letra, o nome da música e o artista no Letras.mus.br
def buscar_letra_e_detalhes(musica, id_musica):
    try:
        # Acessar o site Letras.mus.br
        driver.get("https://www.letras.mus.br")
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "input.searchBar-input"))
        )
        
        # Buscar somente o nome da música na barra de pesquisa
        search_box = driver.find_element(By.CSS_SELECTOR, "input.searchBar-input")
        search_box.clear()
        search_box.send_keys(musica)
        time.sleep(random.uniform(2, 4))  # Pausa aleatória entre 2 a 4 segundos para esperar as sugestões
        
        # Clicar na primeira sugestão
        first_suggestion = WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, ".suggest-list.--mainSearch .suggest-song a"))
        )
        first_suggestion.click()
        
        # Aguardar o carregamento completo da página
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "div.lyric"))
        )
        
        # Capturar a letra
        letra_elements = driver.find_elements(By.CSS_SELECTOR, "div.lyric p")
        letra = "\n".join([element.text for element in letra_elements])
        
        # Capturar o nome da música e do artista
        nome_musica = WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "h1.textStyle-primary"))
        ).text
        
        nome_artista = WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "h2.textStyle-secondary"))
        ).text
        
        # Adicionar o código da música ao dicionário
        return {
            'ID': id_musica,
            'Nome': nome_musica,
            'Artista': nome_artista,
            'Letra': letra
        }
    except Exception as e:
        print(f"Erro ao processar {musica}: {e}")
        return None

# Carregar o data.frame com músicas
df = pd.read_csv("/home/daiane/Reinaldo/musicas_completas.csv")  # Atualize o caminho do CSV

# Variável para contar requisições feitas
requisicoes_realizadas = 0

# Gerar número inicial de músicas até a pausa
musicas_ate_pausa = random.randint(44, 45)

# Configuração para salvar os dados
for index, row in tqdm(df.iterrows(), total=len(df), desc="Processando letras"):
    musica = row['Nome']
    artista = row['Artista']
    ano = row['Ano']
    id_musica = row['Codigo']

    # Sanitizar o nome da música para garantir que não contenha caracteres inválidos
    musica_sanitizada = sanitize_filename(musica.replace(' ', '_'))

    # Nome do arquivo JSON para salvar os resultados
    file_path = output_dir / f"{id_musica}_{musica_sanitizada}.json"

    # Verificar se o arquivo já existe
    if not file_path.exists():
        tqdm.write(f"Buscando letra: {musica} (Ano: {ano}, ID: {id_musica})")
        dados = buscar_letra_e_detalhes(musica, id_musica)
        requisicoes_realizadas += 1        
        # Se a letra for encontrada, salvar no arquivo JSON
        if dados:
            file_path.write_text(json.dumps(dados, ensure_ascii=False, indent=4), encoding='utf-8')
        
    

        # Pausa aleatória após atingir o número de músicas processadas
        if requisicoes_realizadas > 0 and requisicoes_realizadas % musicas_ate_pausa == 0:
            tempo_pausa = random.uniform(250, 310)  # Pausa aleatória entre 2,5 a 3,5 minutos
            tqdm.write(f"Pausando por {int(tempo_pausa)} segundos... ({musicas_ate_pausa} músicas processadas)")
            time.sleep(tempo_pausa)  # Realizar a pausa

            # Redefinir o número de músicas até a próxima pausa
            musicas_ate_pausa = random.randint(35, 40)
            print(musicas_ate_pausa)

# Encerrar o WebDriver
driver.quit()

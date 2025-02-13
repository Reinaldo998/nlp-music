import pandas as pd
import re
import time
import json
from pathlib import Path
from tqdm import tqdm
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Configurações do Selenium
service = Service('/home/daiane/Reinaldo/geckodriver')
options = Options()
driver = webdriver.Firefox(service=service, options=options)

# Diretório de saída
output_dir = Path("/home/daiane/Reinaldo/processed_3")
output_dir.mkdir(parents=True, exist_ok=True)

def sanitize_filename(filename):
    """Remove caracteres inválidos para nomes de arquivos."""
    return re.sub(r'[<>:"/\\|?*]', '', filename)

def buscar_letra_letras_mus(musica, id_musica):
    try:
        # Acessar o site letras.mus.br
        driver.get("https://www.letras.mus.br")
        time.sleep(3)

        # Esperar até que a barra de busca esteja visível
        search_box = WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "input.searchBar-input"))
        )
        
        # Digitar o nome da música na barra de busca e pressionar Enter
        search_box.clear()
        search_box.send_keys(musica)
        search_box.send_keys(Keys.RETURN)
        
        # Esperar a página carregar
        time.sleep(6)
        
        # Capturar o primeiro link de música na página de resultados
        try:
            first_result = WebDriverWait(driver, 15).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, '.gsc-webResult .gs-title a'))
            )
            first_result.click()
            time.sleep(5)
        except Exception as e:
            print(f"Erro ao localizar o primeiro link para {musica}: {e}")
            return None
        
        # Clicar no botão "Letra" para exibir a versão sem tradução
        try:
            letra_button = WebDriverWait(driver, 5).until(
                EC.element_to_be_clickable((By.CSS_SELECTOR, 'a.js-filterLyric'))
            )
            letra_button.click()
            time.sleep(3)
        except Exception as e:
            print(f"Erro ao clicar no botão 'Letra': {e}")

        # Capturar a letra da música
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, 'div.lyric'))
        )
        letra_elements = driver.find_elements(By.CSS_SELECTOR, 'div.lyric p')
        letra = "\n".join([element.text for element in letra_elements])
        nome_musica = driver.find_element(By.CSS_SELECTOR, "h1.textStyle-primary").text
        nome_artista = driver.find_element(By.CSS_SELECTOR, "h2.textStyle-secondary").text
        
        return {
            'ID': id_musica,
            'Nome': nome_musica,
            'Artista': nome_artista,
            'Letra': letra
        }
    except Exception as e:
        print(f"Erro ao processar {musica}: {e}")
        return None

# Carregar a lista de músicas
df = pd.read_csv("/home/daiane/Reinaldo/musicas_dif_2.csv")

# Iterar sobre as músicas para buscar as letras
for index, row in tqdm(df.iterrows(), total=len(df), desc="Processando letras"):
    musica, id_musica = row['Nome'], row['Codigo']
    musica_sanitizada = sanitize_filename(musica.replace(' ', '_'))
    file_path = output_dir / f"{id_musica}_{musica_sanitizada}.json"
    
    if not file_path.exists():
        tqdm.write(f"Buscando letra: {musica} (ID: {id_musica})")
        dados = buscar_letra_letras_mus(musica, id_musica)
        
        if dados:
            file_path.write_text(json.dumps(dados, ensure_ascii=False, indent=4), encoding='utf-8')

# Encerrar o WebDriver
driver.quit()

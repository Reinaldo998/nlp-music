from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
import time

# Configurações do Selenium
service = Service('C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/src/R/geckodriver.exe')  # Atualize o caminho se necessário
options = Options()
options.add_argument('--headless')  # Rodar o Firefox em modo headless

# Inicializa o WebDriver
driver = webdriver.Firefox(service=service, options=options)

# Abre a URL
url = 'https://www.letras.mus.br/ze-ramalho/49367/'
driver.get(url)

# Aguarda o carregamento da página
time.sleep(5)  # Aguardar 5 segundos para garantir que a página carregue

# Tenta localizar o elemento com a letra da música
try:
    # Ajuste o seletor CSS com base na inspeção da página
    letra_elements = driver.find_elements(By.CSS_SELECTOR, 'div.lyric p')  # Atualize o seletor se necessário
    letra = "\n".join([element.text for element in letra_elements])
    print(letra)  # Imprime a letra da música

except Exception as e:
    print(f'Erro: {e}')  # Imprime o erro caso o elemento não seja encontrado

finally:
    driver.quit()  # Fecha o navegador

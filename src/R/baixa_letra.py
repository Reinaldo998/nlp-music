from selenium import webdriver
from selenium.webdriver.firefox.service import Service

# Caminho para o GeckoDriver
geckodriver_path = 'C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/src/R/geckodriver.exe'

# Configuração do Selenium
service = Service(geckodriver_path)
driver = webdriver.Firefox(service=service)

# Acessar a URL
driver.get("URL_DA_MUSICA")

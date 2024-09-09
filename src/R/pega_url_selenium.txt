from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options

# Definir o caminho para o geckodriver
geckodriver_path = "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/src/R/geckodriver.exe"

# Configurar o Firefox para rodar sem abrir a interface gráfica (modo headless)
options = Options()
options.headless = True  # Remova esta linha se quiser ver o navegador abrindo

# Criar uma instância do serviço do Firefox
service = Service(geckodriver_path)

# Iniciar o navegador Firefox com o GeckoDriver
driver = webdriver.Firefox(service=service, options=options)

# Acessar uma página web (exemplo: google.com)
driver.get("https://www.google.com")

# Imprimir o título da página
print(driver.title)

# Fechar o navegador
driver.quit()

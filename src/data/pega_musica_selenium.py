from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Iniciar o navegador Firefox
driver = webdriver.Firefox()

try:
    # Abrir o Google
    driver.get("https://www.google.com")
    
    # Esperar até que a página esteja completamente carregada
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.NAME, "q"))
    )
    
    # Encontrar o campo de busca do Google
    search_box = driver.find_element(By.NAME, "q")
    
    # Digitar "Frevo Mulher site:letras.mus.br" para buscar especificamente no site letras.mus.br
    search_box.send_keys("Frevo Mulher site:letras.mus.br")
    search_box.send_keys(Keys.RETURN)
    
    # Esperar até que os resultados da pesquisa apareçam
    first_result = WebDriverWait(driver, 10).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'h3'))
    )
    
    # Clicar no primeiro resultado da pesquisa
    first_result.click()
    
    # Aguardar o carregamento da nova página
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.TAG_NAME, 'body'))
    )

    # Obter a URL da página atual
    current_url = driver.current_url
    print("A URL da primeira página clicada é:", current_url)

finally:
    # Fechar o navegador
    driver.quit()



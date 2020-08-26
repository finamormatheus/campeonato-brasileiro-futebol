from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup


filename = 'Resultados-2020.csv'

f = open(filename, 'w')
    
header = 'ano,rodada,golMandante,golVisitante\n'
    
f.write(header)

my_url = 'https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/'

uClient = uReq(my_url)
page_html = uClient.read()
uClient.close()

page_soup = soup(page_html, "html.parser")

resultados = page_soup.findAll("span",{"class":"bg-blue color-white label-2"})

rodada = 0

for i in range(36,len(resultados)):
    resultado = resultados[i].string
    golMandante = resultado[0]
    golVisitante = resultado[4]
    
    if i >= 36 and i <= 41: 
        rodada = 1
    elif i%10 == 2:
        rodada += 1
    
    f.write('2020' + ',' + str(rodada) + ',' + str(golMandante) + ',' + str(golVisitante) + '\n')

f.close()

    
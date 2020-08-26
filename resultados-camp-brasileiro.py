from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup

years=['2012','2013','2014','2015','2016','2017','2018', '2019']

filename = 'Resultados.csv'

f = open(filename, 'w')
    
header = 'ano, rodada, golMandante, golVisitante\n'
    
f.write(header)

for year in years:
    my_url = 'https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/'+year
    
    uClient = uReq(my_url)
    page_html = uClient.read()
    uClient.close()
    
    page_soup = soup(page_html, "html.parser")
    
    resultados = page_soup.findAll("span",{"class":"bg-blue color-white label-2"})
    
    rodada = 0
    
    if year != "2019" and year != "2018":
        for i in range(0,len(resultados)):
            if year == '2014' and i == 299:
                golMandante = 0
                golVisitante = 1
            else:
                resultado = resultados[i].string
                golMandante = resultado[0]
                golVisitante = resultado[4]
        
            if i%10 == 0:
                rodada += 1
        
            f.write(year + ',' + str(rodada) + ',' + str(golMandante) + ',' + str(golVisitante) + '\n')
    else:
        for i in range(20,len(resultados)):
            resultado = resultados[i].string
            golMandante = resultado[0]
            golVisitante = resultado[4]
    
            if i%10 == 0:
                rodada += 1
    
            f.write(year + ',' + str(rodada) + ',' + str(golMandante) + ',' + str(golVisitante) + '\n')
f.close()

    
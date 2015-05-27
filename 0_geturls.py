from bs4 import BeautifulSoup
import urllib

page = urllib.urlopen("http://sahuaro.tucsonaz.gov/finance/")
page = page.read()

page = BeautifulSoup(page)

links = []
for dataset in page.findAll('option'):
    links.append("http://sahuaro.tucsonaz.gov" + dataset.attrs['value'])

with open("urls.txt", 'w') as file:
    file.write("\n".join(links))
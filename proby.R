library(rvest)

adres <- 'https://bioconductor.org/packages/3.7/bioc/'
folder <- 'C:/Users/Ania/Desktop/R dla zaawansowanych/paczki/'

bio <- read_html(adres)
paczki <- html_text(html_nodes(bio, 'a'))
paczki <- paczki[which(paczki=='a4'):which(paczki=='zlibbioc')]


for (i in 1:length(paczki)) {
  pakiet <- read_html(paste(adres, 'html/', paczki[i], '.html', sep=''))
  pakiet <- html_text(html_nodes(pakiet, '.rpack'))[1]
  pakiet <- substr(pakiet, 4, nchar(pakiet))
  
  plik <- tempfile(tmpdir=folder, fileext=".tar.gz")
  download.file(paste(adres, 'src/contrib/', pakiet, sep=''), plik)
  untar(plik, exdir=folder)
  file.remove(plik)
  
  w <- list.files(paste(folder, paczki[i], sep=''))
  for(j in 1:length(w)) {
    if(w[j]!='DESCRIPTION' & w[j]!= 'R')
      unlink(paste(folder, paczki[i], '/', w[j], sep=''), recursive = TRUE)
  }
  
}







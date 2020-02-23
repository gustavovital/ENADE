# script para a leitura dos PDF's da prova do ENADE-Economia
#
# Data: 19/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

library(pdftools)

# lendo os PDF's ====

pdf_subset('enade2012.pdf', pages = 8:(pdf_length('enade2012.pdf') -4), output = "enade2012_limpo.pdf")
pdf_subset('enade2015.pdf', pages = 10:(pdf_length('enade2012.pdf') -2), output = "enade2015_limpo.pdf")
pdf_subset('enade2018.pdf', pages = 13:(pdf_length('enade2012.pdf') -3), output = "enade2018_limpo.pdf")
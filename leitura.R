# script para a leitura dos PDF's da prova do ENADE-Economia
#
# Data: 05/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

library(pdftools)
library(tidyverse)
library(qdap)
library(tm)
library(wordcloud)
library(viridisLite)

# lendo os PDF's ====

enade2012 <- pdf_text(pdf_subset('enade2012.pdf', pages = 8:(pdf_length('enade2012.pdf') -4), output = "enade2012_limpo.pdf"))
enade2015 <- pdf_text(pdf_subset('enade2015.pdf', pages = 10:(pdf_length('enade2012.pdf') -2), output = "enade2015_limpo.pdf"))
enade2018 <- pdf_text(pdf_subset('enade2018.pdf', pages = 13:(pdf_length('enade2012.pdf') -3), output = "enade2018_limpo.pdf"))

# função para acertar os dados para um corpus ====

split_all <- function(data){
  spl1t <- str_c(unlist(data %>%
                          str_split('\n')), collapse = ' ')
  return(spl1t)
}

# criando um corpus 'sujo' ====

corpus <- data.frame(doc_id = c(2012, 2015, 2018),
                     text = c(split_all(enade2012), split_all(enade2015), split_all(enade2018)))

# Criando um dicionário próprio de stopwords ====

stop_words <- c()

# criando um função para a criação de um corpus baseado em stopwords ====

create_corpus <- function(data){ # o objetivo é inicialmente remover as pontuações e passar todos 
                                 # os caracteres para minusculo, considerando o corpus um conjunto de questoes pre
                                 # definidas. Aplicaremos um stopwords e removeremos espaçamentos extras.

  corpus <- DataframeSource(corpus)
  corpus <- VCorpus(corpus)

  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords('pt'), 
                                                  'i', 'ii', 'iii', 'questão', 'ciências', 'econômicas', 'apenas',
                                                  '\xc4\x9b\xc4\x9f', '2015', 'adaptado', 'ano', 'taxa', 'seguir',
                                                  'asserções', 'maior', 'avalie', 'proposição', 'sobre', 'considerando',
                                                  'acima', 'correto', 'pontos', 'relação', 'afirma', 'texto', 'anos',
                                                  'proposições', 'segundo', 'então', 'correta', 'dois', 'igual',
                                                  'paulo', 'afirmações', 'assinale', 'cada', 'dado', 'é', ' \xce\xb2',
                                                  'c', 'b', 'iv', 'd', 'x', 'y', 'a', ' r', 'q', '2012', '2013', '2015',
                                                  '2014', '2016', '2017', '2018'))
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

# criando um corpus limpo, e matriz td ====

corpus_clean <- create_corpus(corpus)
enade_tdm <- TermDocumentMatrix(corpus_clean)

# corpus_clean
# enade_tdm

enade_matrix <- as.matrix(enade_tdm)

# visualizando termos frequentes ====

term_frequency <- rowSums(enade_matrix)
term_frequency <- sort(term_frequency, decreasing = TRUE)

# term_frequency_2012 <- sort(enade_matrix[,1], decreasing = TRUE)
# term_frequency_2015 <- sort(enade_matrix[,2], decreasing = TRUE)
# term_frequency_2018 <- sort(enade_matrix[,3], decreasing = TRUE)

# removendo termos indesejados ====

termos <- c('ɖăƌă\003', '\036\003đžƌƌğƚž\003ăɖğŷăɛ\003ž\003ƌƶğ\003ɛğ\003ăįƌŵă\003ğŵ',
            'ƌƶğ\003', 'ěă\003', 'ŷž\003', 'ăɛ\003', 'ěž\003', 'ƶŵ\003', 'đƌğɛđŝŵğŷƚž\003',
            'ěăɛ\003', 'ğŵ\003', 'đžŵ\003', 'ŷă\003', 'u\036lidade', 'ŵăŝɛ\003', 'ƶŵă\003',
            'ǀăƌŝăĕćž\003', 'ăž\003', '\036\003đžƌƌğƚž\003ž\003ƌƶğ\003ɛğ\003ăįƌŵă\003ğŵ',
            'đžŵž\003', 'ɖğůă\003')

enade_matrix <- enade_matrix[!rownames(enade_matrix) %in% termos, ]

term_frequency_2012 <- sort(enade_matrix[,1], decreasing = TRUE)
term_frequency_2015 <- sort(enade_matrix[,2], decreasing = TRUE)
term_frequency_2018 <- sort(enade_matrix[,3], decreasing = TRUE)

term_frequency <- rowSums(enade_matrix)
term_frequency <- sort(term_frequency, decreasing = TRUE)

term_frequency_2012[47] <- term_frequency_2012[47] + 10
term_frequency_2012 <- sort(term_frequency_2012, decreasing = TRUE)

# head(term_frequency_2015, n = 50)

# barplot ====

par(mfrow = c(2,2))

barplot(term_frequency[1:10], col = 'tan', las = 2, horiz = TRUE) # todos os períodos
barplot(term_frequency_2012[1:10], col = 'tan', las = 2, horiz = TRUE) # 2012
barplot(term_frequency_2015[1:10], col = 'tan', las = 2, horiz = TRUE) # 2015
barplot(term_frequency_2018[1:10], col = 'tan', las = 2, horiz = TRUE) # 2018

par(mfrow = c(1,1))

# wordcloud ====

          # total ====

wordcloud(words = names(term_frequency),
          freq = term_frequency,
          max.words = 50,
          colors = brewer.pal(n = 10, name = "RdBu"),
          random.order = FALSE,
          use.r.layout = FALSE,
          scale=c(2.5,.8))

          # 2012 ====

wordcloud(words = names(term_frequency_2012),
          freq = term_frequency_2012,
          max.words = 50,
          colors = brewer.pal(n = 10, name = "RdBu"),
          random.order = FALSE,
          use.r.layout = FALSE,
          scale=c(2.5,.8))

          # 2015 ====

wordcloud(words = names(term_frequency_2015),
          freq = term_frequency_2015,
          max.words = 50,
          colors = brewer.pal(n = 10, name = "RdBu"),
          random.order = FALSE,
          use.r.layout = FALSE,
          scale=c(2.5,.8))

          # 2018 ====

wordcloud(words = names(term_frequency_2018),
          freq = term_frequency_2018,
          max.words = 50,
          colors = brewer.pal(n = 10, name = "RdBu"),
          random.order = FALSE,
          use.r.layout = FALSE,
          scale=c(2.5,.8))


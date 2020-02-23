# script para a criação de um Corpus
#
# Data: 19/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

library(pdftools)
library(tidyverse)
library(qdap)
library(tm)
# library(quanteda)

# Base de dados ====

enade2012 <- pdf_text("enade2012_limpo.pdf")
enade2015 <- pdf_text("enade2015_limpo.pdf")
enade2018 <- pdf_text("enade2018_limpo.pdf")

# função para acertar os dados para um corpus ====

split_all <- function(data){
  spl1t <- str_c(unlist(data %>%
                          str_split('\n')), collapse = ' ')
  return(spl1t)
}

# criando um corpus "sujo" ====

corpus <- data.frame(doc_id = c(2012, 2015, 2018),
                     text = c(split_all(enade2012), split_all(enade2015), split_all(enade2018)))

# criando um função para a criação de um corpus baseado em stopwords ====

source('stop_manual.R')

create_corpus <- function(data){ # o objetivo é inicialmente remover as pontuações e passar todos 
  # os caracteres para minusculo, considerando o corpus um conjunto de questoes pre
  # definidas. Aplicaremos um stopwords e removeremos espaçamentos extras.
  
  corpus <- DataframeSource(corpus)
  corpus <- VCorpus(corpus)
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords('pt'), stop_manual))
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

# criando um corpus limpo, e matriz td ====

corpus_clean <- create_corpus(corpus)
enade_tdm <- TermDocumentMatrix(corpus_clean)

# enade_tdm

enade_matrix <- as.matrix(enade_tdm)

# visualizando termos frequentes ====

term_frequency <- rowSums(enade_matrix)
term_frequency <- sort(term_frequency, decreasing = TRUE)

saveRDS(corpus_clean, 'corpus_clean.rds')
saveRDS(corpus, 'corpus.rds')
saveRDS(enade_tdm, 'enade_tdm,rds')
saveRDS(enade_matrix, 'enade_matrix.rds')

rm(list = ls())

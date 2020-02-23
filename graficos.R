# scripts para gráficos exposicionistas 
#
# autor: gustavovital@id.uff.br

# pacotes necessários ====

library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(ggthemes)

# base de dados ====

# palavras gerais

wordcloud(words = data_geral_final$termos,
          freq = data_geral_final$n,
          random.order = FALSE,
          max.words = 60,
          scale = c(3,.1), rot.per=0.35,
          colors=c('grey', 'grey20', 'grey30'))

# palavras 2018

wordcloud(words = data_2018$palavra,
          freq = data_2018$n,
          random.order = FALSE,
          max.words = 60,
          scale = c(3,.1), rot.per=0.35,
          colors= c('grey', 'grey20', 'grey30'))

# termos gerais ----

termo_geral <- termo_geral[-c(21,23,25),]
termo_geral <- termo_geral[-c(28,30:42),]
termo_geral <- termo_geral[-c(14,15),]

wordcloud(words = termo_geral$termo,
          freq = termo_geral$n,
          random.order = FALSE,
          max.words = 30,
          scale = c(3,.1), rot.per=0.4,
          colors= c('grey', 'grey20', 'grey30'),
          min.freq = 4)

# termos 2018 ====

termo_2018 <- termo_2018[-c(11,14),]

wordcloud(words = termo_2018$termo,
          freq = termo_2018$n,
          random.order = FALSE,
          max.words = 20,
          scale = c(3,.1), rot.per=0.3,
          colors= c('grey', 'grey20', 'grey30'))


# graficos de frequencia ====

# 2018 ====

termo_2018 <- termo_2018[-c(4,12),]

ggplot(subset(termo_2018, n >= 3), aes(x = reorder(termo, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_calc() +
  scale_y_continuous(breaks = seq(1, 6, 1)) +
  labs(x = 'Termos', y = 'Número de Ocorrências')

# geral ====

termo_geral <- termo_geral[-c(11,14),]

ggplot(subset(termo_geral, n >= 4), aes(x = reorder(termo, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_calc() +
  scale_y_continuous(breaks = seq(1, 12, 1)) +
  labs(x = 'Termos', y = 'Número de Ocorrências')

# Script criado para a análise de termos conjuntos 
#
# Data: 19/12/2019
# Autor: gustavovital@id.uff.br

# Pacotes Necessários ====

library(tidytext)
library(qdap)
library(tidyverse)
library(tm)

# Base de dados ====

corpus <- readRDS('corpus.rds')
source('stop_manual.R')

# Contagem geral ====

termos_gerais <- corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('termo1', 'termo2'), sep = ' ') %>% 
  filter(!termo1 %in% c(stopwords('pt'), stop_manual)) %>% 
  filter(!termo2 %in% c(stopwords('pt'), stop_manual))

termos_gerais <- termos_gerais %>% 
  mutate(termo = paste(termo1, termo2))

termos_gerais <- termos_gerais[-c(1:8),]
termos_gerais <- termos_gerais[-c(2,3,5,6,8,9,10,12,13,15:19,21:24,28:30),]
termos_gerais <- termos_gerais[-c(15:17,19,21:26,30,31),]
termos_gerais <- termos_gerais[-c(13,22:26,28,29),]
termos_gerais <- termos_gerais[-c(24:30),]

termos_gerais <- termos_gerais[-c(9,13,15,18,22,23,25:27,29:31),]
termos_gerais <- termos_gerais[-c(21,23),]


# saveRDS(termos_gerais, 'termos_gerais.rds')

# Contagem da prova de 2018 ====

termos_2018 <- corpus %>% 
  filter(doc_id == 2018) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('termo1', 'termo2'), sep = ' ') %>% 
  filter(!termo1 %in% c(stopwords('pt'), stop_manual)) %>% 
  filter(!termo2 %in% c(stopwords('pt'), stop_manual))

termos_2018 <- termos_2018 %>% 
  mutate(termo = paste(termo1, termo2))

termos_2018 <- termos_2018[-16,]
termos_2018 <- termos_2018[-21,]

# saveRDS(termos_2018, 'termos_2018.rds')

# Contagem da prova de 2015 ====

termos_2015 <- corpus %>% 
  filter(doc_id == 2015) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('termo1', 'termo2'), sep = ' ') %>% 
  filter(!termo1 %in% c(stopwords('pt'), stop_manual)) %>% 
  filter(!termo2 %in% c(stopwords('pt'), stop_manual))

termos_2015 <- termos_2015 %>% 
  mutate(termo = paste(termo1, termo2))

termos_2015 <- termos_2015[-c(1:34),]
termos_2015 <- termos_2015[-c(4:20),]
termos_2015 <- termos_2015[-c(5:29),]
termos_2015 <- termos_2015[-c(6:28),]
termos_2015 <- termos_2015[-c(8:54),]
termos_2015 <- termos_2015[-c(11:18),]
termos_2015 <- termos_2015[-c(14:77),]
termos_2015 <- termos_2015[-c(15),]
termos_2015 <- termos_2015[-c(16:36),]
termos_2015 <- termos_2015[-c(17:39),]
termos_2015 <- termos_2015[-c(18:38),]
termos_2015 <- termos_2015[-c(20),]
termos_2015 <- termos_2015[-c(21:22),]
termos_2015 <- termos_2015[-c(24),]

# Contagem da prova de 2012 ====

termos_2012 <- corpus %>% 
  filter(doc_id == 2012) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('termo1', 'termo2'), sep = ' ') %>% 
  filter(!termo1 %in% c(stopwords('pt'), stop_manual)) %>% 
  filter(!termo2 %in% c(stopwords('pt'), stop_manual))

termos_2012 <- termos_2012 %>% 
  mutate(termo = paste(termo1, termo2))

termos_2012 <- termos_2012[-17,]
termos_2012 <- termos_2012[-21,]




# Tabela - geral ====

tabela_geral <- cbind(termos_gerais$termo[1:7], termos_gerais$n[1:7],
                      termos_gerais$termo[8:14], termos_gerais$n[8:14],
                      termos_gerais$termo[15:21], termos_gerais$n[15:21])
colnames(tabela_geral) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable::xtable(tabela_geral), include.rownames = FALSE)

# Tabela - 2018 ====

tabela_2018 <- cbind(termos_2018$termo[1:7], termos_2018$n[1:7],
                      termos_2018$termo[8:14], termos_2018$n[8:14],
                      termos_2018$termo[15:21], termos_2018$n[15:21])
colnames(tabela_2018) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable::xtable(tabela_2018), include.rownames = FALSE)

# Tabela - 2015 ====

tabela_2015 <- cbind(termos_2015$termo[1:7], termos_2015$n[1:7],
                     termos_2015$termo[8:14], termos_2015$n[8:14],
                     termos_2015$termo[15:21], termos_2015$n[15:21])
colnames(tabela_2015) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable::xtable(tabela_2015), include.rownames = FALSE)

# Tabela - 2012 ====

tabela_2012 <- cbind(termos_2012$termo[1:7], termos_2012$n[1:7],
                     termos_2012$termo[8:14], termos_2012$n[8:14],
                     termos_2012$termo[15:21], termos_2012$n[15:21])
colnames(tabela_2012) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable::xtable(tabela_2012), include.rownames = FALSE)

# Núvens de Palavras ----

tibble(Termos = c(termos_2018$termo[1:21], termos_2015$termo[1:21], termos_2012$termo[1:21]),
       Prova = c(rep('2018', 21), rep('2015', 21), rep('2012', 21)),
       `Ocorrências` = c(termos_2018$n[1:21], termos_2015$n[1:21], termos_2012$n[1:21])) %>% 
  arrange(desc(`Ocorrências`)) %>%  
  ggplot(aes(label = Termos, size = as.factor(`Ocorrências`), colour = as.factor(Prova))) +
  ggwordcloud::geom_text_wordcloud(show.legend = TRUE) +
  scale_size_manual(values = c(4,6,8,10,12,14,16,18), guide = FALSE) +
  scale_colour_manual(values = viridis::inferno(5), name = 'Ocorrência na Prova de: ') +
  # labs(title = 'Núvens de Termos de Acordo com as Ocorrências nas Provas',
  #      subtitle = 'Provas de 2012; 2015; 2018') +
  ggthemes::theme_pander() +
  theme(legend.position = 'bottom',
        # text = element_text(family = 'Bookman'),
        plot.title = element_text(size = 25, colour = 'black'),
        plot.subtitle = element_text(size = 25, colour = 'black'),
        plot.title.position = 'plot',
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) 

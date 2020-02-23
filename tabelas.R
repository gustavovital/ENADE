# Script para prover tabelas dos relatórios das provas + detalhamento da prova de 2018
# 
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

library(xtable)
library(tidyverse)  

# base de dados ====

termo_2018 <- readRDS('termo_2018.rds')
termo_geral <- readRDS('termo_geral.rds')

enade_matrix <- readRDS('enade_matrix.rds')
# termos compostos ====

# 2018 ====

tabela_2018_composta <- cbind(termo_2018$termo[1:8], termo_201z8$n[1:8],
                              termo_2018$termo[9:16], termo_2018$n[9:16],
                              termo_2018$termo[17:24], termo_2018$n[17:24])
colnames(tabela_2018_composta) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable(tabela_2018_composta), include.rownames = FALSE)

# geral ====

tabela_geral_composta <- cbind(termo_geral$termo[1:8], termo_geral$n[1:8],
                              termo_geral$termo[9:16], termo_geral$n[9:16],
                              termo_geral$termo[17:24], termo_geral$n[17:24])
colnames(tabela_geral_composta) <- c('Termos', 'N', 'Termos', 'N', 'Termos', 'N')

print(xtable(tabela_geral_composta), include.rownames = FALSE)


# termos simples ====

# geral ====

soma_geral <- rowSums(enade_matrix)
soma_geral <- sort(soma_geral, decreasing = TRUE)

nomes_somageral <- names(soma_geral)

data_geral <- tibble(palavra = nomes_somageral,
                     n = soma_geral)

data_geral <- data_geral[-5,]
data_geral <- data_geral %>% 
  filter(n > 11)

data_geral$n[4] <- data_geral$n[4] - 12
data_geral$n[9] <- data_geral$n[9] - 12
data_geral$n[26] <- data_geral$n[26] - 12
data_geral$n[36] <- data_geral$n[36] - 12

data_geral <- data_geral %>% 
  filter(n > 11)

data_geral_final <- tibble(termos = c(data_geral$palavra, termo_geral$termo[1:2]),
                     n = c(data_geral$n, termo_geral$n[1:2]))

saveRDS(data_geral_final, 'data_geral_final.rds')

# tabela total ====

tabela_geral_simples <- cbind(data_geral_final$termos[1:8], data_geral_final$n[1:8],
                              data_geral_final$termos[9:16], data_geral_final$n[9:16],
                              data_geral_final$termos[17:24], data_geral_final$n[17:24],
                              data_geral_final$termos[25:32], data_geral_final$n[25:32],
                              data_geral_final$termos[33:40], data_geral_final$n[33:40])
colnames(tabela_geral_simples) <- c('Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N')

print(xtable(tabela_geral_simples), include.rownames = FALSE)


# 2018 ====

soma_2018 <- sort(enade_matrix[,3], decreasing = TRUE)
nomes_2018 <- names(soma_2018)

data_2018 <- tibble(palavra = nomes_2018,
                     n = soma_2018)

data_2018 <- data_2018 %>% 
  slice(1:81)

saveRDS(data_2018, 'data_2018.rds')

# tabela total ====

tabela_2018_simples <- cbind(data_2018$palavra[1:8], data_2018$n[1:8],
                              data_2018$palavra[9:16], data_2018$n[9:16],
                              data_2018$palavra[17:24], data_2018$n[17:24],
                              data_2018$palavra[25:32], data_2018$n[25:32],
                              data_2018$palavra[33:40], data_2018$n[33:40])
colnames(tabela_2018_simples) <- c('Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N', 'Palavra', 'N')

print(xtable(tabela_2018_simples), include.rownames = FALSE)



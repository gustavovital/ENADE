# script para a correção da matrix "text document", removendo leituras erradas de formulas,
# marcas d'agua de PDFs e etc.
#
# Data: 19/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

# Corrigindo manualmente termos errôneos ====

enade_matrix <- readRDS('enade_matrix.rds')

termos <- c('ěğ\003', 'ɖăƌă\003', '\036\003đžƌƌğƚž\003ăɖğŷăɛ\003ž\003ƌƶğ\003ɛğ\003ăįƌŵă\003ğŵ', 'ƌƶğ\003', 'ěă\003', 'ŷž\003', 'ăɛ\003', 'ěž\003', 'ƶŵ\003',
            'ŷă\003', 'đƌğɛđŝŵğŷƚž\003', 'ěăɛ\003')
enade_matrix <- enade_matrix[!rownames(enade_matrix) %in% termos, ]


term_frequency_2012 <- sort(enade_matrix[,1], decreasing = TRUE)
term_frequency_2015 <- sort(enade_matrix[,2], decreasing = TRUE)
term_frequency_2018 <- sort(enade_matrix[,3], decreasing = TRUE)

head(term_frequency_2012, n = 50)
head(term_frequency_2015, n = 50)
head(term_frequency_2018, n = 50)

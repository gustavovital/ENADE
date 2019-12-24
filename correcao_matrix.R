# script para a correção da matrix "text document", removendo leituras erradas de formulas,
# marcas d'agua de PDFs e etc.
#
# Data: 19/12/2019
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

# Corrigindo manualmente termos errôneos ====

enade_matrix <- readRDS('enade_matrix.rds')

termos <- c('ěğ\003', 'ɖăƌă\003', '\036\003đžƌƌğƚž\003ăɖğŷăɛ\003ž\003ƌƶğ\003ɛğ\003ăįƌŵă\003ğŵ', 'ƌƶğ\003', 'ěă\003', 'ŷž\003', 'ăɛ\003', 'ěž\003', 'ƶŵ\003', 'ŝŷňăĕćž\003',
            'ŷă\003', 'đƌğɛđŝŵğŷƚž\003', 'ěăɛ\003', 'đžŵ\003', 'ŵăŝɛ\003', 'ƶŵă\003', 'ǀăƌŝăĕćž\003', 'ăž\003', 'ğŵ\003', '\036\003đžƌƌğƚž\003ž\003ƌƶğ\003ɛğ\003ăįƌŵă\003ğŵ',
            'žƶ\003', 'đžŵž\003', 'ɖğůă\003', 'ƌğɛƶůƚăěžɛ\003', 'ğǆɖğđƚăɵǀăɛ\003', 'ğŷƚƌğ\003', 'u\036lidade')
enade_matrix <- enade_matrix[!rownames(enade_matrix) %in% termos, ]
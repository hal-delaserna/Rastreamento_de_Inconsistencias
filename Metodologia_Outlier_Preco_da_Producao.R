rm(list = ls())

#_____________________________________Pacotes & Funções____________________________________________ ----
library(dplyr)
library(tidyr)

caractereParaNumero <- function(x) {
  gsub("[.]", "", x) %>% gsub(pattern = " ", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% gsub(pattern = " ", replacement =
                                                                                                                    "") %>% gsub(pattern = "-", replacement = "0") %>% gsub(pattern = ",", replacement = ".") %>% as.numeric()
}

removeAcentos <- function(x) {
  gsub(pattern = "á", replacement = "a", x) %>% gsub(pattern = "â", replacement = "a") %>% 	gsub(pattern = "à", replacement = "a") %>% 	gsub(pattern = "ã", replacement = "a") %>% 	gsub(pattern = "é", replacement = "e") %>% 	gsub(pattern = "ê", replacement = "e") %>% 	gsub(pattern = "í", replacement = "i") %>% 	gsub(pattern = "ó", replacement = "o") %>% 	gsub(pattern = "ô", replacement = "o") %>% 	gsub(pattern = "õ", replacement = "o") %>% 	gsub(pattern = "ú", replacement = "u") %>% 	gsub(pattern = "ç", replacement = "c") %>% 	gsub(pattern = "Á", replacement = "A") %>% 	gsub(pattern = "Â", replacement = "A") %>% 	gsub(pattern = "À", replacement = "A") %>% 	gsub(pattern = "Ã", replacement = "A") %>% 	gsub(pattern = "É", replacement = "E") %>% 	gsub(pattern = "Ê", replacement = "E") %>% 	gsub(pattern = "Í", replacement = "I") %>% 	gsub(pattern = "Ó", replacement = "O") %>% 	gsub(pattern = "Ô", replacement = "O") %>% 	gsub(pattern = "Õ", replacement = "O") %>% 	gsub(pattern = "Ú", replacement = "U") %>% gsub(pattern = "Ç", replacement = "C") %>% gsub(pattern = "'", replacement = "")
}
maiusculas <- function(x) {
  gsub(pattern =  "a", replacement =  "A", x) %>%	gsub(pattern =  "b", replacement =  "B") %>%	gsub(pattern =  "c", replacement =  "C") %>%	gsub(pattern =  "d", replacement =  "D") %>%	gsub(pattern =  "e", replacement =  "E") %>%	gsub(pattern =  "f", replacement =  "F") %>%	gsub(pattern =  "g", replacement =  "G") %>%	gsub(pattern =  "h", replacement =  "H") %>%	gsub(pattern =  "i", replacement =  "I") %>%	gsub(pattern =  "j", replacement =  "J") %>%	gsub(pattern =  "l", replacement =  "L") %>%	gsub(pattern =  "m", replacement =  "M") %>%	gsub(pattern =  "n", replacement =  "N") %>%	gsub(pattern =  "o", replacement =  "O") %>%	gsub(pattern =  "p", replacement =  "P") %>%	gsub(pattern =  "q", replacement =  "Q") %>%	gsub(pattern =  "r", replacement =  "R") %>%	gsub(pattern =  "s", replacement =  "S") %>%	gsub(pattern =  "t", replacement =  "T") %>%	gsub(pattern =  "u", replacement = "U") %>%	gsub(pattern =  "v", replacement =  "V") %>%	gsub(pattern =  "x", replacement = "X") %>% gsub(pattern = "y", replacement = "Y")	%>% gsub(pattern = "z", replacement = "Z")
}
#__________________________________________________________________________________________________ ----

#----------carregamento e pré-formatação  ----
movimentacao_bruta <-
  read.csv(
    'C:/Users/humberto.serna/Desktop/CSV_Data/Movimentacao_da_Producao_Bruta_SP_2018_7_5.csv',
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "ANSI"
  )
colnames(movimentacao_bruta) <-
  c(
    "ano",
    "cpfcnpj",
    "titular",
    "processo",
    "mina",
    "uf",
    "s.amb",
    "s.ral",
    "minerio",
    "quantidade_producão_ajuste",
    "quantidade_venda_ajuste",
    "valor_venda_ajuste_minério",
    "quantidade_producão_substância_ajuste",
    "quantidade_venda_substância_ajuste",
    "valor_venda_ajuste_substância"
  ) %>% removeAcentos()

#---------- transformação das variáveis  ----

#_____atribuindo coluna preço ----
movimentacao_bruta$preco <-
  round(
    movimentacao_bruta$valor_venda_ajuste_substancia / movimentacao_bruta$quantidade_venda_substancia_ajuste,
    1
  )

#_____preço médio ----
preco_mediano_S.AMB_ano <-
  select(movimentacao_bruta, everything()) %>%
  group_by(s.amb, ano) %>% summarise(median(preco, na.rm = TRUE))

#_____contando ocorrências para priorizar: Argila, mais numerosa ----
table(movimentacao_bruta$s.amb) %>% data.frame() %>% arrange(desc(Freq))


#----------Argilas_Comuns ----
Argilas_Comuns <- select(movimentacao_bruta, everything()) %>%
  filter(s.amb == "Argilas Comuns" & preco > 0)

#_____PLOT preço-ano  ----
preco_mediano_S.AMB_ano[preco_mediano_S.AMB_ano$s.amb == "Argilas Comuns", 3]$`median(preco, na.rm = TRUE)` %>% 
  plot(x = c(2011:2016), type = 'b')

ano2011 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2011)
ano2012 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2012)
ano2013 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2013)
ano2014 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2014)
ano2015 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2015)
ano2016 <-
  select(Argilas_Comuns, everything()) %>% filter(ano == 2016)


#_____ Boxplot ----
boxplot(
  ano2011$preco,
  ano2012$preco,
  ano2013$preco,
  ano2014$preco,
  ano2015$preco,
  ano2016$preco,
  names = c(2011, 2012, 2013, 2014, 2015, 2016),
  ylim = c(0, 30),
  main = "preço mina, Argilas Comuns"
)


#_____ Função de densidade - procurando pontos de acumulação  ----
plot(density(ano2011[ano2011$preco < 100, ]$preco))
plot(density(ano2012[ano2012$preco < 100, ]$preco))
plot(density(ano2013[ano2013$preco < 100, ]$preco))
plot(density(ano2014[ano2014$preco < 100, ]$preco))
plot(density(ano2015[ano2015$preco < 100, ]$preco))
plot(density(ano2016[ano2016$preco < 100, ]$preco))

#_____outliers 2015 ----
outliers_2015 <-
  inner_join(data.frame(preco = boxplot(ano2015$preco)$out), ano2015, by = 'preco')

# Outliers-preço presentes no 3º quartil da produção bruta
inconsistencia_2015 <-  semi_join(
  outliers_2015,
  select(ano2015, everything()) %>%
    filter(
      quantidade_producao_substancia_ajuste > quantile(ano2015$quantidade_producao_substancia_ajuste)[4]
    ),
  by = 'preco'
)




outliers_2016 <-
  inner_join(data.frame(preco = boxplot(ano2016$preco)$out), ano2016, by = 'preco')

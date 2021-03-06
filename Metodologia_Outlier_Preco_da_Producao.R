rm(list = ls())

#_____________________________________Pacotes & Fun��es____________________________________________ ----
library(dplyr)
library(tidyr)

caractereParaNumero <- function(x) {
  gsub("[.]", "", x) %>% gsub(pattern = " ", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% gsub(pattern = " ", replacement =
                                                                                                                    "") %>% gsub(pattern = "-", replacement = "0") %>% gsub(pattern = ",", replacement = ".") %>% as.numeric()
}

removeAcentos <- function(x) {
  gsub(pattern = "�", replacement = "a", x) %>% gsub(pattern = "�", replacement = "a") %>% 	gsub(pattern = "�", replacement = "a") %>% 	gsub(pattern = "�", replacement = "a") %>% 	gsub(pattern = "�", replacement = "e") %>% 	gsub(pattern = "�", replacement = "e") %>% 	gsub(pattern = "�", replacement = "i") %>% 	gsub(pattern = "�", replacement = "o") %>% 	gsub(pattern = "�", replacement = "o") %>% 	gsub(pattern = "�", replacement = "o") %>% 	gsub(pattern = "�", replacement = "u") %>% 	gsub(pattern = "�", replacement = "c") %>% 	gsub(pattern = "�", replacement = "A") %>% 	gsub(pattern = "�", replacement = "A") %>% 	gsub(pattern = "�", replacement = "A") %>% 	gsub(pattern = "�", replacement = "A") %>% 	gsub(pattern = "�", replacement = "E") %>% 	gsub(pattern = "�", replacement = "E") %>% 	gsub(pattern = "�", replacement = "I") %>% 	gsub(pattern = "�", replacement = "O") %>% 	gsub(pattern = "�", replacement = "O") %>% 	gsub(pattern = "�", replacement = "O") %>% 	gsub(pattern = "�", replacement = "U") %>% gsub(pattern = "�", replacement = "C") %>% gsub(pattern = "'", replacement = "")
}
maiusculas <- function(x) {
  gsub(pattern =  "a", replacement =  "A", x) %>%	gsub(pattern =  "b", replacement =  "B") %>%	gsub(pattern =  "c", replacement =  "C") %>%	gsub(pattern =  "d", replacement =  "D") %>%	gsub(pattern =  "e", replacement =  "E") %>%	gsub(pattern =  "f", replacement =  "F") %>%	gsub(pattern =  "g", replacement =  "G") %>%	gsub(pattern =  "h", replacement =  "H") %>%	gsub(pattern =  "i", replacement =  "I") %>%	gsub(pattern =  "j", replacement =  "J") %>%	gsub(pattern =  "l", replacement =  "L") %>%	gsub(pattern =  "m", replacement =  "M") %>%	gsub(pattern =  "n", replacement =  "N") %>%	gsub(pattern =  "o", replacement =  "O") %>%	gsub(pattern =  "p", replacement =  "P") %>%	gsub(pattern =  "q", replacement =  "Q") %>%	gsub(pattern =  "r", replacement =  "R") %>%	gsub(pattern =  "s", replacement =  "S") %>%	gsub(pattern =  "t", replacement =  "T") %>%	gsub(pattern =  "u", replacement = "U") %>%	gsub(pattern =  "v", replacement =  "V") %>%	gsub(pattern =  "x", replacement = "X") %>% gsub(pattern = "y", replacement = "Y")	%>% gsub(pattern = "z", replacement = "Z")
}
#__________________________________________________________________________________________________ ----

#----------carregamento e pr�-formata��o  ----
movimentacao_bruta <-
  read.csv(
    'D:/Users/humberto.serna/Desktop/CSV_Data/Movimentacao_da_Producao_Bruta_SP_2018_7_5.csv',
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
    "quantidade_produc�o_ajuste",
    "quantidade_venda_ajuste",
    "valor_venda_ajuste_min�rio",
    "quantidade_produc�o_subst�ncia_ajuste",
    "quantidade_venda_subst�ncia_ajuste",
    "valor_venda_ajuste_subst�ncia"
  ) %>% removeAcentos()

#---------- transforma��o das vari�veis  ----

#_____atribuindo coluna pre�o ----
movimentacao_bruta$preco <-
  round(
    movimentacao_bruta$valor_venda_ajuste_substancia / movimentacao_bruta$quantidade_venda_substancia_ajuste,
    1
  )

#_____pre�o m�dio ----
preco_mediano_S.AMB_ano <-
  select(movimentacao_bruta, everything()) %>%
  group_by(s.amb, ano) %>% summarise(median(preco, na.rm = TRUE))

#_____contando ocorr�ncias para priorizar: Argila, mais numerosa ----
table(movimentacao_bruta$s.amb) %>% data.frame() %>% arrange(desc(Freq))


#----------Argilas_Comuns ----
Argilas_Comuns <- select(movimentacao_bruta, everything()) %>%
  filter(s.amb == "Argilas Comuns" & preco > 0)

#_____PLOT pre�o-ano  ----
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
  main = "pre�o mina, Argilas Comuns"
)


#_____ Fun��o de densidade - procurando pontos de acumula��o  ----
plot(density(ano2011[ano2011$preco < 100, ]$preco))
plot(density(ano2012[ano2012$preco < 100, ]$preco))
plot(density(ano2013[ano2013$preco < 100, ]$preco))
plot(density(ano2014[ano2014$preco < 100, ]$preco))
plot(density(ano2015[ano2015$preco < 100, ]$preco))
plot(density(ano2016[ano2016$preco < 100, ]$preco))

#_____outliers 2015 ----
outliers_2015 <-
  inner_join(data.frame(preco = boxplot(ano2015$preco)$out), ano2015, by = 'preco')

# Outliers-pre�o presentes na produ��o bruta
inconsistencia_2015 <- semi_join(
  outliers_2015,
  select(ano2015, everything()) %>%
    filter(
      quantidade_producao_substancia_ajuste > quantile(ano2015$quantidade_producao_substancia_ajuste)[4]
    ),
  by = 'preco'
)




outliers_2016 <-
  inner_join(data.frame(preco = boxplot(ano2016$preco)$out), ano2016, by = 'preco')

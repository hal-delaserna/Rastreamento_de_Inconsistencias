#     rm(list = ls()) 

# carregamento ----
source(file = "./carregamento_Bases_AMB_outras.r")
#_______________________________________________________________________________________________----

# CARREGANDO GEOCOD ----

geocod <- 
  read.table(
    "D:/Users/humberto.serna/Desktop/CSV_Data/geocod.csv",
    header = TRUE,
    sep = ";",
    quote = "",
    stringsAsFactors = FALSE
  )[, c(1, 2)][, c(1, as.integer(2))]

geocod[,1] <- FUNA_minusculas(FUNA_removeAcentos(geocod[,1]))


colnames(geocod) <- c("municipio","geocod")

geocod_processo <- 
  spread(left_join(
    unique(reserva_AMB[!reserva_AMB$substancia.amb %in% c("areia", "saibro", "brita e cascalho"), c("processo", "ano", "municipio")]),
    geocod,
    by = "municipio"),key = "ano",value = "geocod")

# removendo ponto e zeros a esquerda de processos. Para compatibilidade com SIGMINE
geocod_processo$processo_sigmine <-
  geocod_processo$processo %>% gsub(pattern = "\\.", replacement = "") %>%
  gsub(pattern = "^0", replacement = "") %>% gsub(pattern = "^0", replacement = "") %>% gsub(pattern = "^0", replacement = "")



# onde a série acusa mais de 1 município
inconsistencia_municipio <-
  left_join(filter(data.frame(
    table(geocod_processo$processo), stringsAsFactors = FALSE
  ), Freq > 1),
  geocod_processo,
  by = c("Var1" = "processo"))

colnames(inconsistencia_municipio)[1] <- c("processo")


# CARREGANDO INTERSECÇÃO MUNICIPÍOS E POLIGONAIS ----

interseccao_poligonais_ibge <-
  read.table(
    "D:/Users/humberto.serna/Desktop/CSV_Data/interseccao_poligonais_ibge2.csv",
    encoding = "UTF-8",header = TRUE,sep = ";", quote = "", stringsAsFactors = FALSE) 

colnames(interseccao_poligonais_ibge) <- 
  c("municipio", "geocod", "processo", "area", "fase", "titular", 
    "substancia.ral", "uso", "uf", "areaDeg2", "perimetro")



  # coluna de Área da poligonal em graus radianos²
interseccao_poligonais_ibge <-
  left_join(interseccao_poligonais_ibge,
            summarise(
              group_by(interseccao_poligonais_ibge, processo),
              "PoligonalDeg2" = sum(areaDeg2)), by = "processo")




geocod_AMB.inconsistencia_SIGMINE <-
  inner_join(
    inconsistencia_municipio,
    interseccao_poligonais_ibge[, c(1:4, 6,10)],
    by = c("processo_sigmine" = "processo"))


colnames(geocod_AMB.inconsistencia_SIGMINE) <- c("processo", "Freq", "municipio.AMB", "2011", "2012", "2013", 
                             "2014", "2015", "2016", "2017", "2018", "processo_sigmine", "municipio.sigmine", 
                             "geocod", "area", "titular", "areaDeg2")









#     rm(list = ls()) 

# carregamento ----
source(file = "./Funcoes_R/FUNA_Eventos_RRR_RFP.R")
source(file = "./Funcoes_R/Funcoes_Consumidores.R")
source(file = "./Funcoes_R/Funcoes_de_Formatacao_Estilo.R")
source(file = "./Funcoes_R/Funcoes_Producao.R")
source(file = "./Funcoes_R/Funcoes_Reserva.R")
source(file = "./Funcoes_R/Abatimento_Reserva_Producao.R")
source(file = "./Funcoes_R/Funcoes_VPM.R")
source(file = "./Funcoes_R/Municipios_inconsistencia.R")
source(file = "./carregamento_Bases_AMB_outras.r")
#_______________________________________________________________________________________________


#---------------------INCONSISTÊNCIA MUNICIPIOS--------------------------------------

# PROCESSOS COM SIGMINE = 1 & AMB >= 2 MUNICÍPIOS
inner_join(
  filter(data.frame(
    table(interseccao_poligonais_ibge$processo)
  ), Freq == 1),
  geocod_AMB.inconsistencia_SIGMINE,
  by = c("Var1" = "processo_sigmine")
)[, c(-1, -2, -4, -15, -16, -18)] %>% View()



#_____________ Preparando lista de inconsistência AMB X Sigmine
   # AMB: processo <-> município (2011 a 2018)
processo.AMB_municipio <- 
  unique(left_join(unique(
    reserva_AMB[!reserva_AMB$substancia.amb %in%
                   c("areia", "saibro", "brita e cascalho"),
                   c("processo", "municipio")]),
                   geocod, by = c("municipio")))


colnames(processo.AMB_municipio) <- c("processo.AMB","municipio.AMB", "geocod.AMB")


    # Formato SigMine de Processo: sem 'ponto' ou zeros a esquerda
processo.AMB_municipio$processo_sigmine.AMB <-  processo.AMB_municipio$processo %>% 
  gsub(pattern = "\\.", replacement = "") %>% gsub(pattern = "^0", replacement = "") %>% 
  gsub(pattern = "^0", replacement = "") %>% gsub(pattern = "^0", replacement = "")


# Inconsistência AMB X Sigmine
inconsistencia_AMB_X_Sigmine <- 
  left_join(
    processo.AMB_municipio,
    interseccao_poligonais_ibge,
    by = c("processo_sigmine.AMB" = "processo")) 

 colnames(inconsistencia_AMB_X_Sigmine) <- 
  c("processo.AMB", "municipio.AMB", "geocod.AMB", "processo_sigmine.AMB", 
    "municipio.sigmine", "geocod.sigmine", "area_ha", "fase", "titular", "substancia.ral", 
    "uso", "uf", "areaDeg2", "perimetro", "poligonalDeg2")

  
 inconsistencia_AMB_X_Sigmine <- 
   inconsistencia_AMB_X_Sigmine[,c("processo.AMB", "municipio.AMB", 
                                   "geocod.AMB", "processo_sigmine.AMB", 
                                   "municipio.sigmine", "geocod.sigmine",
                                   "fase", "titular", "substancia.ral", 
                                   "uso", "area_ha", "areaDeg2", 'poligonalDeg2')]
 # processos com NA são areia e brita
 lista <- list()
  for (i in 1:nrow(inconsistencia_AMB_X_Sigmine)) {
   if (is.na(inconsistencia_AMB_X_Sigmine$geocod.sigmine[i]) == FALSE) { 
     if ((
       inconsistencia_AMB_X_Sigmine$geocod.AMB[i] != inconsistencia_AMB_X_Sigmine$geocod.sigmine[i]
     )) {
       lista[[i]] <-
         inconsistencia_AMB_X_Sigmine[i, c(
           "processo.AMB", "municipio.AMB","geocod.AMB",
           "municipio.sigmine","geocod.sigmine","fase","titular",
           "substancia.ral", "area_ha", "areaDeg2", 'poligonalDeg2')]}}}

 inconsistencia_AMB_X_Sigmine <-
   arrange(do.call("rbind", lista), desc(titular))
 
  inconsistencia_AMB_X_Sigmine$razaoArea <- round(
   (inconsistencia_AMB_X_Sigmine$areaDeg2 / inconsistencia_AMB_X_Sigmine$poligonalDeg2), digits = 2)
  inconsistencia_AMB_X_Sigmine$segmento_poligonal <- round(
   (inconsistencia_AMB_X_Sigmine$area_ha * inconsistencia_AMB_X_Sigmine$razaoArea), digits = 2)
 
 
 # delimitando segmento de poligonal > 25%
 inconsistencia_AMB_X_Sigmine <-
   inconsistencia_AMB_X_Sigmine[inconsistencia_AMB_X_Sigmine$razaoArea > 0.75,]
 
 inconsistencia_AMB_X_Sigmine <-
    left_join(inconsistencia_AMB_X_Sigmine,
              reserva_AMB[reserva_AMB$ano == 2018, c("processo", "pareto")],
              by = c("processo.AMB" = "processo"))
 
 
   View(inconsistencia_AMB_X_Sigmine[,-c(3,5,10,11)])
   
   
   
   # Anfibólito OK
   # Bauxita OK
   # Min de Aluminío OK
   # Caulim OK
   
   
   
   
   # Reserva visão ----
   mina <- 'SÃO JOÃO DA BOA VISTA'
   cpfcnpj = '.'
   subsAMB <- '.'
   processo <- '.'
   
   FUNA_visao_RESERVA(subsAMB = subsAMB, processo = processo, cpfcnpj = cpfcnpj, mina = mina)
  
   # medida + indicada  + Inferida + lavrável ----
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'medida')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'indicada')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'inferida')
   reserva_groupBY_SUBSTANCIA.AMB(processo = processo, mina = mina, reserva = 'lavravel')
   
   
     # Produção ------------------------------------------------------------------
   
   producaoBRUTA_groupBY_PROCESSO(cpfcnpj = cpfcnpj, subsAMB = "")
  
   
   # Consumidores  ------------------------------------------------------------------
  
   consumidoresMINA_busca(cpfcnpj = cpfcnpj)
  
   consumidoresUSINA[consumidoresUSINA$cpfcnpj == cpfcnpj,] %>% View()
  
   #--------------------- Abatimento - Produção Rerserva --------------------------------------
   
   
   processo <- '.'
   mina <- '.'
   cpfcnpj <- '.'
   subsAMB <- '.'
   
   FUNA_Abatimento_Reserva_Producao(processo = processo,
                                    subsAMB = subsAMB,
                                    mina = mina,
                                    cpfcnpj = cpfcnpj,
                                    ano1 = '2011')
  

#--------------------- tela livre  --------------------------------------

# ARDÓSIA: OK!

reserva_groupBY_SUBSTANCIA.AMB()[1,] %>% FUNA_BARPLOT()

FUNA_visao_RESERVA(subsAMB = 'ardosia')

FUNA_visao_RESERVA(processo = "820.276/1979", subsAMB = ".")



reserva_groupBY_SUBSTANCIA.AMB(processo = "821.014/1995")









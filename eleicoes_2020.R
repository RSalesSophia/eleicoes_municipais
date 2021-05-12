#Pacotes necessarios
library(dplyr)
library(readxl)
library(stringi)
library(magrittr)
library(stringr)
library(data.table)
library(lubridate)
library(oaxaca)
library(fastDummies)
library(ggplot2)

##Obs: para as eleicoees de 2020 e necessario baixar todas as bases de dados
#no site do TSE

#Configuracao
options(scipen=999)

#Extraindo a quantidade de votos dos candidatos(as)
votacao <- read.csv2("votacao_candidato_munzona_2020_BRASIL.csv")

#Extraindo as variaveis necessarias 
vot_ <- votacao %>%
  select(SG_UF, DS_CARGO, DS_DETALHE_SITUACAO_CAND,QT_VOTOS_NOMINAIS,
         SQ_CANDIDATO) %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO", DS_CARGO == "Vereador" ) %>%
  select(-DS_DETALHE_SITUACAO_CAND, -DS_CARGO) 

#Criando uma chave identificadora para os candidatos
vot_$chave <- paste0(vot_$SQ_CANDIDATO, vot_$SG_UF)

#Agrupando o total de votos recebido por um candidato (identificado pela chave) no munic??pio
vot_  %<>%
  group_by(chave) %>%
  summarise(TOTAL_VOTOS = sum (QT_VOTOS_NOMINAIS))

###################################################################################################
#Extraindo a base que tem informa??es sobre os candidatos 

informacao <- read.csv2("consulta_cand_2020_BRASIL.csv")

inf_ <- informacao %>%
  select(SG_UE, SG_UF, NM_UE, SG_PARTIDO,
         DS_OCUPACAO, NR_IDADE_DATA_POSSE,DS_GENERO,DS_GRAU_INSTRUCAO, 
         DS_ESTADO_CIVIL, SQ_CANDIDATO, DS_COR_RACA) 

#Criando uma chave identificadora para os candidatos
inf_$chave <- paste0(inf_$SQ_CANDIDATO, inf_$SG_UF)

###################################################################################################
#Unindo as bases vot_ e inf_ por merge: 
base_cand <- inner_join(vot_, inf_, by = "chave")

#Tendo em vista a literatura, optou por excluir da amostra os candidatos que estao tentando
#reeleicao, isto ?, aqueles que ocupam o cargo de vereador naquele ano.

base_cand  %<>%
  filter(DS_OCUPACAO!= "VEREADOR")

#Baixando a base de dados que tem informacoees sobre o IDHM 

idhm <- read_excel("Atlas 2013_municipal, estadual e Brasil.xlsx", sheet = "MUN 91-00-10")

idhm_10 <- idhm %>%
  filter(ANO == 2010) %>%
  select(IDHM, Codmun7) %>%
  rename(codigo_ibge = Codmun7)

#Como a base idhm utiliza o codigo do ibge para identificar municipio e o tse usa outra codigo
#foi necess?rio usar uma base que tem a liga??oo dos dois codigos 

cod <- fread("codigo_tse_ibge.txt", sep = ",", h = T)

#Selecionando apenas as colunas necessarias 

cod %<>% select (codigo_tse, codigo_ibge)

#Unindo as duas bases de dados
idhm_10 <- inner_join(idhm_10, cod, by = "codigo_ibge")

idhm_10$codigo_ibge <- NULL

#Unindo a base idhm com base_cand

base_cand <- inner_join(base_cand, idhm_10,  by =  c("SG_UE" = "codigo_tse"))

#####################################################################################

#No trabalho, iremos utilizar uma variavel intitulada competitividade que corresponde
#a quantidade de vagas dividido pelo numero de candidatos. Para isso, baixa-se a base
#de dados do TSE que tem o numero de vagas a vereador de cada muninicipio, inclusive 
#a competitivadade

num_vagas <- read.csv2("vaga.csv")

num_vagas %<>%
  filter (Cargo == "Vereador") %>%
  select(Concorr?ncia, UF , Munic?pio) %>%
  rename(COMPET = Concorr?ncia)

#Criando uma chave identificadora para conectar com base_cand
num_vagas$distrito <- paste0(num_vagas$UF, num_vagas$Munic?pio)

#Criando a mesma chave em base_cand
base_cand <- inner_join(base_cand, num_vagas, by = "distrito")

#################################################################################
#Exportando a base receitas de campanha 

receitas <- read.csv2("receitas_candidatos_2020_BRASIL.csv", sep = ";")

receitas %<>%
  select(SQ_CANDIDATO, SG_UF, VR_RECEITA) 

#Criando novamente a chave identificadora 
receitas$chave <- paste0(receitas$SQ_CANDIDATO, receitas$SG_UF)

#VR_RECEITA em numerico
receitas$VR_RECEITA <- as.numeric(receitas$VR_RECEITA)

#Somando todos os valores declarados por chave
receitas %<>%
  group_by(chave) %>%
  summarise(VR_RECEITA = sum(VR_RECEITA))

#Unindo a base receitas com base de candidatos 
base_cand <- inner_join(base_cand, receitas, by = "chave")

#Cruzando agora a base de bens dos candidatos com base de dados 
bens <- read.csv2("bem_candidato_2020_BRASIL.csv")

#Criando novamente a chave identificadora 
bens$chave <- paste0(bens$SQ_CANDIDATO, bens$SG_UF)

#Selecionando as variaveis necessarias
bens %<>%
  group_by(chave) %>%
  summarise(VALOR_BENS = sum(VR_BEM_CANDIDATO))

#Unindo atraves do merge a base bens com base_cand
base_cand <- inner_join(base_cand, bens, by = "chave")

#############################################################################################
#Agora que foi feita a uni?o de todas as bases, iremos construir as variaveis que
#serao analisadas no modelo 

#Primeiramente criando dummies de partidos 
base_cand %<>%
  mutate(D_PARTIDO = case_when(SG_PARTIDO == "PCB" ~ "1",
                               SG_PARTIDO == "PCO" ~ "1", 
                               SG_PARTIDO == "PSOL" ~ "1",
                               SG_PARTIDO == "PT" ~ "1",
                               SG_PARTIDO == "PSTU" ~ "1",
                               SG_PARTIDO == "PC do B" ~ "1",
                               SG_PARTIDO == "PDT" ~ "2",
                               SG_PARTIDO == "PPL" ~ "2",
                               SG_PARTIDO == "CIDADANIA" ~ "2",
                               SG_PARTIDO == "PROS" ~ "2",
                               SG_PARTIDO == "PSB" ~ "2",
                               SG_PARTIDO == "DEM" ~ "3",
                               SG_PARTIDO == "DC" ~ "3",
                               SG_PARTIDO == "PHS" ~ "3",
                               SG_PARTIDO == "PMB" ~ "3",
                               SG_PARTIDO == "PSD" ~ "3",
                               SG_PARTIDO == "PSL" ~ "3",
                               SG_PARTIDO == "AVANTE" ~ "3",
                               SG_PARTIDO == "PATRIOTA" ~ "3",
                               SG_PARTIDO == "MDB" ~ "3",
                               SG_PARTIDO == "PRP" ~ "3",
                               SG_PARTIDO == "PSDC" ~ "3",
                               SG_PARTIDO == "PTC" ~ "3",
                               SG_PARTIDO == "PV" ~ "3",
                               SG_PARTIDO == "PODE" ~ "3",
                               SG_PARTIDO == "PSDB" ~ "3",
                               SG_PARTIDO == "NOVO" ~ "4",
                               SG_PARTIDO == "PRB" ~ "4",
                               SG_PARTIDO == "PSC" ~ "4",
                               SG_PARTIDO == "PP" ~ "5",
                               SG_PARTIDO == "REDE" ~ "6",
                               SG_PARTIDO == "PTB" ~ "6",
                               SG_PARTIDO == "PMN" ~ "6",
                               SG_PARTIDO == "PRTB" ~ "6",
                               SG_PARTIDO == "REPUBLICANOS" ~ "6",
                               SG_PARTIDO == "PL" ~ "6",
                               SG_PARTIDO == "SOLIDARIEDADE" ~ "6",
                               SG_PARTIDO == "UP" ~ "6"))

#Criando a variavel coligacao
base_cand %<>%
  mutate(COLIGACAO = if_else(SG_PARTIDO == "PSL"| SG_PARTIDO == "PRTB", 1, 0))

#Definindo os niveis de escolaridade
base_cand %<>% mutate (ESCOL = case_when(DS_GRAU_INSTRUCAO == "L? E ESCREVE" ~ 1,
                                         DS_GRAU_INSTRUCAO == "ENSINO FUNDAMENTAL INCOMPLETO" ~ 2,
                                         DS_GRAU_INSTRUCAO == "ENSINO FUNDAMENTAL COMPLETO" ~ 3,
                                         DS_GRAU_INSTRUCAO == "ENSINO M?DIO INCOMPLETO" ~ 4,
                                         DS_GRAU_INSTRUCAO == "ENSINO M?DIO COMPLETO" ~ 5,
                                         DS_GRAU_INSTRUCAO == "SUPERIOR INCOMPLETO" ~ 6,
                                         DS_GRAU_INSTRUCAO == "SUPERIOR COMPLETO" ~ 7))

#Definindo a situacao conjugal do candidato(a):
base_cand %<>% mutate(CASADO = if_else(DS_ESTADO_CIVIL == "CASADO(A)", 1,0))

#Criando dummy para o caso de ser homem ou mulher:
base_cand %<>% mutate(MULHER = if_else(DS_GENERO == "FEMININO", 1,0))

#Criando dummies de escolaridade e orientacao politica
base_cand <- dummy_cols(base_cand, select_columns = c("ESCOL", "D_PARTIDO"))

#Criando dummies para cor_raca:
base_cand %<>% mutate(BRANCO = if_else(DS_COR_RACA == "BRANCA", 1,0))

#Transformando as variaveis receita, bens e total de votos em ln:
#Por default log corresponde ao logaritmo natural no R

base_cand$ln_votos <- log(base_cand$TOTAL_VOTOS)

base_cand$ln_receita <- log(base_cand$VR_RECEITA)

base_cand$ln_bens <- log(base_cand$VALOR_BENS)

#Tirando da base todas as indeterminacoes criadas ao considerar ln
base_cand %<>% 
  filter_all(all_vars(!is.infinite(.)))

#Baixando novamente a base completa 
write.csv2(base_cand, file = "base_2020.csv", row.names = F)

#Para stata
write.dta(base_cand, file = "base_2020.dta")

#Partindo entao para decomposicao de oaxaca
base_cand <- read.csv2("base_2020.csv")


resultado <- oaxaca(formula = ln_votos ~ CASADO + ln_bens + IDHM + NR_IDADE_DATA_POSSE + ESCOL_2 + ESCOL_3 +
                      ESCOL_4 + ESCOL_5 + ESCOL_6 +ESCOL_7 + D_PARTIDO_2 + D_PARTIDO_3 +
                      D_PARTIDO_4 + D_PARTIDO_5 + D_PARTIDO_6 +  COMPET + ln_receita +
                      COLIGACAO + BRANCO| MULHER , data = base_cand)
print(resultado)
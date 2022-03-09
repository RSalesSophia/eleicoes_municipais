#Pacotes necessarios
library(dplyr)
library(electionsBR)
library(readxl)
library(stringi)
library(magrittr)
library(stringr)
library(data.table)
library(lubridate)
library(oaxaca)
library(fastDummies)
library(ggplot2)
library(foreign)


#Configuração
options(scipen=999)

#Extraindo a quantidade de votos dos candidatos(as)
votacao <- vote_mun_zone_local(2012)

votacao$chave <- paste0(votacao$SIGLA_UF, votacao$SQ_CANDIDATO)

vot_ <- votacao %>%
  select(SQ_CANDIDATO, DESCRICAO_CARGO, DESC_SIT_CANDIDATO,
         TOTAL_VOTOS, SIGLA_UF) %>%
  filter(DESC_SIT_CANDIDATO == "DEFERIDO", DESCRICAO_CARGO == "VEREADOR" ) %>%
  select(-DESC_SIT_CANDIDATO, -DESCRICAO_CARGO) 

#Criando uma chave identificadora para os candidatos
vot_$chave <- paste0(vot_$SQ_CANDIDATO, vot_$SIGLA_UF)

#Agrupando o total de votos recebido por um candidato (identificado pela chave) no município
vot_  %<>%
  group_by(chave) %>%
  summarise(TOTAL_VOTOS = sum (TOTAL_VOTOS))

###################################################################################################
informacao <- candidate_local(2012)

inf_ <- informacao %>%
  select(SIGLA_UF, SIGLA_UE, DESCRICAO_CARGO, SIGLA_PARTIDO, DES_SITUACAO_CANDIDATURA,
         DESCRICAO_OCUPACAO, DATA_NASCIMENTO, DESCRICAO_SEXO, DESCRICAO_GRAU_INSTRUCAO, 
         DESCRICAO_ESTADO_CIVIL, SEQUENCIAL_CANDIDATO) %>%
  filter(DESCRICAO_CARGO == "VEREADOR", DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>%
  select(-DESCRICAO_CARGO, -DES_SITUACAO_CANDIDATURA)

inf_$chave <- paste0(inf_$SEQUENCIAL_CANDIDATO, inf_$SIGLA_UF)

###################################################################################################
#Unindo as bases vot_ e inf_ por merge: 

base_cand <- inner_join(vot_, inf_, by = "chave")

#Tendo em vista a literatura, optou por excluir da amostra os candidatos que estão tentando
#reeleição, isto é, aqueles que ocupam o cargo de vereador naquele ano.

base_cand  %<>%
  filter(DESCRICAO_OCUPACAO != "VEREADOR")

#Baixando a base de dados que tem informações sobre o IDHM 

idhm <- read_excel("Atlas 2013_municipal, estadual e Brasil.xlsx", sheet = "MUN 91-00-10")

idhm_10 <- idhm %>%
  filter(ANO == 2010) %>%
  select(IDHM, Codmun7) %>%
  rename(codigo_ibge = Codmun7)

#Como a base idhm utiliza o codigo do ibge para identificar municipio e o tse usa outra codigo
#foi necessário usar uma base que tem a ligação dos dois codigos 

cod <- fread("codigo_tse_ibge.txt", sep = ",", h = T)

#Na base do tse o codigo sempre tem cinco numeros, enquanto nessa ? omitido os zero a esquerda
#dessa forma necessita-se preencher com zero o codigo do tse 
cod$codigo_tse <- str_pad(cod$codigo_tse, 5, pad = "0")

#Selecionando apenas as colunas necessarias 

cod %<>% select (codigo_tse, codigo_ibge)

#Unindo as duas bases de dados
idhm_10 <- inner_join(idhm_10, cod, by = "codigo_ibge")

idhm_10$codigo_ibge <- NULL

#Unindo a base idhm com base_cand

base_cand <- inner_join(base_cand, idhm_10,  by =  c("SIGLA_UE" = "codigo_tse"))

#####################################################################################

#No trabalho, iremos utilizar uma variavel intitulada competitividade que corresponde
#a quantidade de vagas dividido pelo numero de candidatos. Para isso, baixa-se a base
#de dados do TSE que tem o numero de vagas a vereador de cada muninicipio

num_vagas <- seats_local(2012)

num_vagas %<>%
  filter (DESCRICAO_CARGO == "VEREADOR") %>%
  select(SIGLA_UE, QTDE_VAGAS)

#Fazendo merge num_vagas + base_cand:

base_cand <- inner_join(base_cand, num_vagas, by = "SIGLA_UE")

#Contando quantos candidatos por municipio tem e criando a variavel competitividade (compet): 
base_cand %<>%
  group_by(SIGLA_UE) %>%
  mutate(TOTAL_CAND = n(),
         COMPET = TOTAL_CAND/QTDE_VAGAS) 

#################################################################################
#Exportando a base receitas de campanha 

receitas <- read.csv2("receitas_candidatos_2012_brasil.txt", sep = ";")

#Criando novamente a chave identificadora 
receitas$chave <- paste0(receitas$Sequencial.Candidato, receitas$UF)

#Selecionando as variaveis e filtrando a base
receitas %<>%
  select(chave, Valor.receita, Cargo) %>%
  filter(Cargo == "Vereador")

#Somando todos os valores declarados por chave
receitas %<>%
  group_by(chave) %>%
  summarise(VR_RECEITA = sum(Valor.receita))

#Unindo a base receitas com base de candidatos 
base_cand <- inner_join(base_cand, receitas, by = "chave")


#Cruzando agora a base de bens dos candidatos com base de dados 
#Novamente pelo pacote electionsBR baixando os dados 

bens <- personal_finances_local(2012)

#Como ? observado essa base de dados esta com varias colunas trocadas
#Ent?o com base do que ? observado em outras bases, foi identificado 
#as variaveis que iremos trabalhar

#Criando a chave identificador 

#COD-ELEICAOO = SQ_CANDIDATO
#NOME_TIPO_ELEICAO = SIGLA_UF
#SIGLA_UE = VALOR_BENS

bens$chave <- paste0(bens$COD_ELEICAO, bens$NOME_TIPO_ELEICAO)

bens_ <- bens %>% 
  group_by(chave) %>%
  summarise(VALOR_BENS = sum(SIGLA_UE))

#Unindo atraves do merge a base bens com base_cand
base_cand <- inner_join(base_cand, bens_, by = "chave")

#############################################################################################
#Agora que foi feita a uni?o de todas as bases, iremos construir as variaveis que
#serao analisadas no modelo 

#Primeiramente criando dummies de partidos 
base_cand %<>% 
  mutate(D_PARTIDO = case_when(SIGLA_PARTIDO == "PCB" ~ "1",
                               SIGLA_PARTIDO == "PCO" ~ "1", 
                               SIGLA_PARTIDO == "PSOL" ~ "1",
                               SIGLA_PARTIDO == "PT" ~ "1",
                               SIGLA_PARTIDO == "PSTU" ~ "1",
                               SIGLA_PARTIDO == "PC do B" ~ "1",
                               SIGLA_PARTIDO == "PDT" ~ "2",
                               SIGLA_PARTIDO == "PPL" ~ "2",
                               SIGLA_PARTIDO == "PPS" ~ "2",
                               SIGLA_PARTIDO == "PSB" ~ "2",
                               SIGLA_PARTIDO == "DEM" ~ "3",
                               SIGLA_PARTIDO == "DC" ~ "3",
                               SIGLA_PARTIDO == "PHS" ~ "3",
                               SIGLA_PARTIDO == "PMB" ~ "3",
                               SIGLA_PARTIDO == "PSD" ~ "3",
                               SIGLA_PARTIDO == "PSL" ~ "3",
                               SIGLA_PARTIDO == "PT do B" ~ "3",
                               SIGLA_PARTIDO == "PATRI" ~ "3",
                               SIGLA_PARTIDO == "PMDB" ~ "3",
                               SIGLA_PARTIDO == "PRP" ~ "3",
                               SIGLA_PARTIDO == "PSDC" ~ "3",
                               SIGLA_PARTIDO == "PTC" ~ "3",
                               SIGLA_PARTIDO == "PV" ~ "3",
                               SIGLA_PARTIDO == "PTN" ~ "3",
                               SIGLA_PARTIDO == "PSDB" ~ "3",
                               SIGLA_PARTIDO == "PRB" ~ "4",
                               SIGLA_PARTIDO == "PSC" ~ "4",
                               SIGLA_PARTIDO == "PP" ~ "5",
                               SIGLA_PARTIDO == "PTB" ~ "6", 
                               SIGLA_PARTIDO == "PMN" ~ "6",
                               SIGLA_PARTIDO == "PRTB" ~ "6", 
                               SIGLA_PARTIDO == "PR" ~ "6"))

#Descobrindo a idade dos candidatos 
base_cand <- base_cand %>% 
  rename(IDADE = DATA_NASCIMENTO)

#Data da eleic?o
base_cand$dia_e <- ("2012-10-07")

#Transformando a coluna idade no formato de data 
base_cand$IDADE <- dmy(base_cand$IDADE)

#Extraindo tudo que ? diferente de NA
base_cand <- base_cand[!is.na(base_cand$IDADE),]

#Subtraindo a data de elei??o - data de nascimento
base_cand$dif_IDADE <- as.Date(as.character(base_cand$dia_e), format = "%Y-%m-%d")- as.Date(as.character(base_cand$IDADE),
                                                                                            format="%Y-%m-%d")
#Dividindo por 365 dias para encontrar os anos 
base_cand$dif_IDADE <- (base_cand$dif_IDADE/365)

#Excluindo e renomeando a coluna 
base_cand$IDADE <- NULL
base_cand$dia_e <- NULL

base_cand <- base_cand %>%
  rename(IDADE = dif_IDADE)

#Transformando a coluna IDADE em numerico 
base_cand$IDADE <- as.numeric(base_cand$IDADE)

#Arredondando a idade 
base_cand$IDADE<- round(base_cand$IDADE,digits=3)

#Feito a variavel idade, agora criando a variavel coligacao 
base_cand %<>%
  mutate(COLIGACAO = if_else(SIGLA_PARTIDO == "PT"|SIGLA_PARTIDO == "PMDB"| SIGLA_PARTIDO == "PDT"| SIGLA_PARTIDO == "PDT"|
                               SIGLA_PARTIDO == "PDT"| SIGLA_PARTIDO == "PSB"|SIGLA_PARTIDO == "PR"|SIGLA_PARTIDO == "PCdoB"|
                               SIGLA_PARTIDO == "PRB"| SIGLA_PARTIDO == "PRB"|SIGLA_PARTIDO == "PTN"|SIGLA_PARTIDO == "PSC"|
                               SIGLA_PARTIDO == "PTC", 1, 0))

#Definindo os niveis de escolaridade

#Optou por unir analfabeto + le e escreve: 
base_cand %<>% mutate (ESCOL = case_when(DESCRICAO_GRAU_INSTRUCAO == "ANALFABETO" | DESCRICAO_GRAU_INSTRUCAO == "LÊ E ESCREVE" ~ 1,
                                         DESCRICAO_GRAU_INSTRUCAO == "ENSINO FUNDAMENTAL INCOMPLETO" ~ 2,
                                         DESCRICAO_GRAU_INSTRUCAO == "ENSINO FUNDAMENTAL COMPLETO" ~ 3,
                                         DESCRICAO_GRAU_INSTRUCAO == "ENSINO MÉDIO INCOMPLETO" ~ 4,
                                         DESCRICAO_GRAU_INSTRUCAO == "ENSINO MÉDIO COMPLETO" ~ 5,
                                         DESCRICAO_GRAU_INSTRUCAO == "SUPERIOR INCOMPLETO" ~ 6,
                                         DESCRICAO_GRAU_INSTRUCAO == "SUPERIOR COMPLETO" ~ 7))

#Definindo a situacao conjugal do candidato(a):
base_cand %<>% mutate(CASADO = if_else(DESCRICAO_ESTADO_CIVIL == "CASADO(A)", 1,0))

#Criando dummy para o caso de ser homem ou mulher:
base_cand %<>% mutate(MULHER = if_else(DESCRICAO_SEXO == "FEMININO", 1,0))

#Criando dummies de escolaridade e orientacao politica
base_cand <- dummy_cols(base_cand, select_columns = c("ESCOL", "D_PARTIDO"))

#Transformando as variaveis receita, bens e total de votos em ln:
#Por default log corresponde ao logaritmo natural no R

base_cand$ln_votos <- log(base_cand$TOTAL_VOTOS)

base_cand$ln_receita <- log(base_cand$VR_RECEITA)

base_cand$ln_bens <- log(base_cand$VALOR_BENS)

#Tirando da base todas as indetermina??es criadas ao considerar ln
base_cand %<>% 
  filter_all(all_vars(!is.infinite(.)))

#Baixando novamente a base completa 
write.csv2(base_cand, file = "base_2012.csv", row.names = F)

#Para stata
write.dta(base_cand, file = "base_2012.dta")

#Partindo entao para decomposicao de oaxaca
base_cand <- read.csv2("base_2012.csv")

resultado <- oaxaca(formula = ln_votos ~ CASADO + ln_bens + IDHM + IDADE + ESCOL_2 + ESCOL_3 +
                      ESCOL_4 + ESCOL_5 + ESCOL_6 +ESCOL_7 + D_PARTIDO_2 + D_PARTIDO_3 +
                      D_PARTIDO_4 + D_PARTIDO_5 + D_PARTIDO_6 +  COMPET + ln_receita +
                      COLIGACAO | MULHER , data = base_cand, R = 100)

print(resultado)


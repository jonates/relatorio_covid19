library(dplyr)
library(lubridate)
library(stringr)
library(tidyselect)
library(magrittr)
library(readxl)
library(data.table)

#------------------------------------------------------------------------------
# Microdados de notificcao
#------------------------------------------------------------------------------
########### Leitura dos microdados 
md_cases <- read_xlsx("./data/Banco_Estadual_COVID-19_20-07-2020.xlsx", sheet = 1)

########## Extraindo Datas

md_cases %<>% 
  as.data.frame() %>% 
  mutate(
    DATA_DA_NOTIFICACAO  = dmy(`DATA DA NOTIFICACAO`),
    DATA_DO_INICIO_DOS_SINTOMAS = dmy(`DATA DO INICIO DOS SINTOMAS`),
    DATA_DA_COLETA_DO_TESTE = dmy(`DATA DA COLETA DO TESTE`)
    )%>%
  mutate(DATA_REF = DATA_DO_INICIO_DOS_SINTOMAS) %>%
  select(
    DATA_REF,
    DATA_DO_INICIO_DOS_SINTOMAS,
    DATA_DA_NOTIFICACAO,
    DATA_DA_COLETA_DO_TESTE,
    everything()
  )

#SUBSTITINDO DATA DE REFERENCA FALTANTE POR DATA DA COLETA
md_cases$DATA_REF[is.na(md_cases$DATA_REF)] <- 
    md_cases[is.na(md_cases$DATA_REF),"DATA_DA_COLETA_DO_TESTE"]

#SUBSTITINDO DATA DE REFERENCIA FALTANTE POR DATA DE NOTIFICACAO
md_cases$DATA_REF[is.na(md_cases$DATA_REF)] <- 
  md_cases[is.na(md_cases$DATA_REF),"DATA_DA_NOTIFICACAO"]

# recodificando a classificacao final
md_cases %<>% mutate(
  CLASSIFICACAO_FINAL = `CLASSIFICACAO FINAL` %>%
    str_replace("CONFIRMADO CLINICO-EPIDEMIOLOGICO", "CONFIRMADO") %>%
    str_replace("CONFIRMADO CLINICO-IMAGEM", "CONFIRMADO") %>%
    str_replace("CONFIRMADO LABORATORIALMENTE", "CONFIRMADO") %>%
    str_replace("CONFIRMADO SOROLOGIA", "CONFIRMADO") %>%
    str_replace("CONFIRMADO TESTE RAPIDO", "CONFIRMADO") %>%
    str_replace("DESCARTADO", "DESCARTADO") %>%
    str_replace("DESCARTADO LABORATORIALMENTE", "DESCARTADO") %>%
    str_replace("DESCARTADO TESTE RAPIDO","DESCARTADO") %>%
    str_replace("SUSPEITO", "SUSPEITO" )
)

# filtrando somente os confirmados
md_confirmados <- md_cases %>% filter(CLASSIFICACAO_FINAL =="CONFIRMADO")

#Gerando série historica dos confirmados
serie_casos_BA <- 
  md_confirmados %>%
    group_by(DATA_REF) %>%
    summarise(casos_novos = n()) %>% 
    as.data.frame() %>% 
    mutate(
      mm_7d_direita = frollmean(casos_novos , n=7, align = "right") %>% as.numeric(),
      mm_7d_centro = frollmean(casos_novos , n=7, align = "center") %>% as.numeric(),
      diagnostico = "COVID-19"
    )  

#Inserindo NA na media moveis do registro sem data   
serie_casos_BA[is.na(serie_casos_BA$DATA_REF),"mm_7d_direita"] <- 100
serie_casos_BA[is.na(serie_casos_BA$DATA_REF),"mm_7d_centro"] <- 100

#salvando série histórica
#write.csv2(serie_casos_BA,"./data/serie_casos_microdados_BA.csv", row.names = F)

#------------------------------------------------------------------------------
# Microdados de obitos
#------------------------------------------------------------------------------

########### Leitura dos microdados 
md_obitos <- read_xlsx("./data/Banco_Estadual_COVID-19_20-07-2020.xlsx", sheet = 2)
View(md_obitos)
names(md_obitos)

# Transformando data
md_obitos %<>% 
  as.data.frame() %>% 
  mutate(
    DATA_OBITO  = as_date(`DATA OBITO`),
  )

#Gerando série historica dos obitos
serie_obitos_BA <- 
  md_obitos %>%
  group_by(DATA_OBITO) %>%
  summarise(obitos = n()) %>% 
  as.data.frame() %>% 
  mutate(
    mm_7d_direita = frollmean(obitos , n=7, align = "right") %>% as.numeric(),
    mm_7d_centro = frollmean(obitos , n=7, align = "center") %>% as.numeric()
  )  

#salvando série histórica
#write.csv2(serie_obitos_BA,"./data/serie_obitos_microdados_BA.csv", row.names = F)

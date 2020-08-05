################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        ETL microdados covid-19 da BAHIA. Fonte: SESAB
#####   DATA DA CRIAÇÃO:  20/07/2020
#####   ESCRITO POR:      Jonatas Silva
#####   COOPERACAO:       Cleiton Rocha 
#####   SITE:             "https://github.com/jonates"
#####   LICENÇA:          GPLv3
#####   PROJETO:          https://github.com/jonates/report_covid19_BA


library(dplyr)
library(lubridate)
library(stringr)
library(tidyselect)
library(tidyverse)
library(readr)
library(magrittr)
library(readxl)
library(data.table)
library(epitools)

#------------------------------------------------------------------------------
# Microdados de obitos
#------------------------------------------------------------------------------

# Import ----------------------------------------------------------------------

########### Leitura dos microdados 

SBIObitos <- read.csv2(
  file = "./data/input/exporta_obitos_individualizados_csv.csv",
  header = T , 
  fileEncoding = "UTF-8", 
  na.strings = "NA"
)

########### carregando dados com geoinformacao dos municipios
codigos_IBGE <- 
  read.csv2(
    file = "./data/input/codigos_IBGE_municipios.csv", 
    encoding = "ISO-8859-1"
  )

geoinfo_BA <- read.csv2(
  file = "./data/input/geoinformacao_municipios_BA.csv", 
  encoding = "ISO-8859-1"
) %>%
  select(CD_GEOCMU, CD_GEOCMU6DIG, NM_MUNICIP2, NRS, NM_RS,LONG, LAT, POP_MUN)

# Transform ----------------------------------------------------------------------

#unificando codigos de municipios de outras UF
SBIObitos[SBIObitos$UF.ESTADO!="BA",]$IBGE.MUNICIPIO <- 999998

# Transformando data
SBIObitos %<>% 
  as.data.frame() %>% 
  mutate(
    DATA_OBITO  = dmy(`DATA.OBITO`),
  ) %>% 
  rename(CD_GEOCMU6DIG = IBGE.MUNICIPIO)

###############################################
# Montando serie historica de obitos da Bahia
###############################################

serie_obitos_BA <- SBIObitos %>%
  mutate(
    DATA_OBITO = DATA.OBITO %>% dmy()
  ) %>% 
  group_by(DATA_OBITO ) %>% 
  summarise(obitos = n()) %>% 
  as.data.frame() %>% 
  mutate(
    mm_7d = frollmean(obitos , n=7, align = "right") %>% as.numeric(),
  )

#Inserindo NA na media moveis do registro sem data na serie da Bahia   
serie_obitos_BA[is.na(serie_obitos_BA$DATA_OBITO),"mm_7d"] <- NA

#####################################################
# Montando serie historica de obitos por municipios
#####################################################

# gerando banco auxiliar
periodo_SBI <- 
  seq(
    from = SBIObitos %>% pull(DATA_OBITO) %>% min() %>% as.Date(),
    to = SBIObitos %>% pull(DATA_OBITO) %>% max() %>% as.Date(),
    by="1 days"
  )

serie_SBI <- data.frame( 
  CD_GEOCMU = 
    rep(
      x = geoinfo_BA$CD_GEOCMU,
      times = length(periodo_SBI)
    ),
  CD_GEOCMU6DIG = 
    rep(
      x = geoinfo_BA$CD_GEOCMU6DIG,
      times = length(periodo_SBI)
    ),
  NM_MUNICIP = 
    rep(
      x = geoinfo_BA$NM_MUNICIP,
      times = length(periodo_SBI)
    ),
  NM_MUNICIP2 = 
    rep(
      x = geoinfo_BA$NM_MUNICIP2,
      times = length(periodo_SBI)
    ),
  NRS = 
    rep(
      x = geoinfo_BA$NRS,
      times = length(periodo_SBI)
    ),
  NM_RS = 
    rep(
      x = geoinfo_BA$NM_RS,
      times = length(periodo_SBI)
    ),
  LONG = 
    rep(
      x = geoinfo_BA$LONG,
      times = length(periodo_SBI)
    ),
  LAT = 
    rep(
      x = geoinfo_BA$LAT,
      times = length(periodo_SBI)
    ),
  POP_MUN = 
    rep(
      x = geoinfo_BA$POP_MUN,
      times = length(periodo_SBI)
    ),
  DATA_OBITO = 
    rep(
      x = periodo_SBI,
      times = length(geoinfo_BA$CD_GEOCMU6DIG)
    ) %>% sort()
)

# dataset temporario com serie historica dos obitos por municipio

temp_obitos_city <- SBIObitos %>%
  group_by(DATA_OBITO, CD_GEOCMU6DIG) %>% 
  summarise(obitos = n()) %>% 
  as.data.frame()

# juntando com o dataset de municipios
serie_obitos_city <-
  left_join(
    x = serie_SBI, 
    y = temp_obitos_city, 
    by = c("DATA_OBITO"="DATA_OBITO","CD_GEOCMU6DIG"="CD_GEOCMU6DIG")
  )

# transformando NA em 0
serie_obitos_city$obitos[is.na(serie_obitos_city$obitos)] <- 0

# calculando a media movel
serie_obitos_city %<>%  
  group_by(CD_GEOCMU) %>%
  mutate(
    mm_7d = frollmean(obitos , n=7, align = "right") %>% as.numeric(),
  )

#Inserindo NA na media moveis do registro sem data na serie da Bahia   
serie_obitos_city[is.na(serie_obitos_city$DATA_OBITO),"mm_7d"] <- NA

#####################################################
# Montando serie historica de obitos por macrorregiao
#####################################################

serie_obitos_NRS <- serie_obitos_city %>%
  group_by(DATA_OBITO, NRS) %>% 
  summarise(obitos = sum(obitos)) %>% 
  as.data.frame() %>% 
  group_by(NRS) %>%
  mutate(
    mm_7d = frollmean(obitos , n=7, align = "right") %>% as.numeric(),
  )

#Inserindo NA na media moveis do registro sem data na serie da Bahia   
serie_obitos_NRS[is.na(serie_obitos_NRS$DATA_OBITO),"mm_7d"] <- NA

###################################################################
# serie por semana epidemiologica
###################################################################

# serie de obitos por semana epidemiologia - Bahia
serie_obitos_semepi_BA <- 
  serie_obitos_NRS %>% 
  mutate(
    Semana_Epidemiologica = epitools::as.week(DATA_OBITO)$week
  ) %>% 
  group_by(Semana_Epidemiologica) %>% 
  summarise(obitos=sum(obitos))

# serie de obitos por semana epidemiologia - Macrorregiao
serie_obitos_semepi_NRS <- 
  serie_obitos_NRS %>% 
  mutate(
    Semana_Epidemiologica = epitools::as.week(DATA_OBITO)$week
  ) %>% 
  group_by(Semana_Epidemiologica,NRS) %>% 
  summarise(obitos=sum(obitos))

# Load ------------------------------------------------------------------------

# #salvando série histórica de obitos da Bahia
# write.csv2(serie_obitos_BA,"./data/output/serie_obitos_BA.csv", row.names = F)
# 
# #salvando série histórica de obitos da Bahia
# write.csv2(serie_obitos_city,"./data/output/serie_obitos_city.csv", row.names = F)
# 
# #salvando série histórica de obitos da NRS
# write.csv2(serie_obitos_NRS,"./data/output/serie_obitos_NRS.csv", row.names = F)
# 
# #salvando série histórica de obitos da NRS por semana epidemiologica
# write.csv2(serie_obitos_semepi_NRS,"./data/output/serie_obitos_semepi_NRS.csv", row.names = F)

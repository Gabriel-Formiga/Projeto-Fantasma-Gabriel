source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

##Análise 1

library(readxl)
library(dplyr)
library(ggplot2)

relatorio_vendas <- read_excel("relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")

infos_produtos <- read_excel("relatorio_old_town_road.xlsx", 
                             sheet = "infos_produtos")

infos_vendas <- read_excel("relatorio_old_town_road.xlsx", 
                           sheet = "infos_vendas")

df<-relatorio_vendas

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

df <- df %>%
  mutate(ano = format(Date, "%Y"))

df$ano = as.integer(df$ano)

vendas<-full_join(infos_produtos,infos_vendas,by="ItemID")
vendas<-full_join(vendas,df,by="SaleID")

vendas <- vendas %>%
  mutate(receita= UnityPrice*Quantity*5.31)

vendas$receita

vendas1880=subset(vendas,vendas$ano==1880)
receita_1880=tapply(vendas1880$receita,vendas1880$StoreID,sum)
mean(receita_1880)

vendas1881=subset(vendas,vendas$ano==1881)
receita_1881=tapply(vendas1881$receita,vendas1881$StoreID,sum)
mean(receita_1881)

vendas1882=subset(vendas,vendas$ano==1882)
receita_1882=tapply(vendas1882$receita,vendas1882$StoreID,sum)
mean(receita_1882)

vendas1883=subset(vendas,vendas$ano==1883)
receita_1883=tapply(vendas1883$receita,vendas1883$StoreID,sum)
mean(receita_1883)


vendas1884=subset(vendas,vendas$ano==1884)
receita_1884=tapply(vendas1884$receita,vendas1884$StoreID,sum)
mean(receita_1884)

vendas1885=subset(vendas,vendas$ano==1885)
receita_1885=tapply(vendas1885$receita,vendas1885$StoreID,sum)
mean(receita_1885)

vendas1886=subset(vendas,vendas$ano==1886)
receita_1886=tapply(vendas1886$receita,vendas1886$StoreID,sum)
mean(receita_1886)

vendas1887=subset(vendas,vendas$ano==1887)
receita_1887=tapply(vendas1887$receita,vendas1887$StoreID,sum)
mean(receita_1887)

vendas1888=subset(vendas,vendas$ano==1888)
receita_1888=tapply(vendas1888$receita,vendas1888$StoreID,sum)
mean(receita_1888)

vendas1889=subset(vendas,vendas$ano==1889)
receita_1889=tapply(vendas1889$receita,vendas1889$StoreID,sum)
mean(receita_1889)

anos <- 1880:1889
media_das_receitas <- c(
  mean(receita_1880),
  mean(receita_1881),
  mean(receita_1882),
  mean(receita_1883),
  mean(receita_1884),
  mean(receita_1885),
  mean(receita_1886),
  mean(receita_1887),
  mean(receita_1888),
  mean(receita_1889)
)

df_medias <- data.frame(
  ano = anos,
  receita_Media = media_das_receitas
)

ggplot(df_medias) +
  aes(x=ano, y=receita_Media, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita média") +
  scale_x_continuous(breaks = unique(df_medias$ano)) +
  theme_estat() 
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")



##código pra limpar o banco


















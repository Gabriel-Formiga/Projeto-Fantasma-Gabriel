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

library(readxl)
library(dplyr)
library(ggplot2)

infos_clientes <- read_excel("relatorio_old_town_road.xlsx", 
                             sheet = "infos_clientes")

infos_cidades <- read_excel("relatorio_old_town_road.xlsx", 
                            sheet = "infos_cidades")


infos_lojas <- read_excel("relatorio_old_town_road.xlsx", 
                          sheet = "infos_lojas")

relatorio_vendas <- read_excel("relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")

lojas<-full_join(infos_cidades,infos_lojas,by="CityID")
lojas<-full_join(lojas,relatorio_vendas,by="StoreID")
lojas<-full_join(lojas,infos_clientes,by="ClientID")

loja_ambar=subset(lojas,lojas$CityID==2)

idades_em_ambar <- distinct(loja_ambar, StoreID, NameStore, ClientID, Age)

ggplot(idades_em_ambar) +
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Nome da loja", y = "Idade") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

idades_loja_1=subset(idades_em_ambar,idades_em_ambar$StoreID==2)

idades_loja_2=subset(idades_em_ambar,idades_em_ambar$StoreID==6)

idades_loja_3=subset(idades_em_ambar,idades_em_ambar$StoreID==8)

idades_loja_4=subset(idades_em_ambar,idades_em_ambar$StoreID==9)

media_das_lojas <- c(
  mean(idades_loja_1$Age),
  mean(idades_loja_2$Age),
  mean(idades_loja_3$Age),
  mean(idades_loja_4$Age)
)

mediana_dos_anos <- c(
  median(idades_loja_1$Age),
  median(idades_loja_2$Age),
  median(idades_loja_3$Age),
  median(idades_loja_4$Age)
)

desvio_padrao_dos_anos <- c(
  sd(idades_loja_1$Age),
  sd(idades_loja_2$Age),
  sd(idades_loja_3$Age),
  sd(idades_loja_4$Age)
)

minimo_lojas <- c(
  min(idades_loja_1$Age),
  min(idades_loja_2$Age),
  min(idades_loja_3$Age),
  min(idades_loja_4$Age)
)

maximo_lojas <- c(
  max(idades_loja_1$Age),
  max(idades_loja_2$Age),
  max(idades_loja_3$Age),
  max(idades_loja_4$Age)
)

amplitude_lojas <- c(
  max(idades_loja_1$Age) - min(idades_loja_1$Age),
  max(idades_loja_2$Age) - min(idades_loja_2$Age),
  max(idades_loja_3$Age) - min(idades_loja_3$Age),
  max(idades_loja_4$Age) - min(idades_loja_4$Age)
)

quartil_1_lojas <- c(
  quantile(idades_loja_1$Age, probs = 0.25),
  quantile(idades_loja_2$Age, probs = 0.25),
  quantile(idades_loja_3$Age, probs = 0.25),
  quantile(idades_loja_4$Age, probs = 0.25)
)

quartil_3_lojas <- c(
  quantile(idades_loja_1$Age, probs = 0.75),
  quantile(idades_loja_2$Age, probs = 0.75),
  quantile(idades_loja_3$Age, probs = 0.75),
  quantile(idades_loja_4$Age, probs = 0.75)
)

variancia_lojas <- c(
  var(idades_loja_1$Age),
  var(idades_loja_2$Age),
  var(idades_loja_3$Age),
  var(idades_loja_4$Age)
)

interquartil_lojas <- c(
  IQR(idades_loja_1$Age),
  IQR(idades_loja_2$Age),
  IQR(idades_loja_3$Age),
  IQR(idades_loja_4$Age)
)

limite_inferior_lojas <- c(
  quantile(idades_loja_1$Age, probs = 0.25) - 1.5 * IQR(idades_loja_1$Age),
  quantile(idades_loja_2$Age, probs = 0.25) - 1.5 * IQR(idades_loja_2$Age),
  quantile(idades_loja_3$Age, probs = 0.25) - 1.5 * IQR(idades_loja_3$Age),
  quantile(idades_loja_4$Age, probs = 0.25) - 1.5 * IQR(idades_loja_4$Age)
)

limite_superior_lojas <- c(
  quantile(idades_loja_1$Age, probs = 0.75) + 1.5 * IQR(idades_loja_1$Age),
  quantile(idades_loja_2$Age, probs = 0.75) + 1.5 * IQR(idades_loja_2$Age),
  quantile(idades_loja_3$Age, probs = 0.75) + 1.5 * IQR(idades_loja_3$Age),
  quantile(idades_loja_4$Age, probs = 0.75) + 1.5 * IQR(idades_loja_4$Age)
)

medidas_lojas <- data.frame(
  media = media_das_lojas,
  mediana = mediana_dos_anos,
  desvio_Padrao = desvio_padrao_dos_anos,
  minimo = minimo_lojas,
  maximo = maximo_lojas,
  amplitude = amplitude_lojas,
  quartil_1 = quartil_1_lojas,
  quartil_3 = quartil_3_lojas,
  variancia = variancia_lojas,
  interquartil = interquartil_lojas,
  limite_inferior = limite_inferior_lojas,
  limite_superior = limite_superior_lojas
)

# calculo da moda

frequencia_loja_1 <- table(idades_loja_1$Age)

frequencia_loja_2 <- table(idades_loja_2$Age)

frequencia_loja_3 <- table(idades_loja_3$Age)

frequencia_loja_4 <- table(idades_loja_4$Age)

View(frequencia_loja_1)
View(frequencia_loja_2)
View(frequencia_loja_3)
View(frequencia_loja_4)
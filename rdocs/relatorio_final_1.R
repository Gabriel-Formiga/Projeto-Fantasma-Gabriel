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
library(forcats)
library(stringr)

##Análise 1

library(readxl)
library(dplyr)
library(ggplot2)

relatorio_vendas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")

infos_produtos <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                               sheet = "infos_produtos")

infos_vendas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                               sheet = "infos_vendas")

df1<-relatorio_vendas

df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")

df1 <- df1 %>%
  mutate(ano = format(Date, "%Y"))

df1$ano = as.integer(df1$ano)

vendas<-full_join(infos_produtos,infos_vendas,by="ItemID")
vendas<-full_join(vendas,df1,by="SaleID")

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

# analise 2


infos_clientes <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                              sheet = "infos_clientes")
df <- infos_clientes

df_cliente <- df %>%
  mutate(altura_cm = Height_dm*10)

df_cliente_final <- df_cliente %>%
  mutate(peso_kg = Weight_lbs*0.453592)

cor.test(df_cliente_final$altura_cm,df_cliente_final$peso_kg)


# analise 3

infos_clientes <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                             sheet = "infos_clientes")

infos_cidades <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                            sheet = "infos_cidades")

infos_lojas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                          sheet = "infos_lojas")

relatorio_vendas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")

lojas<-full_join(infos_cidades,infos_lojas,by="CityID")
lojas<-full_join(lojas,relatorio_vendas,by="StoreID")
lojas<-full_join(lojas,infos_clientes,by="ClientID")

loja_ambar=subset(lojas,lojas$CityID==2)

idades_ambar <- distinct(loja_ambar, StoreID, NameStore, ClientID, Age)

# analise 4

relatorio_vendas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                                    sheet = "relatorio_vendas")

infos_vendas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                                    sheet = "infos_vendas")

infos_produtos <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                                    sheet = "infos_produtos")

infos_lojas <- read_excel("~/GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
                                     sheet = "infos_lojas")

df<-relatorio_vendas

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

df <- df %>%
  mutate(ano = format(Date, "%Y"))

df$ano = as.integer(df$ano)

teste_vendas_1 <- full_join(infos_produtos,infos_vendas,by="ItemID")
teste_vendas_1 <- full_join(teste_vendas_1,df,by="SaleID")
teste_vendas_1 <- full_join(teste_vendas_1,infos_lojas,by="StoreID")

teste_vendas_1 <- teste_vendas_1 %>%
  mutate(receita= UnityPrice*Quantity*5.31)

# receita das lojas em 1889

vendas_em_1889=subset(teste_vendas_1,teste_vendas_1$ano==1889)

vendas_loja_1=subset(vendas_em_1889,vendas_em_1889$StoreID==1)
receita_loja_1 = sum(vendas_loja_1$receita)

vendas_loja_2=subset(vendas_em_1889,vendas_em_1889$StoreID==2)
receita_loja_2 = sum(vendas_loja_2$receita)

vendas_loja_3=subset(vendas_em_1889,vendas_em_1889$StoreID==3)
receita_loja_3 = sum(vendas_loja_3$receita)

vendas_loja_4=subset(vendas_em_1889,vendas_em_1889$StoreID==4)
receita_loja_4 = sum(vendas_loja_4$receita)

vendas_loja_5=subset(vendas_em_1889,vendas_em_1889$StoreID==5)
receita_loja_5 = sum(vendas_loja_5$receita)

vendas_loja_6=subset(vendas_em_1889,vendas_em_1889$StoreID==6)
receita_loja_6 = sum(vendas_loja_6$receita)

vendas_loja_7=subset(vendas_em_1889,vendas_em_1889$StoreID==7)
receita_loja_7 = sum(vendas_loja_7$receita)

vendas_loja_8=subset(vendas_em_1889,vendas_em_1889$StoreID==8)
receita_loja_8 = sum(vendas_loja_8$receita)

vendas_loja_9=subset(vendas_em_1889,vendas_em_1889$StoreID==9)
receita_loja_9 = sum(vendas_loja_9$receita)

vendas_loja_10=subset(vendas_em_1889,vendas_em_1889$StoreID==10)
receita_loja_10 = sum(vendas_loja_10$receita)

vendas_loja_11=subset(vendas_em_1889,vendas_em_1889$StoreID==11)
receita_loja_11 = sum(vendas_loja_11$receita)

vendas_loja_12=subset(vendas_em_1889,vendas_em_1889$StoreID==12)
receita_loja_12 = sum(vendas_loja_12$receita)

vendas_loja_13=subset(vendas_em_1889,vendas_em_1889$StoreID==13)
receita_loja_13 = sum(vendas_loja_13$receita)

vendas_loja_14=subset(vendas_em_1889,vendas_em_1889$StoreID==14)
receita_loja_14 = sum(vendas_loja_14$receita)

vendas_loja_15=subset(vendas_em_1889,vendas_em_1889$StoreID==15)
receita_loja_15 = sum(vendas_loja_15$receita)

vendas_loja_16=subset(vendas_em_1889,vendas_em_1889$StoreID==16)
receita_loja_16 = sum(vendas_loja_16$receita)

vendas_loja_17=subset(vendas_em_1889,vendas_em_1889$StoreID==17)
receita_loja_17 = sum(vendas_loja_17$receita)

vendas_loja_18=subset(vendas_em_1889,vendas_em_1889$StoreID==18)
receita_loja_18 = sum(vendas_loja_18$receita)

receita_lojas <- c(
  receita_loja_1,
  receita_loja_2,
  receita_loja_3,
  receita_loja_4,
  receita_loja_5,
  receita_loja_6,
  receita_loja_7,
  receita_loja_8,
  receita_loja_9,
  receita_loja_10,
  receita_loja_11,
  receita_loja_12,
  receita_loja_13,
  receita_loja_14,
  receita_loja_15,
  receita_loja_16,
  receita_loja_17,
  receita_loja_18
)

df_receitas <- data.frame(
  receitas = receita_lojas
)

# quantidade vendida dos itens da loja 7

item_1_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==1)
quantidade_item_1_loja_7 = sum(item_1_loja_7$Quantity)

item_2_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==2)
quantidade_item_2_loja_7 = sum(item_2_loja_7$Quantity)

item_3_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==3)
quantidade_item_3_loja_7 = sum(item_3_loja_7$Quantity)

item_4_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==4)
quantidade_item_4_loja_7 = sum(item_4_loja_7$Quantity)

item_5_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==5)
quantidade_item_5_loja_7 = sum(item_5_loja_7$Quantity)

item_6_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==6)
quantidade_item_6_loja_7 = sum(item_6_loja_7$Quantity)

item_7_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==7)
quantidade_item_7_loja_7 = sum(item_7_loja_7$Quantity)

item_8_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==8)
quantidade_item_8_loja_7 = sum(item_8_loja_7$Quantity)

item_9_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==9)
quantidade_item_9_loja_7 = sum(item_9_loja_7$Quantity)

item_10_loja_7=subset(vendas_loja_7,vendas_loja_7$ItemID==10)
quantidade_item_10_loja_7 = sum(item_10_loja_7$Quantity)

quantidades_loja7 <- c(
  quantidade_item_1_loja_7,
  quantidade_item_2_loja_7,
  quantidade_item_3_loja_7,
  quantidade_item_4_loja_7,
  quantidade_item_5_loja_7,
  quantidade_item_6_loja_7,
  quantidade_item_7_loja_7,
  quantidade_item_8_loja_7,
  quantidade_item_9_loja_7,
  quantidade_item_10_loja_7
)

df_quantidade_7 <- data.frame(
  quantidade = quantidades_loja7
)

# quantidade vendida dos itens da loja 5

item_1_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==1)
quantidade_item_1_loja_5 = sum(item_1_loja_5$Quantity)

item_2_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==2)
quantidade_item_2_loja_5 = sum(item_2_loja_5$Quantity)

item_3_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==3)
quantidade_item_3_loja_5 = sum(item_3_loja_5$Quantity)

item_4_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==4)
quantidade_item_4_loja_5 = sum(item_4_loja_5$Quantity)

item_5_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==5)
quantidade_item_5_loja_5 = sum(item_5_loja_5$Quantity)

item_6_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==6)
quantidade_item_6_loja_5 = sum(item_6_loja_5$Quantity)

item_7_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==7)
quantidade_item_7_loja_5 = sum(item_7_loja_5$Quantity)

item_8_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==8)
quantidade_item_8_loja_5 = sum(item_8_loja_5$Quantity)

item_9_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==9)
quantidade_item_9_loja_5 = sum(item_9_loja_5$Quantity)

item_10_loja_5=subset(vendas_loja_5,vendas_loja_5$ItemID==10)
quantidade_item_10_loja_5 = sum(item_10_loja_5$Quantity)

quantidades_loja5 <- c(
  quantidade_item_1_loja_5,
  quantidade_item_2_loja_5,
  quantidade_item_3_loja_5,
  quantidade_item_4_loja_5,
  quantidade_item_5_loja_5,
  quantidade_item_6_loja_5,
  quantidade_item_7_loja_5,
  quantidade_item_8_loja_5,
  quantidade_item_9_loja_5,
  quantidade_item_10_loja_5
)

df_quantidade_5 <- data.frame(
  quantidade = quantidades_loja5
)

# quantidade vendida dos itens da loja 17

item_1_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==1)
quantidade_item_1_loja_17 = sum(item_1_loja_17$Quantity)

item_2_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==2)
quantidade_item_2_loja_17 = sum(item_2_loja_17$Quantity)

item_3_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==3)
quantidade_item_3_loja_17 = sum(item_3_loja_17$Quantity)

item_4_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==4)
quantidade_item_4_loja_17 = sum(item_4_loja_17$Quantity)

item_5_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==5)
quantidade_item_5_loja_17 = sum(item_5_loja_17$Quantity)

item_6_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==6)
quantidade_item_6_loja_17 = sum(item_6_loja_17$Quantity)

item_7_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==7)
quantidade_item_7_loja_17 = sum(item_7_loja_17$Quantity)

item_8_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==8)
quantidade_item_8_loja_17 = sum(item_8_loja_17$Quantity)

item_9_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==9)
quantidade_item_9_loja_17 = sum(item_9_loja_17$Quantity)

item_10_loja_17=subset(vendas_loja_17,vendas_loja_17$ItemID==10)
quantidade_item_10_loja_17 = sum(item_10_loja_17$Quantity)

quantidades_loja17 <- c(
  quantidade_item_1_loja_17,
  quantidade_item_2_loja_17,
  quantidade_item_3_loja_17,
  quantidade_item_4_loja_17,
  quantidade_item_5_loja_17,
  quantidade_item_6_loja_17,
  quantidade_item_7_loja_17,
  quantidade_item_8_loja_17,
  quantidade_item_9_loja_17,
  quantidade_item_10_loja_17
)

df_quantidade_17 <- data.frame(
  quantidade = quantidades_loja17
)

#colocar as top3 lojas em um df

top_3_lojas = subset(vendas_em_1889, vendas_em_1889$StoreID == 7 | vendas_em_1889$StoreID == 5 | vendas_em_1889$StoreID == 17 )

loja_5 = subset(top_3_lojas, top_3_lojas$StoreID == 5)
top_3_produtos_5 = subset(loja_5, loja_5$ItemID == 5 | loja_5$ItemID == 10 | loja_5$ItemID == 4 )
top_3_produtos_5 = top_3_produtos_5 %>%
  select(-UnityPrice, -SaleID, -Date, -ClientID, -CityID, -receita)

loja_7 = subset(top_3_lojas, top_3_lojas$StoreID == 7)
top_3_produtos_7 = subset(loja_7, loja_7$ItemID == 1 | loja_7$ItemID == 10 | loja_7$ItemID == 3 )
top_3_produtos_7 = top_3_produtos_7 %>%
  select(-UnityPrice, -SaleID, -Date, -ClientID, -CityID, -receita)

loja_17 = subset(top_3_lojas, top_3_lojas$StoreID == 17)
top_3_produtos_17 = subset(loja_17, loja_17$ItemID == 3 | loja_17$ItemID == 5 | loja_17$ItemID == 6 )
top_3_produtos_17 = top_3_produtos_17 %>%
  select(-UnityPrice, -SaleID, -Date, -ClientID, -CityID, -receita)

as_3_lojas = dados_completos <- bind_rows(top_3_produtos_5, top_3_produtos_7, top_3_produtos_17)
resumo_quantidade <- as_3_lojas %>%
  group_by(ItemID, StoreID) %>%
  summarise(TotalQuantity = sum(Quantity))

resumo_quantidade_classificado <- resumo_quantidade %>%
  mutate(
    Tipo_Loja = case_when(
      StoreID == 7 ~ "Loja Ouro Fino",
      StoreID == 5 ~ "Loja TendTudo",
      StoreID == 17 ~ "Ferraria Apache"
    )) 

resumo_quantidade_classificado <- resumo_quantidade_classificado %>%
  mutate(
    Nome_loja = case_when(
      ItemID == 1 ~ "Botas de Couro",
      ItemID == 3 ~ "Chapéu de Couro",
      ItemID == 4 ~ "Colt.45",
      ItemID == 5 ~ "Espingarda",
      ItemID == 6 ~ "Machado",
      ItemID == 10 ~ "Whisky"
    ))

resumo_quantidade_classificado <- resumo_quantidade_classificado %>%
  group_by(Tipo_Loja, Nome_loja) %>%
  summarise(
  Total_Vendido = sum(TotalQuantity)) %>%
  group_by(Tipo_Loja) %>%
  mutate(
    freq_relativa = round(Total_Vendido / sum(Total_Vendido) * 100, 1)
  )

porcentagens <- str_c(resumo_quantidade_classificado$freq_relativa, "%") %>% str_replace("
\\.", ",")

legendas <- str_squish(str_c(resumo_quantidade_classificado$Total_Vendido, " (", porcentagens, ")")
)

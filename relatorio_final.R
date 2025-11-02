library(readxl)
library(dplyr)
library(ggplot2)


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

df_relatorio_vendas <- df %>%
  mutate(ano = format(Date, "%Y"))

df_relatorio_vendas$ano = as.integer(df$ano)

teste_vendas_1 <- full_join(infos_produtos,infos_vendas,by="ItemID")
teste_vendas_1 <- full_join(teste_vendas_1,df_relatorio_vendas,by="SaleID")
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

vendas <- c(
  df_quantidade_7,
  df_quantidade_5,
  df_quantidade_17
)


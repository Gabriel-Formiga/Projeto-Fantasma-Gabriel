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

infos_clientes <- read_excel("~/relatorio_old_town_road (1).xlsx", 
                             sheet = "infos_clientes")

df <- infos_clientes

df_cliente <- df %>%
  mutate(altura_cm = Height_dm*10)

df_cliente_final <- df_cliente %>%
  mutate(peso_kg = Weight_lbs*0.453592)

cor.test(df_cliente_final$altura_cm,df_cliente_final$peso_kg)

ggplot(df_cliente_final) +
  aes(x = altura_cm, y = peso_kg) +
  geom_point(colour = "#A11D21", size = 3,
             alpha = 0.5) +
  labs(
    x = "Altura (Centimetros)",
    y = "Peso (quilogramas)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

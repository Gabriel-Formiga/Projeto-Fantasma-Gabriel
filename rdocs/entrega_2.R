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


infos_clientes <- read_excel("GitHub/Projeto-Fantasma-Gabriel/relatorio_old_town_road.xlsx", 
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


# medidas para o quadro de resumo da altura

media_do_peso_altura <- c(
  "Altura" = mean(df_cliente_final$altura_cm),
  "Peso" = mean(df_cliente_final$peso_kg)
)

mediana_do_peso_altura <- c(
  median(df_cliente_final$altura_cm),
  median(df_cliente_final$peso_kg)
)

desvio_padrao_do_peso_altura <- c(
  sd(df_cliente_final$altura_cm),
  sd(df_cliente_final$peso_kg)
)

minimo_peso_altura <- c(
  min(df_cliente_final$altura_cm),
  min(df_cliente_final$peso_kg)
)

maximo_peso_altura <- c(
  max(df_cliente_final$altura_cm),
  max(df_cliente_final$peso_kg)
)

amplitude_peso_altura <- c(
  max(df_cliente_final$altura_cm) - min(df_cliente_final$altura_cm),
  max(df_cliente_final$peso_kg) - min(df_cliente_final$peso_kg)
)

quartil_1_peso_altura <- c(
  quantile(df_cliente_final$altura_cm, probs = 0.25),
  quantile(df_cliente_final$peso_kg, probs = 0.25)
)

quartil_3_peso_altura <- c(
  quantile(df_cliente_final$altura_cm, probs = 0.75),
  quantile(df_cliente_final$peso_kg, probs = 0.75)
)

variancia_peso_altura <- c(
  var(df_cliente_final$altura_cm),
  var(df_cliente_final$peso_kg)
)

interquartil_peso_altura <- c(
  IQR(df_cliente_final$altura_cm),
  IQR(df_cliente_final$peso_kg)
)

limite_inferior_peso_altura <- c(
  quantile(df_cliente_final$altura_cm, probs = 0.25) - 1.5 * IQR(df_cliente_final$altura_cm),
  quantile(df_cliente_final$peso_kg, probs = 0.25) - 1.5 * IQR(df_cliente_final$peso_kg)
)

limite_superior_peso_altura <- c(
  quantile(df_cliente_final$altura_cm, probs = 0.75) + 1.5 * IQR(df_cliente_final$altura_cm),
  quantile(df_cliente_final$peso_kg, probs = 0.75) + 1.5 * IQR(df_cliente_final$peso_kg)
)



medidas_altura_peso <- data.frame(
  media = media_do_peso_altura,
  mediana = mediana_do_peso_altura,
  desvio_padrao = desvio_padrao_do_peso_altura,
  minimo = minimo_peso_altura,
  maximo = maximo_peso_altura,
  amplitude = amplitude_peso_altura,
  quartil_1 = quartil_1_peso_altura,
  quartil_3 = quartil_3_peso_altura,
  variancia = variancia_peso_altura,
  interquartil = interquartil_peso_altura,
  limite_inferior = limite_inferior_peso_altura,
  limite_superior = limite_superior_peso_altura
)

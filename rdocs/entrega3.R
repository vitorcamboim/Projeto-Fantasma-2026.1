setwd("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1")
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
# Consultor, favor utilizar este arquivo .R para realizar as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

# Trocar para o próprio diretório
infos_jogadores <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/banco/pixel110011.xlsx", sheet = "infos_jogadores")

# Todas variáveis escritas corretamente, então sem correções. Seguindo...

# Foi pedido o tempo em horas e está em minutos, então transformamos...
# Gráfico do tempo médio mensal pela idade
grafico_idade_tempo <- infos_jogadores %>%
  mutate(tempo_medio_mensal_horas = (logins_mes * tempo_medio_sessao_min) / 60) %>%
  ggplot() +
  aes(x = idade, y = tempo_medio_mensal_horas) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Idade (anos)",
    y = "Tempo médio mensal gasto (horas)"
    ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")


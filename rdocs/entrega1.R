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

# Atribuindo variáveis à abas do banco de dados
jogadores <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "infos_jogadores")
players <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "infos_players")

# Verificando as colunas do banco de dados por "names" e o banco por "view"
names(jogadores)
names(players)
# View(jogadores)
# View(players)

# A chave primária "player_id" da aba "infos_jogadores" 
# deveria ser chave estrangeira na aba "infos_players", 
# porém foi escrita como "pl4yers_id", ou seja, 
# devemos trocar esse "4" por "a".  
players <- players %>%
  rename(player_id = pl4yer_id)

# Para associar a variável "idade" de "infos_jogadores" 
# com a variável "onde_jogam" de "infos_players", 
# juntaremos as abas.
jogadores_players <- left_join(jogadores, players, by = "player_id")
# View(jogadores_players)

# Começando as análises...

# Boxplot das idades por plataforma:
boxplot_meios <- jogadores_players %>%
  ggplot(aes(x = reorder(onde_jogam, idade, FUN = median), y = idade)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Plataforma", y = "Idade em anos") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

# Quadro geral:
quadro_idade_onde_jogam <- jogadores_players %>%
  group_by(onde_jogam) %>%
  print_quadro_resumo(var_name = idade)

# Tabela geral:
jogadores_players %>%
  count(onde_jogam) %>%
  mutate(
    porcentagem = round(n / sum(n) * 100, 2)
  )

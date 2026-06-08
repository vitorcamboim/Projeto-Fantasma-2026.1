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


# Atribuindo os bancos as variáveis
jogos <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/banco/pixel110011.xlsx", sheet = "info_jogos")
compras <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/banco/pixel110011.xlsx", sheet = "info_compras")

# Organizando nome dos ID's no banco de dados
jogos <- jogos %>%
  rename(game_id = ga8e_id)

compras <- compras %>%
  rename(purchase_id = purch4se_id, 
         player_id = p7ayer_id, 
         game_id = gam5_id, 
         product_id = pr0duct_id)

# Juntando os bancos de dados

jogos_compras <- left_join(jogos, compras, by = "game_id")

# Criando BD apenas com compras de 2025 do jogo "Minecaft"

minecraft_2025 <- jogos_compras %>%
  filter(jogo == "Minecraft", year(data_compra) == 2025)

# Agrupando a soma dos valores monetários por mês

receita_minecraft_2025 <- minecraft_2025 %>%
  mutate(mes = lubridate::month(data_compra, label = TRUE)) %>%
  group_by(mes) %>%
  summarise(
    total_reais = sum(quantidade * `preco_unitario_R$`),
    total_dolares = round(total_reais / 5.19, 2))

# Gráfico de linhas do total de dólares por mês:

grafico_receita_2025 <- receita_minecraft_2025 %>%
  ggplot(aes(x=mes, y=total_dolares, group=1)) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Mês", y="Receita em dólares (US$)") +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")

# Quadro resumo das receitas

quadro_resumo_receita <- print_quadro_resumo(receita_minecraft_2025, total_dolares, 
                    title = "Medidas resumo da receita mensal do Minecraft em dólares em 2025",
                    label = "quad:quadro_receita")

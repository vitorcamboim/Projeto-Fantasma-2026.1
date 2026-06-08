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

# Atribuindo BD à variáveis
info_jogos <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "info_jogos")
infos_players <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "infos_players")
infos_produtos <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "infos_produtos")
info_compras <- read_excel("C:/Users/v4746/OneDrive/Desktop/ESTAT/Projeto-Fantasma-2026.1/rdocs/pixel110011.xlsx", sheet = "info_compras")

# Corrigindo nome de variáveis no BD
info_jogos <- info_jogos %>%
  rename(game_id = ga8e_id)

infos_players <- infos_players %>%
  rename(player_id = pl4yer_id,
         game_id = game_1d)

infos_produtos <- infos_produtos %>%
  rename(game_id = gam3_id)

info_compras <- info_compras %>%
  rename(purchase_id = purch4se_id,
         player_id = p7ayer_id,
         game_id = gam5_id,
         product_id = pr0duct_id)


# Juntando e selecionando todos as variáveis necessárias 
info_jogos_players <- info_jogos %>%
  left_join(infos_players, by = "game_id")

info_jogos_compras <- info_jogos %>%
  left_join(info_compras, by = "game_id")

# Top 3 jogos mais bem avaliados
top3_jogos <- info_jogos_players %>%
  filter(year(data_ultima_sessao) == 2024) %>%
  group_by(jogo) %>%
  summarise(media_nota_jogo = mean(nota_jogo)) %>%
  arrange(desc(media_nota_jogo)) %>%
  slice_head(n = 3)

# Produtos dos 3 jogos mais bem avaliados
produtos_dos_3jogos <- info_jogos_compras %>%
  filter(jogo %in% top3_jogos$jogo)

# Top 3 produtos de cada um dos 3 jogos mais bem avaliados

top3_produtos <- produtos_dos_3jogos %>%
  group_by(jogo, produto) %>%
  summarise(quantidade = sum(quantidade)) %>%
  slice_max(quantidade, n = 3) %>%
  arrange(jogo, desc(quantidade))

# Gráfico de colunas de acordo com o tópico passado

top3_produtos_dos_jogos <- top3_produtos %>%
  group_by(jogo, produto) %>%
  summarise(freq = sum(quantidade)) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

legendas <- str_squish(str_c(top3_produtos_dos_jogos$freq)
)
top3_produtos_dos_jogos <- top3_produtos_dos_jogos %>%
  mutate(
    produto = forcats::fct_reorder(produto, freq, .desc = TRUE)
  )
grafico_top3_produtos <- ggplot(top3_produtos_dos_jogos) +
  aes(
    x = fct_reorder(jogo, freq, .desc = T), y = freq,
    fill = produto, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2.5
  ) +
  labs(x = "Jogo", y = "Quantidade", fill = "Produto") +
  
  theme_estat() +
  coord_cartesian(
    ylim = c(0, 60),
    clip = "off"
  ) +
  guides(
    fill = guide_legend(nrow = 5)
  ) 

ggsave("top3-produtos-dos-top3-jogos.pdf", width = 158, height = 93, units = "mm")

grafico_top3_produtos



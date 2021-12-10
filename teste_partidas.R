library(tidyverse)
library(rvest)
library(curl)
library(xlsx)

pega_partidas_placar <- function(url) {
  page <- read_html(curl(url, handle = curl::new_handle("useragent" = "Firefox/5.0"))) 
  
  tabela <- tibble(rodada = rep(page %>%
                             html_nodes(".swiper-slide") %>%
                             html_attr("data-slide-index"),
                           each = 10) %>%
                as.numeric() + 1, 
              time_casa = page %>%
                html_nodes(".time .pull-left") %>%
                html_attr("title"),
              placar = page %>%
                html_nodes(".partida-horario") %>%
                html_node("span") %>% html_text(),
              time_fora = page %>%
                html_nodes(".time .pull-right") %>%
                html_attr("title")) %>%
    separate(col = placar, into = c("placar_casa", "placar_fora"), sep = "x") %>%
    mutate_at(vars(placar_casa, placar_fora), as.numeric)
}

# Cria um tibble para mudar os nomes velhos para os novos
vetor_nomes <- tribble(
  ~nome_old, ~nome_new,
  "AMERICA FC - MG", "AMERICA - MG",
  "ATLETICO - PR", "ATHLETICO PARANAENSE - PR",
  "ATLETICO PARANAENSE - PR", "ATHLETICO PARANAENSE - PR",
  "ATLETICO MINEIRO - MG", "ATLETICO - MG",
  "BRAGANTINO - SP", "RED BULL BRAGANTINO - SP"
)

# Pega as series do Brasileirao disponiveis
tab <- tibble(serie = c("A", "B")) %>%
  # Expande para os anos disponiveis
  expand(serie, edicao = 2012:2021) %>%
  mutate(value = paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-",
                        serie %>% str_to_lower(),
                        "/",
                        edicao)) %>%
  mutate(dados = map(.x = value, .f = pega_partidas_placar))

# TODO: Falta tratar os nomes dos clubes!
tab_rodadas <- tab %>%
  unnest(dados) %>%
  mutate(Clube = str_remove(Clube, "[+-]?\\d*") %>% str_trim(side = "both") %>% str_to_upper()) %>%
  left_join(vetor_nomes, by = c("Clube" = "nome_old")) %>%
  mutate(Clube = if_else(!is.na(nome_new),
                         nome_new,
                         Clube)) %>%
  select(-c(value, nome_new)) %>%
  arrange(serie, edicao, Posicao)

write.xlsx(tab_rodadas, "rodadas_brasileirao_serie_ab_2012-2021.xlsx")
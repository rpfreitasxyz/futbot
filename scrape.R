library(tidyverse)
library(rvest)
library(curl)
library(stringi)
library(xlsx)

trata_tabela = function(url) {
  tabela_extraida <- read_html(curl(url, handle = curl::new_handle("useragent" = "Firefox/5.0"))) %>%
    # A ultima tabela eh dos artilheiros. Interessante...
    html_node(".tabela-expandir") %>%
    html_table()
  
  tabela_final <- tabela_extraida %>%
    rename_with(~stri_trans_general(., "Latin-ASCII")) %>%
    mutate_all(~stri_trans_general(., "Latin-ASCII")) %>%
    # Trata os nomes esquisitos da tabela e depois da fill com os nomes de clubes bonitinhos
    mutate(Posicao = str_remove_all(Posicao, "\\\r") %>% str_remove_all("\\\n")) %>%
    mutate(Posicao = str_trim(Posicao)) %>%
    # PREMISSA: Nao tem numero em nome de clube
    mutate(Clube = str_remove(Posicao, "\\d*º") %>% as.character() %>% str_remove("^[+-]?\\w*") %>% str_trim(side = "both"),
           Posicao = str_extract(Posicao, "\\d*º") %>% str_remove("º") %>% as.numeric()) %>%
    relocate(Clube, Posicao) %>%
    mutate(Clube = na_if(Clube, "")) %>%
    fill(c(Clube, Posicao), .direction = "up") %>%
    group_by(Clube) %>% filter(min(row_number()) == row_number()) %>% ungroup() %>%
    select(-any_of(c("Recentes", "Prox"))) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    drop_na()
  
  return(tabela_final)
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
  mutate(dados = map(.x = value, .f = trata_tabela)) 

tab_fin <- tab %>%
  unnest(dados) %>%
  mutate(Clube = str_remove(Clube, "[+-]?\\d*") %>% str_trim(side = "both") %>% str_to_upper()) %>%
  left_join(vetor_nomes, by = c("Clube" = "nome_old")) %>%
  mutate(Clube = if_else(!is.na(nome_new),
                         nome_new,
                         Clube)) %>%
  select(-c(value, nome_new))

write.xlsx(tab_fin, "brasileirao_serie_a_2012-2021.xlsx")
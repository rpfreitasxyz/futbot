library(tidyverse)
library(rvest)
library(curl)
library(stringi)

trata_tabela = function(url) {
  tabela_extraida <- read_html(curl(url, handle = curl::new_handle("useragent" = "Firefox/5.0"))) %>%
    # A ultima tabela eh dos artilheiros. Interessante...
    html_table() %>%
    .[[1]]
  
  tabela_final <- tabela_extraida %>%
    rename_with(~stri_trans_general(., "Latin-ASCII")) %>%
    mutate_all(~stri_trans_general(., "Latin-ASCII")) %>%
    mutate(Posicao = str_remove(Posicao, "- \\w*")) %>%
    # Trata os nomes esquisitos da tabela e depois da fill com os nomes de clubes bonitinhos
    mutate(Posicao = str_remove_all(Posicao, "\\\r") %>% str_remove_all("\\\n") %>% str_remove("  .*")) %>%
    mutate(Posicao = str_trim(Posicao)) %>%
    mutate(Clube = str_remove(Posicao, "\\d*º") %>% as.character(),
           Posicao = str_remove(Posicao, "º") %>% as.numeric()) %>%
    relocate(Clube, Posicao) %>%
    mutate(Clube = na_if(Clube, "")) %>%
    # PREMISSA: Todos os nomes dos clubes tem ate 25 caracteres
    fill(c(Clube, Posicao), .direction = "up") %>%
    group_by(Clube) %>% filter(min(row_number()) == row_number()) %>% ungroup() %>%
    select(-any_of(c("Recentes", "Prox"))) %>%
    mutate_at(2:ncol(.), as.numeric)
  
  return(tabela_final)
}

tab <- seq(from = 2012, to = 2021, by = 1) %>%
  enframe(name = NULL) %>%
  rename(edicao = value) %>%
  mutate(value = paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a",
                        "/",
                        edicao)) %>%
  mutate(dados = map(.x = value, .f = trata_tabela))

tab_fin <- tab %>%
  unnest(dados)

# Analise do que esta acontecendo com 2017
url_2017 <- "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2017"
tabela_consol <- read_html(curl(url_2017, handle = curl::new_handle("useragent" = "Firefox/5.0"))) %>%
  # A ultima tabela eh dos artilheiros. Interessante...
  html_table() %>%
  .[[1]]

tabtab <- tabela_consol %>%
  rename_with(~stri_trans_general(., "Latin-ASCII"))
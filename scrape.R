library(tidyverse)
library(rvest)
library(curl)
library(stringi)

trata_tabela = function(url) {
  tabela_consol <- read_html(curl(url, handle = curl::new_handle("useragent" = "Firefox/5.0"))) %>%
    # A ultima tabela eh dos artilheiros. Interessante...
    html_table() %>%
    .[[1]] %>%
    rename_with(~stri_trans_general(., "Latin-ASCII")) %>%
    mutate(Posicao = str_remove(Posicao, "- \\w*")) %>%
    # Trata os nomes esquisitos da tabela e depois da fill com os nomes de clubes bonitinhos
    mutate(Posicao = ifelse(row_number() %% 2 == 1,
                            NA,
                            Posicao)) %>%
    mutate(Posicao = str_remove_all(Posicao, "\\\r") %>% str_remove_all("\\\n") %>% str_remove("  .*")) %>%
    mutate(Posicao = str_trim(Posicao)) %>%
    # PREMISSA: Todos os nomes dos clubes tem ate 25 caracteres
    fill(Posicao, .direction = "up") %>%
    filter(row_number() %% 2 == 1) %>%
    select(-matches(c("Recentes", "Prox"))) %>%
    mutate_at(2:ncol(.), as.numeric)
  
  return(tabela_consol)
}

tab <- seq(from = 2012, to = 2021, by = 1) %>%
  enframe(name = NULL) %>%
  rename(EDICAO = value) %>%
  mutate(value = paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a",
                        "/",
                        EDICAO)) %>%
  mutate(dados = map(.x = value, .f = trata_tabela))

tab_fin <- tab %>%
  unnest(dados)

# Analise do que esta acontecendo com 2017
url_2017 <- "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2017"
tabela_consol <- read_html(curl(url_2017, handle = curl::new_handle("useragent" = "Firefox/5.0"))) %>%
  # A ultima tabela eh dos artilheiros. Interessante...
  html_table() %>%
  .[[1]] %>%
  rename_with(~stri_trans_general(., "Latin-ASCII"))
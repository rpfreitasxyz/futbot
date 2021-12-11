library(tidyverse)
library(readxl)

# Pontuacao media para sair do rebaixamento
pont_rbx <- read_excel("brasileirao_serie_ab_2012-2021.xlsx") %>%
  filter(serie == "A", edicao < 2021) %>%
  group_by(edicao) %>%
  filter(Posicao == 16)
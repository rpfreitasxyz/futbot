library(tidyverse)
library(readxl)

# Pontuacao media para sair do rebaixamento
pont_rbx <- read_excel("dados/brasileirao_serie_ab_2012-2021.xlsx") %>%
  filter(serie == "A") %>%
  group_by(edicao) %>%
  filter(Posicao == 16)
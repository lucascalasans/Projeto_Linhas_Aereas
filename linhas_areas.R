library(tidyverse)
theme_set(theme_bw())
library(brazilmaps)
library(janitor)
aeroportos_br <- clean_names(read.csv(file = "dados/aeroportos_br.csv"))
aeroportos    <- clean_names(read.csv(file = "dados/Full_Merge_of_All_Unique Airports.csv"))
rotas         <- clean_names(read.csv(file = "dados/Full_Merge_of_All_Unique_Routes.csv"))
rotas_br <-
  rotas %>%
  filter(departure %in% aeroportos_br$id) %>%
  filter(destination %in% aeroportos_br$id)
rotas_br <- 
  rotas_br %>%
  rename(id = departure) %>%
  left_join(aeroportos, by = "id") %>%
  select(departure = id, destination, latitude_dep = latitude, longitude_dep = longitude) %>%
  rename(id = destination) %>%
  left_join(aeroportos, by = "id") %>%
  select(departure, destination = id, latitude_dep, longitude_dep, latitude_des = latitude, longitude_des = longitude)
rotas_br <- 
  rotas_br %>%
  group_by(departure, destination) %>%
  count() %>%
  left_join(rotas_br, by = c("departure", "destination")) %>%
  arrange(n)
library(circlize)
maiores_aeroportos <- 
  rotas_br %>%
  group_by(departure) %>%
  summarise(rotas_de_saida = sum(n)) %>%
  arrange(desc(rotas_de_saida))

k <- 10

maiores_aeroportos_nomes <- head(maiores_aeroportos$departure, k)

rotas_br_simplificadas <- 
  rotas_br %>%
  select(departure, destination, n) %>%
  mutate(departure = ifelse(departure %in% maiores_aeroportos_nomes, departure, "Outros"),
         destination = ifelse(destination %in% maiores_aeroportos_nomes, destination, "Outros"))

chordDiagram(rotas_br_simplificadas, 
             row.col = brewer.pal(k, "Paired"),
             column.col = brewer.pal(k, "Paired"))
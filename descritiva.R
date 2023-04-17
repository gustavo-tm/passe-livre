library(readr)
library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)

municipios <- read_municipality(code_muni = "all", year = 2018)

df <- read_csv("data.csv") %>% 
  select(id_municipio, abstencao, ano, turno) %>%
  rename("code_muni" = "id_municipio")
  
data <- left_join(municipios, df, by = "code_muni")

gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = abstencao), color = NA) +
  scale_fill_gradient2(midpoint = median(df$abstencao), low = "#5C5B60", mid = "#B3DBCF", high = "#D28673")+
  theme_void()

ggsave("descritiva/map.png", gg, dpi = 600, bg='transparent')

gg <- ggplot(df, aes(x = factor(ano), y = abstencao)) +
  geom_violin() +
  geom_hline(yintercept = mean(na.omit(df$abstencao)), color = "black", linetype = "dashed") +
  facet_wrap("turno") +
  stat_summary(fun = "mean", geom = "crossbar", width = .5) +
  theme_void()

ggsave("descritiva/abstencao.png", gg, dpi = 600, bg='transparent')



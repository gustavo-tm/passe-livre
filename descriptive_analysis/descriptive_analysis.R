library(readr)
library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)
windowsFonts(A = windowsFont("Times New Roman"))

municipios <- read_municipality(code_muni = "all", year = 2018)

df <- read_csv("data.csv") %>% 
  select(id_municipio, abstencao, ano, turno) %>%
  rename("code_muni" = "id_municipio")
  
data <- left_join(municipios, df, by = "code_muni")

#Mapa
gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = abstencao), color = NA) +
  scale_fill_gradient2(midpoint = median(df$abstencao), low = "#5C5B60", mid = "#B3DBCF", high = "#D28673") +
  labs(fill = "Abstenção") +
  theme_void()

ggsave("descriptive_analysis/map.png", gg, dpi = 600, bg='transparent')

#Violino
gg <- ggplot(df, aes(x = factor(ano), y = abstencao)) +
  geom_violin() +
  geom_hline(yintercept = mean(na.omit(df$abstencao)), color = "black", linetype = "dashed") +
  facet_wrap("turno") +
  stat_summary(fun = "mean", geom = "crossbar", width = .5) +
  theme_void()

ggsave("descriptive_analysis/abstencao.png", gg, dpi = 600, bg='transparent')

#Mapa de tendencias
df <- read_csv("data.csv") %>% 
  select(id_municipio, ano, turno, tratamento, abstencao) %>% 
  group_by(ano, turno, tratamento,) %>%
  summarize(abstencao = mean(abstencao)) %>% 
  mutate(turno = ifelse(turno == 1, "Primeiro Turno", "Segundo Turno"),
         tratamento= ifelse(tratamento == 1, "Tratamento", "Controle"))


gg <- ggplot(df, aes(x = ano, y = abstencao, color = tratamento)) +
  geom_vline(xintercept = 2018, linetype = "dashed", alpha = .4) +
  geom_line(lwd = .8)+
  geom_point(size = 2)+
  facet_wrap("turno") +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022)) +
  theme_bw() +
  labs(color = "Grupo", x = "Ano", y = "Abstenção", 
       title = "Abstenção para os grupos ao longo do tempo") +
  theme(text = element_text(family = "A")) +
  scale_color_manual(values = c("Tratamento" = "#5C5B60",
                                "Controle"="#D28673"))

ggsave("descriptive_analysis/tendencias.png", gg, dpi = 600)

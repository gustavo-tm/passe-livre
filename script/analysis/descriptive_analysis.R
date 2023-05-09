library(readr)
library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)
windowsFonts(A = windowsFont("Times New Roman"))

municipios <- read_municipality(code_muni = "all", year = 2018)

df <- read_csv("output/data.csv") %>% 
  select(id_municipio, abstencao, ano, turno, tratamento) %>%
  rename("code_muni" = "id_municipio") %>% 
  mutate(tratamento = ifelse(tratamento == 1, "Houve Passe Livre", "Não Houve Passe Livre"))
  
data <- left_join(municipios, df, by = "code_muni")

#Mapa da abstencao
gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = abstencao), color = NA) +
  scale_fill_gradient2(midpoint = median(df$abstencao), low = "#5C5B60", mid = "#B3DBCF", high = "#D28673") +
  labs(fill = "Abstenção") +
  theme_void()

ggsave("output/map_abstencao.png", gg, dpi = 600, bg='transparent')

#Mapa com municípios tratados
gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = factor(tratamento)), color = "#5C5B60", lwd = .001) +
  labs(fill = "Abstenção") +
  scale_fill_manual(values = c("Houve Passe Livre" = "#D28673",
                               "Não Houve Passe Livre"="#5C5B60"))+
  theme_void()

ggsave("output/map_tratamento.png", gg, dpi = 600, bg='transparent')


#Mapa de tendencias
df <- bind_rows(
  read_csv("output/data.csv") %>% mutate(psm = "Não balanceado"),
  read_csv("output/data_psm.csv") %>% mutate(psm = "Balanceado")) %>% 
  select(id_municipio, ano, turno, tratamento, abstencao, psm) %>% 
  group_by(ano, turno, tratamento, psm) %>%
  summarize(media = mean(abstencao),
            n = n(),
            sd = sd(abstencao)) %>% 
  mutate(ci = qt(.995, n) * sd / sqrt(n),
         turno = ifelse(turno == 1, "Primeiro Turno", "Segundo Turno"),
         tratamento= ifelse(tratamento == 1, "Tratamento", "Controle"))


gg <- ggplot(df, aes(x = ano, y = media, color = tratamento)) +
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=.3) +
  geom_line(lwd = .8)+
  geom_point(size = 2)+
  facet_grid(cols = vars(turno), rows = vars(psm)) +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022)) +
  theme_bw() +
  labs(color = "Grupo", x = "Ano", y = "Abstenção", 
       title = "Abstenção para cada grupo ao longo das eleições, antes e depois do PSM") +
  theme(text = element_text(family = "A")) +
  scale_color_manual(values = c("Tratamento" = "#5C5B60",
                                "Controle"="#D28673"))

gg

ggsave("output/tendencias.png", gg, dpi = 600)

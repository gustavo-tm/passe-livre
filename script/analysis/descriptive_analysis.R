library(tidyverse)
library(geobr)
library(ggplot2)
library(ggthemes)
library(sf)
library(modelsummary)
windowsFonts(A = windowsFont("Calibri"))

municipios <- read_municipality(code_muni = "all", year = 2018)

df <- read_csv("output/data.csv") %>% 
  select(id_municipio, abstencao, ano, turno, passe_livre) %>%
  rename("code_muni" = "id_municipio")  
  # mutate(tratamento = ifelse(tratamento == 1, "Houve Passe Livre", "Não Houve Passe Livre"))
  
data <- left_join(municipios, df, by = "code_muni")

#Mapa da abstencao
gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = abstencao), color = NA) +
  scale_fill_gradient2(midpoint = median(df$abstencao), low = "#5C5B60", mid = "#B3DBCF", high = "#D28673") +
  labs(fill = "Abstenção") +
  theme_void() +
  theme(text = element_text(family = "A"))
  
gg

ggsave("output/map_abstencao.png", gg, dpi = 600, bg='transparent')

#Mapa com municípios tratados
gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = factor(passe_livre)), color = "#5C5B60", lwd = .001) +
  labs(fill = "Abstenção") +
  theme(text = element_text(family = "A")) +
  scale_fill_manual(values = alpha(c("1" = "white",
                               "0" = "white"), c(1, 0)))+
  theme_void()


ggsave("output/map_tratamento.png", gg, dpi = 600, bg='transparent')


gg <- ggplot() +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = abstencao), color = NA) +
  scale_fill_gradient2(midpoint = median(df$abstencao), low = "#5C5B60", mid = "#B3DBCF", high = "#D28673") +
  geom_sf(data = data %>% filter(ano == 2022 & turno == 2), aes(fill = factor(passe_livre)), color = "#5C5B60", lwd = .001) +
  scale_fill_manual(values = alpha(c("1" = "white",
                                     "0" = "white"), c(1, 0)))+
  labs(fill = "Abstenção") +
  theme_void() +
  theme(text = element_text(family = "A"))


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

ggsave("output/tendencias.png", gg, dpi = 600, bg='transparent')

#Mapa de tendencias

df <- read_csv("output/data.csv") %>% 
  select(id_municipio, ano, turno, abstencao) %>% 
  group_by(ano, turno) %>%
  mutate(abstencao = abstencao * 100) %>% 
  summarize(media = mean(abstencao),
            n = n(),
            sd = sd(abstencao)) %>% 
  mutate(ci = qt(.995, n) * sd / sqrt(n),
         turno = ifelse(turno == 1, "Primeiro Turno", "Segundo Turno"))


gg <- ggplot(df, aes(x = ano, y = media, color = turno)) +
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=.3) +
  geom_line(lwd = .8)+
  geom_point(size = 2)+
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022)) +
  theme_bw() +
  # ylim(c(.15,.30)) +
  labs(color = "Turno", x = "Ano", y = "Abstenção", 
       title = "Abstenção ao longo do tempo") +
  scale_color_manual(values = c("Primeiro Turno" = "#5C5B60",
                                "Segundo Turno"="#D28673")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.background = element_blank(),
        # legend.box.background = element_blank(),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"))
gg

ggsave("output/tendencias.png", gg, dpi = 600)

#Mapa de tendencias
df <- read_csv("output/data.csv")

df <- df %>% 
  select(id_municipio, ano, turno, abstencao) %>% 
  left_join(
    df %>% 
      filter(ano == 2022) %>% 
      mutate(tratamento = (passe_livre == 1),
             controle = (passe_livre == 0)) %>% 
      select(c(id_municipio, turno, tratamento, controle))
  ) %>%
  group_by(ano, turno, tratamento) %>%
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
  facet_grid(rows = vars(turno)) +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022)) +
  theme_bw() +
  labs(color = "Grupo", x = "Ano", y = "Abstenção", 
       title = "Abstenção para cada grupo ao longo das eleições") +
  scale_color_manual(values = c("Tratamento" = "#5C5B60",
                                "Controle"="#D28673"))
gg

ggsave("output/tendencias.png", gg, dpi = 600)

#ANÁLISE DESCRITIVA
read_csv("output/data.csv") %>% 
  filter(ano == 2022) %>% 
  datasummary((abstencao) * (factor(passe_livre))  ~ factor(turno) * (Mean + SD + N), .,
              output = 'latex')

library(readr)
library(tidyverse)
library(plm)
library(MatchIt)
library(modelsummary)
library(fixest)

df <- read_csv("output/data.csv") 

modelo_placebo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + tratamento : factor(ano) | id_municipio + ano

clfe.1t <- feols(modelo_placebo, data = df %>% filter(turno == 1))
clfe.2t <- feols(modelo_placebo, data = df %>% filter(turno == 2))

summary(clfe.1t)
summary(clfe.2t)

df.psm <- read_csv("output/psm.csv") %>% 
  left_join(df %>% filter(ano == 2022) %>% select(id_municipio, tratamento, turno)) %>% 
  left_join(df %>% filter(ano == 2018) %>% select(id_municipio, abstencao, turno))

modelo_psm <- tratamento ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + (populacao_urbana/populacao) +
  abstencao

match.1t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 1), link="probit", replace = T) 
match.2t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 2), link="probit", replace = T) 

plot(match.1t,type="density",interactive=FALSE)
plot(match.2t,type="density",interactive=FALSE)

# match.1t <- matchit(
#   tratamento ~ log(abstencao) + log(competitividade) + log(pib_pc) + log(beneficiados) + log(pib_governo) + eleitores_secao,
#   data=df.psm %>% filter(turno == 1), link="probit", replace = T) 
# match.2t <- matchit(
#   tratamento ~ log(abstencao) + log(competitividade) + log(pib_pc) + log(beneficiados) + log(pib_governo) + eleitores_secao,
#   data=df.psm %>% filter(turno == 2), link="probit", replace = T) 

# plot(match,type="density",interactive=FALSE)


df.1t <- match.1t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 1)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 1))
df.2t <- match.2t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 2)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 2))

clfe.1t.psm.placebo <- feols(modelo_placebo, data = df.1t)
clfe.2t.psm.placebo <- feols(modelo_placebo, data = df.2t)
summary(clfe.1t.psm.placebo)
summary(clfe.2t.psm.placebo)

modelo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + passe_livre | id_municipio + ano

clfe.1t.psm <- feols(modelo, data = df.1t)
clfe.2t.psm <- feols(modelo, data = df.2t)

summary(clfe.1t.psm)
summary(clfe.2t.psm)


modelo_placebo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref= 2022) | id_municipio + ano

clfe <- feols(modelo_placebo, data = df.2t)
coefplot(clfe)


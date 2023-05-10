library(readr)
library(tidyverse)
library(plm)
library(MatchIt)
library(modelsummary)
library(fixest)
library(modelsummary)
formals(modelsummary)$stars <- TRUE

df <- read_csv("output/data.csv") 

#Teste placebo----
modelo_placebo <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref= 2022) | id_municipio + ano

modelsummary(list(
  feols(modelo_placebo, data = df %>% filter(turno == 1)),
  feols(modelo_placebo, data = df %>% filter(turno == 2))
))


#Propensity score matching ----
df.psm <- read_csv("output/psm.csv") %>% 
  left_join(df %>% filter(ano == 2022) %>% select(id_municipio, tratamento, turno)) %>% 
  left_join(df %>% filter(ano == 2018) %>% select(id_municipio, abstencao, turno) %>% rename("abstencao_2018" = "abstencao")) %>% 
  left_join(df %>% filter(ano <2022) %>% group_by(id_municipio) %>% 
      summarize(abstencao = mean(abstencao), competitividade = mean(competitividade), pib_pc = mean(pib_pc),
                beneficiados = mean(beneficiados), pib_governo = mean(pib_governo),
                eleitores_secao = mean(eleitores_secao), tratamento = max(tratamento))) %>% 
  mutate(populacao_urbana = populacao_urbana/populacao) %>% 
  drop_na()

modelo_psm <- tratamento ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana + abstencao_2018 +
  log(abstencao) + log(competitividade) + log(pib_pc) + log(beneficiados) + pib_governo + eleitores_secao

#Estimação do propensity
summary(glm(modelo_psm, data = df.psm, family = binomial(link = 'logit')))

modelo_psm <- tratamento ~ taxa_envelhecimento + taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + log(populacao) + populacao_urbana + log(abstencao) + log(pib_pc) + log(beneficiados) + eleitores_secao + abstencao_2018

match.1t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 1), link="probit", replace = T) 
match.2t <- matchit(
  modelo_psm,
  data=df.psm %>% filter(turno == 2), link="probit", replace = T) 

#Resultado do balanceamento
plot(match.1t,type="density",interactive=FALSE)
plot(match.2t,type="density",interactive=FALSE)

df.1t <- match.1t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 1)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 1))
df.2t <- match.2t %>%  
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 2)) %>% 
  select(id_municipio) %>% 
  left_join(df %>% filter(turno == 2))

bind_rows(df.1t, df.2t) %>% 
  write_csv("output/data_psm.csv")

#Teste placebo depois do PSM
modelsummary(list(
  feols(modelo_placebo, data = df.1t),
  feols(modelo_placebo, data = df.2t)
))


#Estimação do DD----
modelo.1 <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + passe_livre | id_municipio + ano

feols.m1.1t <- feols(modelo.1, data = df.1t)
feols.m1.2t <- feols(modelo.1, data = df.2t)

modelo.2 <- log(abstencao) ~ 
  log(competitividade) + ideb + log(beneficiados) + 
  log(pib_governo) + log(eleitores_secao) + passe_livre * log(pib_pc) | id_municipio + ano
feols.m2.1t <- feols(modelo.2, data = df.1t)
feols.m2.2t <- feols(modelo.2, data = df.2t)

modelo.3 <- log(abstencao) ~ 
  log(competitividade) + ideb + log(beneficiados) + log(pib_pc) +
  log(pib_governo) + log(eleitores_secao) + 
  passe_livre : factor(ntile(pib_pc,5)) | id_municipio + ano

feols.m3.1t <- feols(modelo.3, data = df.1t)
feols.m3.2t <- feols(modelo.3, data = df.2t)

modelsummary(list(
  feols.m1.1t,
  feols.m2.1t,
  feols.m3.1t
))

modelsummary(list(
  feols.m1.2t,
  feols.m2.2t,
  feols.m3.2t
))

modelsummary(list(
  feols.m1.1t,
  feols.m1.2t
))
modelsummary(list(
  feols.m2.1t,
  feols.m2.2t
))
modelsummary(list(
  feols.m3.1t,
  feols.m3.2t
))

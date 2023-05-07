library(readr)
library(tidyverse)
library(plm)
library(MatchIt)


df.1t <- read_csv("output/data.csv") %>% 
  filter(turno == 1)
df.2t <- read_csv("output/data.csv") %>% 
  filter(turno == 2)

modelo <- log(abstencao) ~ log(competitividade) + log(pib_pc) + ideb + log(beneficiados) + log(pib_governo) + log(eleitores_secao)

model.fe.1t <- plm(
  modelo, data = df.1t, model = "within", index = c("id_municipio", "ano")
)
model.fe.2t <- plm(
  modelo, data = df.2t, model = "within", index = c("id_municipio", "ano")
)

model.re.1t <- plm(
  modelo, data = df.1t, model = "random", index = c("id_municipio", "ano")
)
model.re.2t <- plm(
  modelo, data = df.2t, model = "random", index = c("id_municipio", "ano")
)

phtest(model.fe.1t, model.re.1t)
phtest(model.fe.2t, model.re.2t)

summary(model.fe.1t)
summary(model.fe.2t)

df.psm.2t <- df.2t %>% 
  filter(ano <2022) %>% 
  group_by(id_municipio) %>% 
  summarize(abstencao = mean(abstencao),
            competitividade = mean(competitividade),
            pib_pc = mean(pib_pc),
            beneficiados = mean(beneficiados),
            pib_governo = mean(pib_governo),
            eleitores_secao = mean(eleitores_secao),
            tratamento = max(tratamento)) %>% 
  drop_na()

match <- matchit(
  tratamento ~ log(abstencao) + log(competitividade) + log(pib_pc) + log(beneficiados) + log(pib_governo) + eleitores_secao,
  data=df.psm.2t,
  link="probit",
  replace = TRUE)

plot(match,type="density",interactive=FALSE)



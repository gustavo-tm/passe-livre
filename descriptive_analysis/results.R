library(readr)
library(tidyverse)
library(plm)

df.1t <- read_csv("data.csv") %>% 
  filter(turno == 1)
df.2t <- read_csv("data.csv") %>% 
  filter(turno == 2)

modelo <- log(abstencao) ~ sqrt(competitividade) + log(pib_pc) + ideb + log(beneficiados) + pib_governo

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

summary(model.re.1t)
summary(model.re.2t)


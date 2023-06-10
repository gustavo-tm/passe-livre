library(readr)
library(tidyverse)
library(MatchIt)
library(modelsummary)
library(fixest)
library(modelsummary)
library(gt)
formals(modelsummary)$stars <- TRUE

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# DADOS

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

df <- read_csv("output/data.csv") %>% 
  mutate(quantil = factor(ntile(.$pib_pc,4)))

df <- df %>% 
  #Tratamento A -> municípios que adotaram o passe livre
  #Controle A   -> municípios que não adotaram o passe livre
  left_join(
    df %>% 
      filter(ano == 2022) %>% 
      mutate(tratamento_A = (passe_livre == 1),
             controle_A = (passe_livre == 0)) %>% 
      select(c(id_municipio, turno, tratamento_A, controle_A))
  ) %>%
  #Tratamento B -> municípios que tiveram o passe livre apenas no segundo turno
  #Controle B   -> municípios que tiveram o passe livre em ambos os turnos
  left_join(
    df %>%
      filter(ano == 2022) %>%
      select(c(id_municipio, passe_livre)) %>%
      group_by(id_municipio) %>%
      summarise(passe_livre = sum(passe_livre)) %>%
      mutate(tratamento_B = (passe_livre == 1),
             controle_B = (passe_livre == 2)) %>%
      select(c(id_municipio, tratamento_B, controle_B)),
    by = "id_municipio"
  ) %>% 
  select(-passe_livre)

#Base com a estratégia A
df.A <- df %>% 
  filter(tratamento_A == 1 | controle_A == 1) %>% 
  mutate(tratamento = tratamento_A) %>% 
  select(-c(tratamento_A, controle_A, tratamento_B, controle_B))

#Base com a estratégia B
df.B <- df %>% 
  filter(tratamento_B == 1 | controle_B == 1) %>% 
  mutate(tratamento = tratamento_B) %>% 
  select(-c(tratamento_A, controle_A, tratamento_B, controle_B))

#Estatísticas descritivas
df %>% 
  filter(ano == 2022) %>% 
  datasummary((tratamento_A + controle_A + tratamento_B + controle_B) * (abstencao) ~ (Mean + N) * (factor(turno)), .,
              fmt = 3)

#Variáveis do propensity
df.psm <- read_csv("output/psm.csv") %>% 
  #Grupo controle ou tratamento
  left_join(
    df.A %>% 
      filter(ano == 2022) %>% 
      select(id_municipio, tratamento, turno)
  ) %>% 
  #Abstenção de 2018
  left_join(
    df.A %>% 
      filter(ano == 2018) %>% select(id_municipio, abstencao, turno) %>% 
      rename("abstencao_2018" = "abstencao")
  ) %>% 
  #Variáveis médias antes de 2022
  left_join(
    df.A %>% filter(ano <2022) %>% 
      group_by(id_municipio, turno) %>% 
      summarize(competitividade = mean(competitividade), pib_pc = mean(pib_pc),
                beneficiados = mean(beneficiados), pib_governo = mean(pib_governo),
                eleitores_secao = mean(eleitores_secao))) %>% 
  mutate(populacao_urbana = populacao_urbana/populacao,
         quantil = ntile(.$pib_pc,4)) %>% 
  drop_na()

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# DESIGN DOS MODELOS

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

modelo.feols <- log(abstencao) ~ 
  i(ano, tratamento, ref = 2018) | id_municipio + ano

modelo.faols <- log(abstencao) ~ 
  i(ano, tratamento, ref = 2018) + tratamento | ano

modelo.feols.controle <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + log(beneficiados) + ideb + log(populacao) +
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref = 2018)| id_municipio + ano

modelo.faols.controle <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + log(beneficiados) + ideb + log(populacao) +
  log(pib_governo) + log(eleitores_secao) + i(ano, tratamento, ref = 2018) + tratamento | ano

modelo.psm <- tratamento ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana + 
  log(abstencao_2018) + log(competitividade) + log(pib_pc) + log(beneficiados) + pib_governo + eleitores_secao

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# ARTIGO TELEFONE

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#


#TABELA DE RESULTADOS
modelsummary(list(
  feols(modelo.feols, data = df.B %>% filter(turno == 1)),
  feols(modelo.faols, data = df.B %>% filter(turno == 1)),
  feols(modelo.feols.controle, data = df.B %>% filter(turno == 1)),
  feols(modelo.faols.controle, data = df.B %>% filter(turno == 1))
), 
# stars = T,
# statistic = "({p.value})",
estimate = "{estimate} {stars}",
statistic = NULL,
coef_rename = c("log(pib_pc)" = "log(PIB per capita)"),
gof_omit = "AIC|BIC|F|Lik|Std.Errors|RMSE")

#VISUALIZAÇÃO DO EVENT STUDY
iplot(feols(modelo.feols, data = df.B %>% filter(turno == 1)))
iplot(feols(modelo.feols.controle, data = df.B %>% filter(turno == 1)))


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# NOSSA ABORDAGEM ANTES DO PSM

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

#TABELA DE RESULTADOS
modelsummary(list(
  feols(modelo.feols, data = df.A %>% filter(turno == 1)),
  feols(modelo.faols, data = df.A %>% filter(turno == 1)),
  feols(modelo.feols.controle, data = df.A %>% filter(turno == 1)),
  feols(modelo.faols.controle, data = df.A %>% filter(turno == 1))
), 
# stars = T,
# statistic = "({p.value})",
estimate = "{estimate} {stars}",
statistic = NULL,
coef_rename = c("log(pib_pc)" = "log(PIB per capita)"),
gof_omit = "AIC|BIC|F|Lik|Std.Errors|RMSE")

#VISUALIZAÇÃO DO EVENT STUDY
iplot(list(
  feols(modelo.feols, data = df.A %>% filter(turno == 1)),
  feols(modelo.feols, data = df.A %>% filter(turno == 2))
  ))

iplot(list(
  feols(modelo.feols.controle, data = df.A %>% filter(turno == 1)),
  feols(modelo.feols.controle, data = df.A %>% filter(turno == 2))
))



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# PROPENSITY SCORE MATCHING

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

psm <- glm(modelo.psm, data = df.psm, family = binomial(link = "logit"))

#Balanceamento antes do PSM
df.psm %>% 
  mutate(ps = predict(glm(modelo.psm, data = ., family = binomial(link = "logit")), type = "response")) %>% 
  ggplot() +
    geom_density(aes(ps, fill = tratamento), alpha = .7) +
    facet_wrap("turno")

df.match.1t <- matchit(modelo.psm, data=df.psm %>% filter(turno == 1), link="probit", replace = T) %>% 
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 1)) %>% 
  select(id_municipio) %>% 
  left_join(df.A %>% filter(turno == 1))

df.match.2t <- matchit(modelo.psm, data=df.psm %>% filter(turno == 2), link="probit", replace = T) %>% 
  get_matches(distance = "propscore", data=df.psm %>% filter(turno == 2)) %>% 
  select(id_municipio) %>% 
  left_join(df.A %>% filter(turno == 2))

df.match <- df.match.1t %>% 
  bind_rows(df.match.2t)

#Balanceamento depois do propensity
df.psm %>% 
  semi_join(df.match, by = "id_municipio") %>% 
  mutate(ps = predict(glm(modelo.psm, data = ., family = binomial(link = "logit")), type = "response")) %>% 
  ggplot() +
  geom_density(aes(ps, fill = tratamento), alpha = .7) +
  facet_wrap("turno")


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# NOSSA ABORDAGEM DEPOIS DO PSM

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#


#TABELA DE RESULTADOS
modelsummary(list(
  feols(modelo.feols, data = df.match %>% filter(turno == 1)),
  feols(modelo.faols, data = df.match %>% filter(turno == 1)),
  feols(modelo.feols.controle, data = df.match %>% filter(turno == 1)),
  feols(modelo.faols.controle, data = df.match %>% filter(turno == 1))
), 
# stars = T,
# statistic = "({p.value})",
estimate = "{estimate} {stars}",
statistic = NULL,
coef_rename = c("log(pib_pc)" = "log(PIB per capita)"),
gof_omit = "AIC|BIC|F|Lik|Std.Errors|RMSE")

#VISUALIZAÇÃO DO EVENT STUDY
iplot(feols(modelo.feols, data = df.match %>% filter(turno == 1)))
iplot(feols(modelo.feols.controle, data = df.match %>% filter(turno == 1)))

#VISUALIZAÇÃO DO EVENT STUDY
iplot(list(
  feols(modelo.feols, data = df.match %>% filter(turno == 1)),
  feols(modelo.feols, data = df.match %>% filter(turno == 2))
))

iplot(list(
  feols(modelo.feols.controle, data = df.match %>% filter(turno == 1)),
  feols(modelo.feols.controle, data = df.match %>% filter(turno == 2))
))


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# EFEITO HETEROGÊNEO COM PROPENSITY GERAL

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

df.test <- df.match %>% 
  mutate(quantil = factor(ntile(.$pib_pc,4)))

modelo.feols.controle.heterogen <- log(abstencao) ~ 
  log(competitividade) + log(pib_pc) + log(beneficiados) + ideb + log(populacao) +
  log(pib_governo) + log(eleitores_secao) + trat * factor(ntile(pib_pc,5))| id_municipio + ano

#TABELA DE RESULTADOS
modelsummary(
  feols(modelo.feols.controle.heterogen, data = df.test %>% filter(turno == 2)), 
# stars = T,
statistic = "({p.value})",
estimate = "{estimate} {stars}",
# statistic = NULL,
coef_rename = c("log(pib_pc)" = "log(PIB per capita)"),
gof_omit = "AIC|BIC|F|Lik|Std.Errors|RMSE")

iplot(list(
  feols(modelo.feols, data = df.test %>% filter(turno == 1 & quantil == 1)),
  feols(modelo.feols, data = df.test %>% filter(turno == 1 & quantil == 2)),
  feols(modelo.feols, data = df.test %>% filter(turno == 1 & quantil == 3)),
  feols(modelo.feols, data = df.test %>% filter(turno == 1 & quantil == 4))
))

iplot(list(
  feols(modelo.feols, data = df.test %>% filter(turno == 2 & quantil == 1)),
  feols(modelo.feols, data = df.test %>% filter(turno == 2 & quantil == 2)),
  feols(modelo.feols, data = df.test %>% filter(turno == 2 & quantil == 3)),
  feols(modelo.feols, data = df.test %>% filter(turno == 2 & quantil == 4))
))


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# EFEITO HETEROGÊNEO COM PROPENSITY INDIVIDUAL

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

df.match.segmented <- data.frame()
for (quantil.i in c(1:5)) {
  df.match.segmented <- df.match.segmented %>% 
    bind_rows(
      matchit(update(modelo.psm, . * (quantil == quantil.i) ~ .), data=df.psm %>% filter(turno == 2), link="probit", replace = T) %>% 
        get_matches(distance = "propscore", data=df.psm %>% filter(turno == 2)) %>% 
        select(id_municipio) %>% 
        left_join(df.A %>% filter(turno == 2))
    )
}

#Balanceamento depois do propensity
df.psm %>% 
  semi_join(df.match.segmented, by = "id_municipio") %>% 
  mutate(ps = predict(glm(modelo.psm, data = ., family = binomial(link = "logit")), type = "response")) %>% 
  ggplot() +
  geom_density(aes(ps, fill = tratamento), alpha = .7) +
  facet_wrap("quantil")

iplot(list(
  feols(modelo.feols, data = df.match.segmented %>% filter(turno == 2 & quantil == 1)),
  feols(modelo.feols, data = df.match.segmented %>% filter(turno == 2 & quantil == 2)),
  feols(modelo.feols, data = df.match.segmented %>% filter(turno == 2 & quantil == 3)),
  feols(modelo.feols, data = df.match.segmented %>% filter(turno == 2 & quantil == 4))
))


# Tese de Laurea - Credito politicamente direcionado
# William Radaic
# Essa versao: Nov 3 2023


## Download dados ----------------------------------------------

rm(list = ls())

# Packages 

library(tidyverse)
library(basedosdados)
library(readr)
library(ggplot2)
library(stringr)
library(modelsummary)
library(estimatr)
library(foreign)
library(stringi)
library(rdrobust)


# Downloading TSE data 

set_billing_id("sacred-ember-346401")


# Demographics
query <- bdplyr("br_tse_eleicoes.candidatos")
df_demographics <- bd_collect(query)

write_csv(df_demographics, "data/demographics.csv")


# Revenue
query <- bdplyr("br_tse_eleicoes.receitas_candidato")
df_revenue <- bd_collect(query)

write_csv(df_revenue, "data/revenue.csv")



# Voting
query <- bdplyr("br_tse_eleicoes.resultados_candidato")
df_voting <- bd_collect(query)

write_csv(df_voting, "data/voting.csv")

save(df_demographics, df_revenue, df_voting, file = "bdd_data.RData")

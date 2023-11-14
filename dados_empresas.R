# Tese de Laurea - Credito politicamente direcionado
# William Radaic
# Essa versao: Nov 3 2023

## Dados -- Empresas ----------------------------------------------

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


# Importing data -----------------------------

demographics <- read_csv("data/demographics.csv")
# revenue <- read_csv("data/revenue.csv")
revenue <- read_csv("data/revenue_trimmed.csv")
voting <- read_csv("data/voting.csv")

desembolsos_mensais <- read_delim("data/desembolsos-mensais.csv", 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                  trim_ws = TRUE)


operacoes_indiretas <- read_delim("data/operacoes-financiamento-operacoes-indiretas-automaticas.csv", 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                      grouping_mark = "."), trim_ws = TRUE)

operacoes_nao_automaticas <- read_delim("data/operacoes-financiamento-operacoes-nao-automaticas.csv", 
                                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                            grouping_mark = "."), trim_ws = TRUE) # provavelmente base principal 


# Coalition data


coalition_2006 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2006.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 grouping_mark = ".", encoding = "ISO-8859-1"), 
                             trim_ws = TRUE) %>%
  select(sg_uf, nr_candidato, nm_candidato, sg_partido, ds_composicao_coligacao, ds_sit_tot_turno)

coalition_2010 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2010.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 grouping_mark = ".", encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)  %>%
  select(sg_uf, nr_candidato, nm_candidato, sg_partido, ds_composicao_coligacao, ds_sit_tot_turno)

coalition_2014 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2014.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 grouping_mark = ".", encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)  %>%
  select(sg_uf, nr_candidato, nm_candidato, sg_partido, ds_composicao_coligacao, ds_sit_tot_turno)






# Dropping data outside BNDES interval ----------------------------------
demographics <- demographics %>% 
  filter(ano >= 2002)

revenue <- revenue %>% 
  filter(ano <= 2019) %>%
  filter(cargo == "deputado federal")

# write_csv(revenue, "data/revenue_trimmed.csv")

voting <- voting %>% 
  filter(ano >= 2002)



# Merging candidate data ---------------------------------------------

df_depfed <- demographics %>%
  drop_na(id_candidato_bd) %>%
  filter(cargo == "deputado federal") %>%
  left_join(voting, by = c("id_candidato_bd", "ano", "tipo_eleicao")) 
  
df_depfed <- df_depfed %>% 
  select(-contains(".y")) %>%
  # select(-c(sigla_uf:sequencial_candidato)) %>%
  rename_with(~str_remove(., '.x')) %>%
  select(-email)


df_depfed <- df_depfed %>%
  left_join(revenue, by = c("id_candidato_bd", "ano", "tipo_eleicao"), relationship = "one-to-many") %>%
  select(-contains(".y")) %>%
  rename_with(~str_remove(., '.x')) %>%
  mutate(cpf_cnpj_doador = str_replace_all(cpf_cnpj_doador, "[[:punct:]]", "")) %>%
  mutate(cpf_cnpj_doador = as.numeric(cpf_cnpj_doador)) %>%
  mutate(municipio_nascimento = toupper(stri_trans_general(str = municipio_nascimento, 
                                                           id = "latin-ascii")
)) %>% # harmonizing formatting for municipio
  mutate(municipio_nascimento = ifelse(municipio_nascimento == "MAZAG AO", "MAZAGAO", municipio_nascimento)) %>%
  mutate(municipio_nascimento = 
           str_replace_all(municipio_nascimento, "[^[:alnum:]]", ""))


# formatting municipio
operacoes_indiretas <- operacoes_indiretas %>%
  mutate(municipio = str_replace_all(municipio, "[^[:alnum:]]", ""))



# Selecting BNDES loans from TSE CNPJs -----------------------------------------

analysis_years = c(2006, 2010, 2014) 

for (analysis_year in analysis_years) { 


# analysis_year = 2014

df_depfed_subset <- df_depfed %>%
  filter(ano == analysis_year)


operacoes_nao_automaticas <- operacoes_nao_automaticas %>%
  mutate(cnpj = str_replace_all(cnpj, "[[:punct:]]", "")) %>%
  mutate(cnpj = as.numeric(cnpj))

bndes_op_donors <- operacoes_nao_automaticas

# Subset 4 yrs after election
bndes_op_donors <- bndes_op_donors %>%
  filter(data_da_contratacao >= paste(analysis_year, "01-01", sep = "-") & 
           data_da_contratacao <= paste(analysis_year + 4, "12-31", sep = "-")) %>%
  filter(cnpj %in% df_depfed_subset$cpf_cnpj_doador) 


# sum total contracts by CNPJ
bndes_op_donors <- bndes_op_donors %>%
  group_by(cnpj) %>%
#  filter(cnpj != 191) %>% # removendo operacoes ao BB
  summarise(total_contratado = sum(valor_contratado_reais), total_desembolsado = sum(valor_desembolsado_reais), across()) %>%
  distinct(cnpj, total_contratado, total_desembolsado, .keep_all = T) %>%
  ungroup() %>%
  select(cnpj, total_contratado, total_desembolsado, cliente, uf, municipio)
  
  
# Merging candidate and donor
df_candidate_contracts <- df_depfed_subset %>%
  select(ano, tipo_eleicao, sigla_uf, id_candidato_bd, nome, cpf_cnpj_doador, valor_receita, data_receita, origem_receita, resultado, votos, cpf, numero_partido, sigla_partido, situacao, 
         idade, genero, instrucao, estado_civil, nacionalidade, sigla_uf_nascimento, raca, turno, numero_candidato) %>% 
  left_join(bndes_op_donors, by = c("cpf_cnpj_doador" = "cnpj"))

df_candidate_contracts <- df_candidate_contracts %>% 
  mutate(total_contratado = replace_na(total_contratado, 0), 
         total_desembolsado = replace_na(total_desembolsado, 0),
         valor_receita = replace_na(valor_receita, 0)) %>%
  filter(origem_receita == "recursos de pessoas juridicas")


df_candidate_contracts <- df_candidate_contracts %>%
  group_by(id_candidato_bd)

# calculate total contracts for each candidate, avg, log 
df_candidate_contracts <- df_candidate_contracts %>%
  summarise(total_donations = sum(valor_receita), 
            total_bndes_donors_c = sum(total_contratado), 
            total_bndes_donors_d = sum(total_desembolsado), 
            n_clients = nlevels(as.factor(unique(cliente, na.rm = T))),
            avg_bndes_donors_c = total_bndes_donors_c/n_clients,
            avg_bndes_donors_d = total_bndes_donors_d/n_clients,
            across()) %>%
  mutate(avg_bndes_donors_c = ifelse(is.nan(avg_bndes_donors_c), 0, avg_bndes_donors_c),
         avg_bndes_donors_d = ifelse(is.nan(avg_bndes_donors_d), 0, avg_bndes_donors_d)) %>%
  mutate(log_avg_c = log(1+avg_bndes_donors_c),
         log_avg_d = log(1+avg_bndes_donors_d)) %>%
  ungroup() %>%
  filter(total_donations > 0 & (situacao == "deferido" | situacao == "deferido com recurso"))

df_total_contracts <- df_candidate_contracts %>%
  distinct(id_candidato_bd, .keep_all = T) 


df_total_contracts <- df_total_contracts %>%
  select(-c(origem_receita, cpf, cliente, uf, municipio))

df_total_contracts <- df_total_contracts %>%
  mutate(indic_bndes = ifelse(total_contratado > 0, 1, 0))



# Merge with coalition
coalition_year <- get(paste0("coalition_", analysis_year))

coalition_year <- coalition_year %>%
  distinct(sg_uf, sg_partido, ds_composicao_coligacao) %>%
  rename(colig = ds_composicao_coligacao) %>%
  group_by(sg_uf) %>%
  # mutate(colig_pt = ifelse(sg_partido == "PT", colig, NA)) %>%
  ungroup()


colig_uf_pt <- coalition_year %>%
  group_by(sg_uf) %>%
  mutate(colig_pt = ifelse(sg_partido == "PT", colig, NA)) %>%
  select(sg_uf, colig_pt) %>%
  drop_na(colig_pt) %>%
  distinct() %>%
  ungroup()


df_total_contracts <- df_total_contracts %>%
  left_join(coalition_year, by = c("sigla_uf" = "sg_uf", 
                                   "sigla_partido" = "sg_partido")) %>%
  relocate(sigla_uf, .after = ano) %>%
  relocate(numero_candidato, sigla_partido, colig, .after = sigla_uf)


# pt coalition
df_total_contracts <- df_total_contracts %>%
  left_join(colig_uf_pt, by = c("sigla_uf" = "sg_uf")) %>%
  mutate(is_colig_pt = ifelse(colig == colig_pt, 1, 0)) %>%
  mutate(colig = ifelse(is.na(colig), sigla_partido, colig))


# Calculate vote margin

if (analysis_year == 2014) {
  
  df_total_contracts <- df_total_contracts %>%
    mutate(elected = 
             ifelse(resultado == "eleito por qp" | resultado == "eleito por media",
                    1, 0))
  
  result_w <- df_total_contracts %>%
    group_by(sigla_uf, colig) %>%
    filter(resultado == "eleito por qp" | resultado == "eleito por media") %>%
    summarize(min_votos_win = min(votos, na.rm = T), .groups = "keep")
  
  result_l <- df_total_contracts %>%
    group_by(sigla_uf, colig) %>%
    filter(resultado != "eleito por qp" & resultado != "eleito por media") %>%
    summarize(max_votos_loss = max(votos, na.rm = T), .groups = "keep")
  
} else { 
  
  df_total_contracts <- df_total_contracts %>%
    mutate(elected = ifelse(resultado == "eleito" | resultado == "eleito por media",
                            1, 0))
  
  result_w <- df_total_contracts %>%
    group_by(sigla_uf, colig) %>%
    filter(resultado == "eleito" | resultado == "eleito por media") %>%
    summarize(min_votos_win = min(votos, na.rm = T), .groups = "keep")
  
  result_l <- df_total_contracts %>%
    group_by(sigla_uf, colig) %>%
    filter(resultado != "eleito" & resultado != "eleito por media") %>%
    summarize(max_votos_loss = max(votos, na.rm = T), .groups = "keep")
  
}

df_total_contracts <- df_total_contracts %>%
  left_join(result_w, by = c("sigla_uf", "colig")) %>%
  left_join(result_l, by = c("sigla_uf", "colig")) %>%
  ungroup()


df_total_contracts <- df_total_contracts %>% 
  drop_na(min_votos_win) %>% # remove coalition with zero seats or in which everyone wins
  mutate(vote_margin = ifelse(resultado == "eleito" | resultado == "eleito por media", 
                              votos - max_votos_loss, votos - min_votos_win),
         abs_vote_margin = abs(vote_margin))        


# save df
assign(paste0("df_", analysis_year), df_total_contracts)






}


# remove margin zero (tie)


df_2006 <- df_2006 %>%
  filter(abs_vote_margin != 0)

df_2010 <- df_2010 %>%
  filter(abs_vote_margin != 0)

df_2014 <- df_2014 %>%
  filter(abs_vote_margin != 0)



### Pooled data

df_all <- rbind(df_2006, df_2010, df_2014)

df_all <- df_all %>%
  mutate(factor_cluster = interaction(ano, sigla_uf, colig)) 

df_reg <- df_all %>%
  filter(abs_vote_margin <= 25000) %>%
  mutate(elected = as.factor(elected)) %>%
  mutate(elected = recode_factor(elected, `0` = "Não eleito", 
                                 `1` = "Eleito"))

df_pt <- df_all %>%
  filter(sigla_partido == "PT") %>%
  mutate(elected = as.factor(elected)) %>%
  mutate(elected = recode_factor(elected, `0` = "Não eleito", 
                                 `1` = "Eleito"))

df_pt_2006 <- df_pt %>%
  filter(ano == "2006")
df_pt_2010 <- df_pt %>%
  filter(ano == "2010")
df_pt_2014 <- df_pt %>%
  filter(ano == "2014")




df_pt_colig <- df_all %>%
  filter(is_colig_pt == 1) %>%
  mutate(elected = as.factor(elected)) %>%
  mutate(elected = recode_factor(elected, `0` = "Não eleito", 
                                 `1` = "Eleito"))

df_pt_colig_2006 <- df_pt_colig %>%
  filter(ano == "2006")
df_pt_colig_2010 <- df_pt_colig %>%
  filter(ano == "2010")
df_pt_colig_2014 <- df_pt_colig %>%
  filter(ano == "2014")





write.dta(df_all, file = "df_all.dta")
write.dta(df_pt, file = "df_pt.dta")
write.dta(df_pt_colig, file = "df_pt_colig.dta")



# inspect

# View(df_all %>% group_by(ano) %>% arrange(colig, votos))







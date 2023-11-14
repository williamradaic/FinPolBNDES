# Tese de Laurea - Credito politicamente direcionado
# William Radaic
# Essa versao: Nov 6 2023


## Dados -- Cidades, base eleitoral ----------------------------

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

# Importing data -----------------------------

demographics <- read_csv("data/demographics.csv")
# revenue <- read_csv("data/revenue.csv")
revenue <- read_csv("data/revenue_trimmed.csv")
voting <- read_csv("data/voting.csv")


# desembolsos_mensais <- read_delim("data/desembolsos-mensais.csv", 
#                                   delim = ";", escape_double = FALSE, 
#                                   locale = locale(decimal_mark = ",",
#                                                   grouping_mark = "."), 
#                                   trim_ws = TRUE)

operacoes_indiretas <- read_delim("data/operacoes-financiamento-operacoes-indiretas-automaticas.csv", 
                                  delim = ";", escape_double = FALSE, 
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = "."), 
                                  trim_ws = TRUE)

operacoes_nao_automaticas <- read_delim("data/operacoes-financiamento-operacoes-nao-automaticas.csv", 
                                        delim = ";", escape_double = FALSE, 
                                        locale = locale(decimal_mark = ",", 
                                                       grouping_mark = "."), 
                                        trim_ws = TRUE) # provavelmente base principal 



coalition_2006 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2006.csv", 
                             delim = ";", escape_double = FALSE, 
                             locale = locale(decimal_mark = ",", 
                                             grouping_mark = ".", 
                                             encoding = "ISO-8859-1"), 
                                             trim_ws = TRUE) %>%
  select(sg_uf, nr_candidato, 
         nm_candidato, sg_partido, 
         ds_composicao_coligacao, 
         ds_sit_tot_turno)

coalition_2010 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2010.csv", 
                             delim = ";", 
                             escape_double = FALSE, 
                             locale = locale(decimal_mark = ",", 
                                             grouping_mark = ".", 
                                             encoding = "ISO-8859-1"), 
                                             trim_ws = TRUE)  %>%
  select(sg_uf, nr_candidato, 
         nm_candidato, sg_partido, 
         ds_composicao_coligacao, 
         ds_sit_tot_turno)

coalition_2014 <- read_delim("data/cand_mais_votado-brasil_deputado_federal_2014.csv", 
                             delim = ";", 
                             escape_double = FALSE, 
                             locale = locale(decimal_mark = ",", 
                             grouping_mark = ".", 
                             encoding = "ISO-8859-1"), 
                             trim_ws = TRUE)  %>%
  select(sg_uf, nr_candidato, 
         nm_candidato, sg_partido, 
         ds_composicao_coligacao, 
         ds_sit_tot_turno)


# mais votado por municipio
votacao_candidato_mun_2006 <- read_csv("data/votacao_candidato_mun_2006.csv")
votacao_candidato_mun_2010 <- read_csv("data/votacao_candidato_mun_2010.csv")
votacao_candidato_mun_2014 <- read_csv("data/votacao_candidato_mun_2014.csv")

# votos nominais por municipio
votos_por_mun_2006 <- read_csv("data/votos_por_mun_2006.csv")
votos_por_mun_2010 <- read_csv("data/votos_por_mun_2010.csv")
votos_por_mun_2014 <- read_csv("data/votos_por_mun_2014.csv")





# Label and rbind data for plots, estimation ---------------------------------
operacoes_nao_automaticas <- operacoes_nao_automaticas %>%
  mutate(tipo_op = "Não automática") %>%
  rename(valor_da_operacao_em_reais = valor_contratado_reais)


operacoes_indiretas <- operacoes_indiretas %>%
  mutate(tipo_op = "Automática")


operacoes_full <- bind_rows(operacoes_nao_automaticas, operacoes_indiretas)



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
  rename_with(~str_remove(., '.x')) %>%
  select(-email)

df_depfed <- df_depfed %>%
  mutate(municipio_nascimento = 
           toupper(stri_trans_general(str = municipio_nascimento, 
                                      id = "latin-ascii")
           )) %>% # harmonizing formatting for municipio
  mutate(municipio_nascimento = ifelse(municipio_nascimento == "MAZAG AO", "MAZAGAO", municipio_nascimento)) %>%
  mutate(municipio_nascimento = 
           str_replace_all(municipio_nascimento, "[^[:alnum:]]", ""))



# Selecting BNDES loans from candidates' cities -----------------------------------
analysis_years = c(2006, 2010, 2014) 

for (analysis_year in analysis_years) { 
  
  # analysis_year = 2014
  
  df_depfed_subset <- df_depfed %>%
    filter(ano == analysis_year)
  
  
  bndes_cities <- operacoes_full
  
  # Subset 4 yrs after election
  bndes_cities <- bndes_cities %>%
    filter(data_da_contratacao >= paste(analysis_year, "01-01", sep = "-") & 
             data_da_contratacao <= paste(analysis_year + 4, "12-31", sep = "-"))
  
  
  bndes_cities <- bndes_cities %>%
    select(-c(prazo_carencia_meses, prazo_amortizacao_meses, 
              modalidade_de_apoio, area_operacional, 
              cnpj_do_agente_financeiro))
  
  
  # formatting municipio
  bndes_cities <- bndes_cities %>%
    mutate(municipio = str_replace_all(municipio, "[^[:alnum:]]", ""))
  
  
  # sum total
  bndes_cities <- bndes_cities %>%
    group_by(municipio_codigo) %>%
    summarise(total_contratado = sum(valor_da_operacao_em_reais),
              total_desembolsado = sum(valor_desembolsado_reais), across()) %>%
    distinct(municipio, total_contratado, total_desembolsado, .keep_all = T) %>%
    ungroup() %>% 
    relocate(uf, municipio, data_da_contratacao, .after = municipio_codigo) %>%
    select(-c(cliente:situacao_da_operacao))
  
  # remove sem municipio
  bndes_cities <- bndes_cities %>%
    filter(municipio_codigo != 0)
  
  # get mais votados
  votacao_mun_year <- get(paste0("votacao_candidato_mun_", analysis_year))
  votos_por_mun_year <- get(paste0("votos_por_mun_", analysis_year))

  # get proporcao de votos do candidato no municipio
  votacao_mun_year <- votacao_mun_year %>%
    left_join(votos_por_mun_year, by = c("nm_municipio" = "Município",
                                         "sg_uf" = "UF")) %>%
    rename(qt_votos_mun = `Quantidade de votos nominais válidos`)

  votacao_mun_year <- votacao_mun_year %>%
    mutate(vot_prop_mun = qt_votos_nom_validos/qt_votos_mun) %>%
    mutate(municipio = toupper(stri_trans_general(str = nm_municipio,
                                                  id = "latin-ascii"))) %>%
    mutate(municipio = str_replace_all(municipio, "[^[:alnum:]]", "")) %>%
    relocate(municipio, .before = nm_municipio)
  
  
  # merge tse with base eleitoral 
  df_candidate_contracts <- df_depfed_subset %>%
    select(ano, tipo_eleicao, sigla_uf_nascimento, 
           municipio_nascimento, sigla_uf, id_candidato_bd, 
           nome, resultado, votos, cpf, numero_partido, 
           sigla_partido, situacao, idade, genero, 
           instrucao, estado_civil, nacionalidade, 
           sigla_uf_nascimento, raca, turno, numero_candidato)
  
  
  df_candidate_contracts <- df_candidate_contracts %>%
    left_join(votacao_mun_year, by = c("sigla_uf" = "sg_uf",
                                     "numero_candidato" = "nr_candidato"),
              relationship = "one-to-many")
  
  
  # calculate prop voting by candidate
  df_candidate_contracts <- df_candidate_contracts %>%
    group_by(id_candidato_bd) %>%
    mutate(vot_prop_cand = qt_votos_nom_validos/votos) #%>%
   # select(vot_prop_cand) 
  
  
  df_candidate_contracts <- df_candidate_contracts %>%
      group_by(id_candidato_bd) %>%
      slice_max(vot_prop_cand, n = 5) %>%
      ungroup() %>%
      filter(!is.nan(vot_prop_cand) & !is.na(vot_prop_cand)) %>%
      filter(vot_prop_cand > 0)
  
  
  # Merging candidate and bndes municipality data 
  df_candidate_contracts <- df_candidate_contracts %>%
    left_join(bndes_cities, by = c("sigla_uf" = "uf",
                                   "municipio"))
  
 
  # clean df
  df_candidate_contracts <- df_candidate_contracts %>% 
    mutate(total_contratado = replace_na(total_contratado, 0), 
           total_desembolsado = replace_na(total_desembolsado, 0)) %>%
    filter(situacao == "deferido" | situacao == "deferido com recurso")
  
  
  df_candidate_contracts <- df_candidate_contracts %>%
    group_by(id_candidato_bd) %>%
    summarise(total_contratado_base = sum(total_contratado), 
              total_desembolsado_base = sum(total_desembolsado), across()) %>%
    ungroup()
  
  
  df_candidate_contracts <- df_candidate_contracts %>%
    mutate(log_c = log(1+total_contratado_base), 
           log_d = log(1+total_desembolsado_base))
  
  df_candidate_contracts <- df_candidate_contracts %>%
    distinct(id_candidato_bd, log_c, .keep_all = T) %>%
    select(id_candidato_bd, ano, sigla_uf, log_c, log_d, 
           total_contratado_base, total_desembolsado_base,
           votos, resultado, nome, numero_partido, sigla_partido, sg_partido, 
           ds_composicao_coligacao, idade, genero, instrucao, 
           estado_civil, nacionalidade, raca, numero_candidato, 
           ds_sit_tot_turno, vot_prop_cand)
  
  
  df_total_contracts <- df_candidate_contracts
  
  
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
    mutate(colig = ifelse(is.na(colig), sigla_partido, colig)) %>%
    mutate(is_colig_pt = ifelse(colig == colig_pt, 1, 0))
  
  
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
  
  # merge win loss for each coalition
  df_total_contracts <- df_total_contracts %>%
    left_join(result_w, by = c("sigla_uf", "colig")) %>%
    left_join(result_l, by = c("sigla_uf", "colig")) %>%
    ungroup()
  
  # calculate vote margin
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
  filter(abs_vote_margin != 0) %>%
  mutate(factor_cluster = interaction(sigla_uf, colig))

df_2010 <- df_2010 %>%
  filter(abs_vote_margin != 0) %>%
  mutate(factor_cluster = interaction(sigla_uf, colig))


df_2014 <- df_2014 %>%
  filter(abs_vote_margin != 0) %>%
  mutate(factor_cluster = interaction(sigla_uf, colig))




### Pooled data
df_all <- rbind(df_2006, df_2010, df_2014)

df_all <- df_all %>%
  mutate(factor_cluster = interaction(ano, sigla_uf, colig)) %>%
  mutate(factor_uf = as.factor(sigla_uf))

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



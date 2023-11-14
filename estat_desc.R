# Tese de Laurea - Credito politicamente direcionado
# William Radaic
# Essa versao: Nov 1 2023


# Estatísticas descritivas 



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

theme_set(theme_bw(base_size = 25))

theme_update(
  legend.position="bottom",
  # legend.position = c(0.01, 0.01),
  plot.title = element_text(hjust = 0.5),
  legend.justification = c("center", "bottom"),
  legend.box.just = "right",
  legend.background = element_rect(color = NA),
  # legend.margin = margin(0, 2, 0, 2),
  # legend.box.background = element_rect(),
  # legend.box.margin = margin(2, 2, 2, 2),
  panel.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = "dashed", size = .5, colour = "grey80"),
  panel.grid.minor.y = element_blank(),
  legend.title = element_blank(),
  text = element_text(family = "Times")
)


# Create your function
custom_number_format <- function(x){ifelse(x > 999999999,
                                           paste(format(round((x/1000000000), 2), 
                                                        nsmall=1, big.mark="."), "bi"),
                                           ifelse(x > 999999, 
                                                  paste(format(round((x/1000000), 1), 
                                                               nsmall=1, big.mark="."),"mi"), 
                                                  format(round(x), nsmall=0, big.mark=".")))}


options(scipen = 999)
options(OutDec= ",")   # read ?options



# Figura: Volume de recursos contratados por tipo e ano -------------------


op_por_tipo_ano <- operacoes_full %>%
  mutate(ano = year(data_da_contratacao)) %>%
  group_by(tipo_op, ano) %>%
  summarise(total_contratado = sum(valor_da_operacao_em_reais),
            total_desembolsado = sum(valor_desembolsado_reais),
            n_operacoes = n(),
            media_contratado = mean(valor_da_operacao_em_reais, na.rm = T), 
            var_contratado = var(valor_da_operacao_em_reais, na.rm = T)) %>%
  filter(ano <= 2018 & ano >= 2002)



ggplot(op_por_tipo_ano, aes(fill=tipo_op, y=total_contratado, x=ano)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                     label = scales::label_number(prefix = "R$ ", suffix = " bi", scale = 1e-9)) + 
  labs(x = "Ano", y = "Montante de recursos contratados") 

ggsave("fig/vol_ano.pdf", width = 11, height = 8.5)


# Tabela descritiva: -------------------------------------------------


op_tipo_table <- op_por_tipo_ano %>%
  select(-total_desembolsado) %>%
  group_by(ano) %>%
  pivot_wider(names_from = c(tipo_op), values_from = c("total_contratado", 
                                                       "media_contratado", 
                                                       "var_contratado",
                                                       "n_operacoes")) %>%
  relocate(contains("_Automá"), .after = "ano") %>%
  mutate(across(contains("_"), ~(custom_number_format(.x)))) %>%
  select(-contains("var_"))


datasummary_df(data = op_tipo_table, fmt = 0)
datasummary_df(data = op_tipo_table, output = "tabela_desc.tex", fmt = 0)


               # scales::label_number(prefix = "R$ ", suffix = " bi", scale = 1e-9))






























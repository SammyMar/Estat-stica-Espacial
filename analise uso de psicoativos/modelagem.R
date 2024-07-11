# Importacao e enriquecimento dos dados -----------------------------------
library(readxl)
library(readr)
library(dplyr)
library(sf)
library(spdep)
library(sp)
library(spdep)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(leaflet)
setwd("analise uso de psicoativos")
dados <- readRDS('dados/base_final.rds')

#Dados de alfabetizacao
alfabetizacao <- read_excel("dados/tabela9542_BR_GR_UF.xlsx", 
                                  sheet = "Tabela 2", skip = 7)
alfabetizacao <- alfabetizacao[2:(nrow(alfabetizacao)-1),1:4]
names(alfabetizacao) <- c('Estado','total','alfabetizados','nao_alfabetizados')

#save da tabela tratada e filtrada
alfabetizacao |> saveRDS('dados/taxa_alfabetizacao.rds')

#Exclusao da coluna porc total para juncao com base de dados
alfabetizacao$total <- NULL

#Dados de renda media total e media per capita, pop branca e pop preta
dados_ibge <- read_delim("dados/dados_ibge.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(numero_medio_anos_de_estudo_18_a_29 = col_skip(), 
                                                                              porc_ens_sup_completo_25_anos_mais = col_skip(), 
                                                                              porc_so_estudam_15_a_17 = col_skip(), 
                                                                              porc_so_estudam_18_a_24 = col_skip(), 
                                                                              porc_so_ocupados_18_a_24 = col_skip(), 
                                                                              porc_ens_sup_rede_publica = col_skip(), 
                                                                              porc_ens_medio_rede_publica = col_skip(), 
                                                                              porc_freq_escolar_bruta_18_a_24 = col_skip(), 
                                                                              porc_estudam_e_ocupados_18_a_24 = col_skip(), 
                                                                              porc_ens_medio_completo_25_anos_mais = col_skip(), 
                                                                              porc_despesas_em_cultura = col_skip(), 
                                                                              porc_despesas_em_educacao = col_skip(), 
                                                                              taxa_freq_escolar_liquida_15_a_17 = col_skip(), 
                                                                              despesa_media_cultura = col_skip(), 
                                                                              porc_utilizaram_internet = col_skip(), 
                                                                              porc_estudam_e_ocupados_15_a_17 = col_skip()), 
                         trim_ws = TRUE)


#Juncao das 3 bases
dados2 <- dados |> 
  full_join(dados_ibge, by = c('name' = 'uf')) |> 
  full_join(alfabetizacao, by = c('name'= 'Estado'))

#Verificar se teve algum erro ou NA
dados2 <- dados2[!(dados2$morte_por_morte_total |> is.na()),] #Esqueci de remover os dados de alfab pra macro regioes

#save
dados2 |> saveRDS('dados/dados_com_inf_ibge.rds')

# Breve analise -----------------------------------------------------------
library(ggplot2)
library(reshape2)
library(corrplot)




# Modelagem ---------------------------------------------------------------
dados2$morte_por_morte_total <- dados2$morte_por_morte_total * 100
dados2$taxa_morte_total <- dados2$`numero de mortos` / dados2$habitantes * 10000
## Ajustando um modelo de regressão linear comum
fit_ols <- lm(
  (taxa_morte_total) ~`IDHM Educação` + `IDHM Longevidade`  + `IDHM Renda`,
  data = dados2
)
dados2
summary(fit_ols)
plot(fit_ols)

hist(fit_ols$residuals)
#vizinhos
nb <- poly2nb(dados2, queen=TRUE)

#atribuindo pesos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

## Verificando se os resíduos apresentam autocorrelação espacial
lm.morantest(fit_ols, lw, zero.policy = TRUE, alternative = "greater") 

## Buscando o modelo de regressão espacial mais adequado
lm.RStests(fit_ols, lw, test="RSerr", zero.policy = T) |>  summary()# spatial error

lm.RStests(fit_ols, lw, test="RSlag", zero.policy = T) # spatial lag model

lm.LMtests(fit_ols, lw, test=c("LMerr","RLMerr","LMlag","RLMlag"))
dados2 |> names()
## Ajustando o modelo de erros
fit_err <- spatialreg::errorsarlm(  
  (taxa_morte_total) ~  `IDHM Educação` + `IDHM Longevidade`  + `IDHM Renda`,
  data = dados2,
  listw = lw
)
summary(fit_err)
fit_lag <- spatialreg::lagsarlm(  
  (taxa_morte_total) ~ `IDHM Educação` + `IDHM Longevidade`  + `IDHM Renda`,
  data = dados2,
  listw = lw
)
summary(fit_lag)

fit_lm <- 
  
dados2$morte_por_residentes * 1000

# Supondo que 'dados2' seja o nome do seu dataframe
# Selecione apenas as colunas relevantes para a análise de correlação
dados_selecionados <- dados2[, c("num_casos", "numero de mortos", "morte_por_morte_total", 
                                 "habitantes", "IDHM", "IDHM Renda", "IDHM Educação", 
                                 "IDHM Longevidade", "morte_por_residentes", 
                                 "rend_domiciliar_per_capita_medio", "porc_pop_branca", 
                                 "porc_pop_preta", "rendimento_medio_real", "alfabetizados", 
                                 "nao_alfabetizados","taxa_morte_total")]

# Calcular a matriz de correlação
matriz_correlacao <- cor(dados_selecionados |> as.data.frame() |> select(-geometry), use = "complete.obs")

# Derreter a matriz de correlação para o formato longo
cor_melt <- melt(matriz_correlacao)

# Criar o heatmap usando ggplot2
heatmap <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlação") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap de Correlação")

# Exibir o heatmap
print(heatmap)

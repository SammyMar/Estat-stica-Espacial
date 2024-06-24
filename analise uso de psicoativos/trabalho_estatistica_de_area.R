library(dplyr)
library(munifacil)
library(tidyverse)
library(janitor)
library(ggplot2)
library(sf)
library(spdep)
library(RColorBrewer)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(leaflet)
# SIM- F10-19 -------------------------------------------------------------
sim <- microdatasus::fetch_datasus(2022,1,2022,12,information_system = 'SIM-DO')
vars <- c('CAUSABAS','CODMUNRES',
          'DTNASC', 'DTOBITO',
          'ESC2010','ESCFALAGR1',
          'IDADE','RACACOR',
          'SEXO')
write.csv(sim,'sim_completo.csv')

# Ajustar a coluna 'id_municipio' para remover o último dígito
depara_muni <- munifacil::depara_muni_codigo() |> 
  mutate(id_municipio = substr(id_municipio, 1, nchar(id_municipio) - 1)) |> 
  select(c('id_municipio','muni_join','uf_join')) 

# Identificar duplicados e manter apenas a primeira ocorrência
depara_muni_unico <- depara_muni |>
  group_by(id_municipio) |>
  slice(1) |>
  ungroup()


# Criar um mapeamento dos dois primeiros dígitos para as UFs
uf_map <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP", "17" = "TO",
  "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "50" = "MS", "51" = "MT", "52" = "GO", "53" = "DF"
)

# Adicionar a UF com base nos dois primeiros dígitos de CODMUNRES
sim_filtrado <- sim |>
  select(all_of(vars)) |>
  filter(CAUSABAS >= "F100" & CAUSABAS <= "F199") |>
  left_join(depara_muni_unico, by = c("CODMUNRES" = "id_municipio")) |>
  mutate(
    uf_join = ifelse(is.na(uf_join), uf_map[substr(CODMUNRES, 1, 2)], uf_join)
  )


# Agregando numero de casos por estado --------------------------------

var_resposta_agreg <- sim_filtrado |> 
  count(uf_join, name = "num_casos")

#Contando numero mortos total por estado
total_mortes <- sim  |> 
  select("CODMUNRES") |>
  left_join(depara_muni_unico, by = c("CODMUNRES" = "id_municipio")) |>
  mutate(
    uf_join = ifelse(is.na(uf_join), uf_map[substr(CODMUNRES, 1, 2)], uf_join)
  ) |> count(uf_join,name = 'numero de mortos')
var_resposta_agreg <- left_join(var_resposta_agreg,total_mortes)

#criando taxa de morte
var_resposta_agreg <- var_resposta_agreg |> 
  mutate(taxa_morte = (as.numeric(num_casos)  / as.numeric(`numero de mortos`) )* 1000)

# Grafico de taxa de morte por uso de drogas ------------------------------

# Obter o shapefile dos estados do Brasil
br_states <- st_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson")

# Verificar a estrutura do shapefile
str(br_states)

# Adicionar os dados de taxa de morte ao shapefile
br_states <- br_states %>%
  left_join(var_resposta_agreg, by = c("sigla" = "uf_join"))

# Criar o mapa usando ggplot2
mapa_taxa <- ggplot(data = br_states) +
  geom_sf(aes(fill = taxa_morte), color = 'white') +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Proporção de morte por uso de psicoativos para o ano de 2022",
       fill = "Morte por psicoativos x 1000\n--------------------------------------\n            Mortes totais",
       caption = "Dados obtidos pelo SIM") +
  theme_minimal() +
  theme(legend.position = "right")

mapa_taxa

# Criar a pasta se ela não existir
if (!dir.exists('graficos')) {
  dir.create('graficos')
}
ggsave("graficos/mapa_morte_psicoativos.png", mapa_taxa, width = 12, height = 8, dpi = 300)
# Indice de Moran global ---------------------------------------------------------
# Indice global
par(mfrow = c(1,2))
br_states$taxa_morte |> boxplot()
br_states$taxa_morte |> hist()

#criando os vizinhos
# Corrigir geometrias inválidas
br_states <- st_make_valid(br_states)
nb <- poly2nb(br_states, queen=TRUE)

#atribuindo pesos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
inc.lag <- lag.listw(lw, br_states$taxa_morte)
inc.lag
# Gráfico de dispersão com linha de regressão
plot(inc.lag ~ taxa_morte, pch = 16, asp = 1,
     main = "Relação entre taxa de morte e Lag",  # Título do gráfico
     xlab = "Taxa de morte",                         # Rótulo do eixo x
     ylab = "Lag",data = br_states)                               # Rótulo do eixo y

# Adicionar linha de regressão
abline(lm(inc.lag ~ taxa_morte,data = br_states), col = "blue")
# Salvar o gráfico em um arquivo PNG
png("graficos/grafico_taxa_morte_inc_lag.png", width = 800, height = 600, units = "px", res = 300)
dev.off()
#indice de moran
I <- moran(br_states$taxa_morte, lw, length(nb), Szero(lw))[1]
I
#metodo analitico
moran.test(br_states$taxa_morte,lw, alternative="greater", zero.policy=TRUE) 

#metodo de monte carlo
MC<- moran.mc(br_states$taxa_morte, lw, nsim = 999, alternative = "greater", zero.policy=TRUE)
plot(MC, xlab="Indice de Moran")
# Salvar o gráfico em um arquivo PNG
png("graficos/Indice_de_moran_MC.png", width = 800, height = 600, units = "px", res = 300)
dev.off()
# indice de moran local ---------------------------------------------------
local <- localmoran(x = br_states$taxa_morte, listw = lw)
moran.map <- cbind(br_states, local)
quadrant <- vector(mode="numeric", length=nrow(local))

# Centraliza a variável de interesse em torno de sua média
m.qualification <- br_states$taxa_morte - mean(br_states$taxa_morte)

# Centraliza o Moran Local em torno da média
m.local <- local[,1] - mean(local[,1])

# Limite de significância
signif <- 0.05

# Constrói o quadrante de dados
quadrant[m.qualification > 0 & m.local > 0] <- 4
quadrant[m.qualification < 0 & m.local < 0] <- 1
quadrant[m.qualification < 0 & m.local > 0] <- 2
quadrant[m.qualification > 0 & m.local < 0] <- 3
quadrant[local[,5] > signif] <- 0

# Configuração das quebras e cores
brks <- c(0, 1, 2, 3, 4)
colors <- viridisLite::magma(5)
# Cria o gráfico
Moran_local <- ggplot() +
  geom_sf(data = br_states, aes(fill = as.factor(quadrant)), color = 'white') +
  scale_fill_manual(values = colors,
                    breaks = as.character(brks),
                    labels = c("Não significativo", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Alto-Alto"),
                    name = "Quadrantes") +
  labs(title = "Indice de Moran Local",
       subtitle = "Taxa de Morte por Psicoativos") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title.position = "top", label.position = "right")) 


ggsave("graficos/mapa_moran_local.png", Moran_local, width = 12, height = 8, dpi = 300)


# Correlacao entre Y e covariaveis ----------------------------------------


# Save dos dados ----------------------------------------------------------

# Nome da pasta
pasta <- "dados"

# Criar a pasta se ela não existir
if (!dir.exists(pasta)) {
  dir.create(pasta)
}

saveRDS(br_states,"br_states.rds")
write.csv(sim_filtrado,"sim_filtrado.csv")

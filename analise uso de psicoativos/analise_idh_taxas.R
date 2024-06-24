library(readxl)
library(dplyr)
library(sf)
library(spdep)
library(ggplot2)
library(GGally)
# Leitura da base ---------------------------------------------------------
setwd('analise uso de psicoativos')
br_states <- readRDS('dados/br_states.rds')
idhm <- read_excel("idhm.xlsx", col_types = c("text", 
                                              "skip", "numeric", "skip", "numeric", 
                                              "skip", "numeric", "skip", "numeric"))
residentes <- read_excel("residentes.xlsx", 
                         skip = 3) 
residentes <- residentes[-1, ]

# Juncao das bases --------------------------------------------------------

idhm_habitandes <- residentes |> 
  full_join(idhm, by = c('Brasil e Unidade da Federação' = 'Territorialidade'   ))
colnames(idhm_habitandes)[4] <- 'habitantes'
idhm_habitandes <- idhm_habitandes[!(idhm_habitandes$IDHM |> is.na()),]

base_final <- br_states |> 
  full_join(idhm_habitandes, by = c('codigo_ibg' = 'Cód.'))
base_final<- base_final |> 
  select(-c(`Brasil e Unidade da Federação`, id, regiao_id,cartodb_id,Nível, created_at, updated_at))|> 
  mutate(
    morte_por_residentes = (num_casos/habitantes) * 1000
  ) |>  rename(morte_por_morte_total = taxa_morte)
lula <- c(
  "11" =  29.34 , "12" = 29.70, "13" = 51.10, "14" = 23.92, "15" = 54.75, "16" = 48.64, "17" = 51.35,
  "21" = 71.14, "22" = 76.86, "23" = 69.97, "24" = 65.10, "25" = 66.62, "26" = 66.93, "27" = 58.68, "28" = 67.21, "29" = 72.12,
  "31" = 50.20, "32" = 41.96, "33" = 43.47, "35" = 44.76,
  "41" = 37.60, "42" = 30.73, "43" = 43.65,
  "50" = 40.51, "51" = 34.92, "52" = 41.29, "53" = 41.19
)
# Criando o vetor lula como um data frame
lula <- data.frame(
  codigo_ibg = as.character(names(lula)),
  lula = as.numeric(lula)
)

# Unindo o data frame base_final com o data frame lula
base_final <- base_final |>
  left_join(lula, by = 'codigo_ibg')

saveRDS(base_final, 'dados/base_final.rds')

# Correlacao --------------------------------------------------------------
colnames(base_final)
plot_data <- base_final[, c("morte_por_morte_total", "morte_por_residentes", "IDHM", "IDHM Renda", "IDHM Educação", "IDHM Longevidade")]

# Gráficos de dispersão individuais com linhas de tendência
g1 <- ggplot(base_final, aes(x = IDHM, y = morte_por_morte_total)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("IDHM") +
  theme_light() +
  theme(axis.title.y = element_blank())

g2 <- ggplot(base_final, aes(x = `IDHM Renda`, y = morte_por_morte_total)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("IDHM Renda") +
  theme_light() +
  theme(axis.title.y = element_blank())

g3 <- ggplot(base_final, aes(x = `IDHM Educação`, y = morte_por_morte_total)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("IDHM Educação") +
  theme_light() +
  theme(axis.title.y = element_blank())

g4 <- ggplot(base_final, aes(x = `IDHM Longevidade`, y = morte_por_morte_total)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("IDHM Longevidade") +
  theme_light() +
  theme(axis.title.y = element_blank())

g5 <- ggplot(base_final, aes(x = IDHM, y = morte_por_residentes)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("IDHM") +
  theme_light() +
  theme(axis.title.y = element_blank())

g6 <- ggplot(base_final, aes(x = `IDHM Renda`, y = morte_por_residentes)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("IDHM Renda") +
  theme_light() +
  theme(axis.title.y = element_blank())

g7 <-   ggplot(base_final, aes(x = `IDHM Educação`, y = morte_por_residentes)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("IDHM Educação") +
  theme_light() +
  theme(axis.title.y = element_blank())

g8 <- ggplot(base_final, aes(x = `IDHM Longevidade`, y = morte_por_residentes)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("IDHM Longevidade") +
  theme_light() +
  theme(axis.title.y = element_blank())
g9 <- ggplot(base_final, aes(x =lula, y = morte_por_morte_total)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle(" Taxa de Morte por Morte Total x votos no lula") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"))
g10 <- ggplot(base_final, aes(x =lula, y = morte_por_residentes)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Taxa de Morte por Habitantes x votos no lula") +
  theme_light()  +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"))
ggpubr::ggarrange(g1,g2,g3,g4)
ggpubr::ggarrange(g5,g6,g7,g8)
ggpubr::ggarrange(g9,g10)
# Para morte_por_morte_total vs outras variáveis
cor_test1 <- cor.test(base_final$morte_por_morte_total, base_final$IDHM, method = 'spearman')
cor_test2 <- cor.test(base_final$morte_por_morte_total, base_final$`IDHM Renda`, method = 'spearman')
cor_test3 <- cor.test(base_final$morte_por_morte_total, base_final$`IDHM Educação`, method = 'spearman')
cor_test4 <- cor.test(base_final$morte_por_morte_total, base_final$`IDHM Longevidade`, method = 'spearman')
cor_test9 <- cor.test(base_final$morte_por_morte_total, base_final$lula, method = 'spearman')

# Para morte_por_residentes vs outras variáveis
cor_test5 <- cor.test(base_final$morte_por_residentes, base_final$IDHM, method = 'spearman')
cor_test6 <- cor.test(base_final$morte_por_residentes, base_final$`IDHM Renda`, method = 'spearman')
cor_test7 <- cor.test(base_final$morte_por_residentes, base_final$`IDHM Educação`, method = 'spearman')
cor_test8 <- cor.test(base_final$morte_por_residentes, base_final$`IDHM Longevidade`, method = 'spearman')
cor_test10 <- cor.test(base_final$morte_por_residentes, base_final$lula, method = 'spearman')
# Criar uma tabela com os resultados
results <- data.frame(
  `Variavel Resposta` = c("Por morte total", "Por morte total", "Por morte total", "Por morte total","Por morte total",
                "Por Residente", "Por Residente", "Por Residente", "Por Residente", "Por Residente"),
  Covariavel = c("IDHM", "IDHM Renda", "IDHM Educação", "IDHM Longevidade","porc. votos lula",
                "IDHM", "IDHM Renda", "IDHM Educação", "IDHM Longevidade","porc. votos lula"),
  Correlacao = c(cor_test1$estimate, cor_test2$estimate, cor_test3$estimate, cor_test4$estimate,cor_test9$estimate,
                  cor_test5$estimate, cor_test6$estimate, cor_test7$estimate, cor_test8$estimate,cor_test10$estimate),
  `p-value` = c(cor_test1$p.value, cor_test2$p.value, cor_test3$p.value, cor_test4$p.value, cor_test9$p.value,
              cor_test5$p.value, cor_test6$p.value, cor_test7$p.value, cor_test8$p.value,cor_test10$p.value)
)
# Exibir a tabela no Viewer do RStudio
datatable(results, options = list(pageLength = 10, autoWidth = TRUE))

g1 <- ggplot(data = base_final) +
  geom_sf(aes(fill = IDHM), color = 'white') +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "IDHM para o ano de 2022",
       fill = "IDHM") +
  geom_label(aes(label = round(IDHM, 2), geometry = geometry),
             size = 3, fontface = "bold", stat = 'sf_coordinates', label.padding = unit(0.15, "lines")) +
  theme_minimal() +
  theme(legend.position = "right")
g2 <- ggplot(data = base_final) +
  geom_sf(aes(fill = IDHM), color = 'white') +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "",
       fill = "IDHM") +
  theme_minimal() +
  theme(legend.position = "right")
ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1,common.legend = T,legend = 'right')

g1 <- ggplot(data = base_final) +
  geom_sf(aes(fill = morte_por_morte_total), color = 'white') +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Numero de casos por mortes totais x 1000",
       fill = "taxa") +
  theme_minimal() +
  theme(legend.position = "right")
g2 <- ggplot(data = base_final) +
  geom_sf(aes(fill = morte_por_residentes), color = 'white') +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Numero de casos por residentes x 1000",
       fill = "taxa") +
  theme_minimal() +
  theme(legend.position = "right")
ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1,common.legend = F,legend = 'bottom')

# Indice de Moran global ---------------------------------------------------------
# Indice global
#criando os vizinhos
# Corrigir geometrias inválidas
base_final <- st_make_valid(base_final)
nb <- poly2nb(base_final, queen=TRUE)

#atribuindo pesos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
inc.lag <- lag.listw(lw, base_final$morte_por_residentes)
inc.lag
# Gráfico de dispersão com linha de regressão
plot(inc.lag ~ morte_por_residentes, pch = 16, asp = 1,
     main = "Casos por Residentes e Lag",  # Título do gráfico
     xlab = "Taxa de morte",                         # Rótulo do eixo x
     ylab = "Lag",data = base_final)                               # Rótulo do eixo y

# Adicionar linha de regressão
abline(lm(inc.lag ~ morte_por_residentes,data = base_final), col = "blue")
# Salvar o gráfico em um arquivo PNG
png("graficos/grafico_taxa_morte_inc_lag.png", width = 800, height = 600, units = "px", res = 300)
dev.off()
#indice de moran
I <- moran(base_final$morte_por_residentes, lw, length(nb), Szero(lw))[1]
I
#metodo analitico
moran.test(base_final$morte_por_morte_total,lw, alternative="greater", zero.policy=TRUE) 

#metodo de monte carlo
MC<- moran.mc(base_final$morte_por_residentes, lw, nsim = 999, alternative = "greater", zero.policy=TRUE)
plot(MC, xlab="Indice de Moran")
# Salvar o gráfico em um arquivo PNG
png("graficos/Indice_de_moran_MC.png", width = 800, height = 600, units = "px", res = 300)
dev.off()
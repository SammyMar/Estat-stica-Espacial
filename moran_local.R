# Calcula o índice de Moran Local
local <- localmoran(x = br_states$taxa_morte, listw = lw)
moran.map <- cbind(br_states, local)

# Cria a coluna 'quadrant' para os quadrantes
quadrant <- vector(mode = "numeric", length = nrow(local))

# Centraliza a variável de interesse em torno de sua média
m.qualification <- br_states$taxa_morte - mean(br_states$taxa_morte)

# Centraliza o Moran Local em torno da média
m.local <- local[, 1] - mean(local[, 1])

# Limite de significância
signif <- 0.05

# Constrói o quadrante de dados
quadrant[m.qualification > 0 & m.local > 0] <- 4
quadrant[m.qualification < 0 & m.local < 0] <- 1
quadrant[m.qualification < 0 & m.local > 0] <- 2
quadrant[m.qualification > 0 & m.local < 0] <- 3
quadrant[local[, 5] > signif] <- 0

# Adiciona os quadrantes ao data frame
moran.map$quadrant <- as.factor(quadrant)

# Configuração das quebras e cores
brks <- c(0, 1, 2, 3, 4)
colors <- viridisLite::magma(5)

# Cria o gráfico
Moran_local <- ggplot(data = moran.map) +
  geom_sf(aes(fill = quadrant), color = 'white') +
  scale_fill_manual(values = colors,
                    breaks = as.character(brks),
                    labels = c("Não significativo", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Alto-Alto"),
                    name = "Quadrantes") +
  geom_label(aes(label = round(local[, 1], 2), geometry = geometry),
             size = 3, fontface = "bold", stat = 'sf_coordinates', label.padding = unit(0.15, "lines")) +
  labs(title = "Índice de Moran Local",
       subtitle = "Taxa de Morte por Psicoativos") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title.position = "top", label.position = "right"))

# Exibe o gráfico
print(Moran_local)

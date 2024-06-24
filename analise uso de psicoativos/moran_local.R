# Calcula o índice de Moran Local
local <- localmoran(x = base_final$morte_por_morte_total, listw = lw)
moran.map <- cbind(base_final, local)

# Cria a coluna 'quadrant' para os quadrantes
quadrant <- vector(mode = "numeric", length = nrow(local))

# Centraliza a variável de interesse em torno de sua média
m.qualification <- base_final$morte_por_morte_total - mean(base_final$morte_por_morte_total)

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
  labs(title = "Casos Por numero de mortes totais") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", label.position = "right"))

# Exibe o gráfico
print(Moran_local)

# Calcula o índice de Moran Local
local <- localmoran(x = base_final$morte_por_residentes, listw = lw)
moran.map <- cbind(base_final, local)

# Cria a coluna 'quadrant' para os quadrantes
quadrant <- vector(mode = "numeric", length = nrow(local))

# Centraliza a variável de interesse em torno de sua média
m.qualification <- base_final$morte_por_residentes - mean(base_final$morte_por_residentes)

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
  labs(title = "Casos Por Numero de Residentes") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", label.position = "right"))

# Exibe o gráfico
print(Moran_local)

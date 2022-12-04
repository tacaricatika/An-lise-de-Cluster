#Pacotes necessários

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando e formatando a tabela que será utilizada

paises <- read.csv("paises.csv")
rownames(paises) <- paises$Name 
paises <- paises[2:5]
View(paises)
paises <- na.omit(paises)
paises$LifeExpectancy <- as.numeric(paises$LifeExpectancy)
summary(paises)

#Aplicando o z-score para a padronização das variáveis

zpaises <- as.data.frame(scale(paises[1:4]))
view(zpaises)
round(mean(zpaises$SurfaceArea, 2))#Testando se a média é 0
round(sd(zpaises$SurfaceArea, 2))#Testando se o SD é 1

#Aplicando a matriz de dissimilaridade (usaremos o método Euclidiano)

matriz_D <- zpaises %>%
  dist(method = 'euclidian')
  
#Aplicando agora o melhor metodo de encadeamento para agrupamentos hierárquico

cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

#Apreciando o Dendrograma

fviz_dend(x = cluster_hier_complete, show_labels = F)

# Dendrograma com visualização dos clusters (selecionando por "altura")
  
fviz_dend(x = cluster_hier_complete,
          h = 3.9,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())

## Formam 10 clusters cortando o dendrograma em 3.9

# Vamos detalhar esse esquema hierárquico

coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 12 clusters

paises$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 10))
zpaises$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 10))

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_SurfaceArea <- aov(formula = SurfaceArea ~ cluster_H,
                                data = zpaises))

summary(anova_Population <- aov(formula = Population ~ cluster_H,
                             data = zpaises))

summary(anova_LifeExpectancy <- aov(formula = LifeExpectancy ~ cluster_H,
                            data = zpaises))

summary(anova_GNP <- aov(formula = GNP ~ cluster_H,
                             data = zpaises))

# O que os cluster indicam? Vamos interpretar algumas variáveis médias:

análise <- group_by(paises, cluster_H) %>%
  summarise(SurfaceArea = mean(SurfaceArea, na.rm = TRUE),
            Population = mean(Population, na.rm = TRUE),
            LifeExpectancy = mean(LifeExpectancy, na.rm = TRUE),
            GNP = mean(GNP, na.rm = TRUE))

print(análise)

#Exportando o banco de dados

write.table(paises, file= "paises com agrupados.csv", sep=";", dec=".")

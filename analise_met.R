##Importando bibliotecas

library(randomNames)
library(psych)
library(ggplot2)
library(ggfortify)

## Criando dataset
#nomes <- randomNames(300, which.names = "first")

dados <- matrix(data = trunc(runif(37 * 300, min = -3, max = 3)), nrow = 300, ncol = 37)
colnames(dados) <- paste("P", 1:37, sep="")
rownames(dados) <- paste("a",1:300, sep = "")

## alpha Cronbach

psych::alpha(dados)

## Teste de Kaiser-Meyer-Oklin (KMO)

KMO(dados)

## Teste de Bartlett

cortest.bartlett(dados)

## PCA

pca <- prcomp(dados, scale = TRUE)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, xlab = "Componentes Principais", ylab = "Porcentagem de Variacao")


##### Plot PCA

x11()
autoplot(prcomp(dados), data = dados, loadings = T, loadings.label = T, loadings.colour = "Blue")

###### Seus dados

Ray <- c(-2, -2, 2,2,1,-1,-2,-1,1,-2,0,-2,-1,0,2,2,2,2,2,-2,-2,2,2,1,-1,-2,-1,1,-2,2,1,-1,-2,-1,1,-2,2)

novos.dados <- rbind(Ray, dados)

x11()
autoplot(prcomp(novos.dados), data = novos.dados, label = T, loadings = T, loadings.colour = "Blue")



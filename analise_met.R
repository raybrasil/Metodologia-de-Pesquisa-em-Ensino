##Importando bibliotecas

library(psych)
library(ggplot2)
library(ggfortify)

## Criando dataset

dados <- matrix(data = -2:2, nrow = 300, ncol = 38)
colnames(dados) <- c(
  paste("P", 1:37, sep=""),
  paste("Nomes", sep = " "))
rownames(dados) <- paste("a", 1:300, sep = " ")

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
barplot(pca.var.per, xlab = "Componentes Principais", ylab = "Porcentagem de variação")


##### Plot PCA

autoplot(prcomp(dados), data = dados, colour = "Nomes", loadings = T, 
         loadings.label = T, loadings.colour = "Blue")

#pca.plot <- plot(pca$x[,1], pca$x[,2])

##ggplot(data = dados.pca, aes(x = X, y = Y, label = Sample)) +
##  geom_text()+
##  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = ""))+
##  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = ""))


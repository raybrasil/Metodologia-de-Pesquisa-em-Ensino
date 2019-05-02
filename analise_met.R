##Importando bibliotecas

library(psych)
library(ggplot2)
library(ggfortify)

## Criando dataset

cria.dataset <- function(perguntas, alunos){ 
  
  dados <- matrix(data = trunc(runif(perguntas * alunos, min = -3, max = 3)), nrow = alunos, ncol = perguntas)
  colnames(dados) <- c(
    paste("P", 1:perguntas, sep=""))
  rownames(dados) <- paste("a", 1:alunos, sep = " ")
  return(dados)
}

dados <- cria.dataset(38, 300)

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

##Plots secundários

#pca.plot <- plot(pca$x[,1], pca$x[,2])

##ggplot(data = dados.pca, aes(x = X, y = Y, label = Sample)) +
##  geom_text()+
##  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = ""))+
##  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = ""))


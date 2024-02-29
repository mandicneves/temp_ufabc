# Ex1 ####

# Cria os vetores (i.e. as variaveis) com seus valores

prov <- c("Mittelfranken", "Niderbayern", "Oberfranken", 
          "Oberpfalz", "Schunben", "Unterfranken")
mort <- c(250, 320, 170, 300, 270, 190)

amat <- c(60, 30, 90, 60, 40, 90)

# Cria um banco de dados com os vetores acima
bd <- data.frame(prov, mort, amat)
# carrega um banco de dados chamado `dados`localizado na pasta raiz da sessao do R
dados <- read.table("dados", header = TRUE)
# Abre a documentacao
?read.table

# Ex1.a ####

# carregar pacote
library(ggplot2)
# criando grafico
g1 <- ggplot(data = dados,
             mapping = aes(x = Amamentação, y = Mortalidade)) + 
  geom_point()
# plota o grafico
g1
plot(g1)

# estatisticas descritivas
summary(dados)

# Ex1.b ####

# criando modelo de regressao linear (lm)
reg <- lm(formula = Mortalidade ~ Amamentação, data = dados)
# abre documentacao da funcao lm
?lm
# abre o sumario da regressao
summary(reg)

# ajustando linha da regressao
g2 <- g1 + geom_smooth(method = "lm")
g2
# abre a documentacao da funcao
?geom_smooth
# novos dados
pais <- data.frame(Província = c("Oberbayern", "Pfalz"),
                   Amamentação = c(37, 85))
# previsao para os novos dados
predict(reg, pais)

# Ex1.c ####

# Ex2
































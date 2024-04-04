# Ex1 ####

prov <- c("Mittelfranken", "Niderbayern", "Oberfranken", "Oberpfalz", "Schunben", "Unterfranken", "Média")
mort <- c(250, 320, 170, 300, 270, 190, 250)
amem <- c(60, 30, 90, 60, 40, 80, 60)

dados <- data.frame(prov, mort, amem)
dados <- read.table("./tx_mort", header = TRUE)

# ex1.a) ####

# biblioteca para grafico
library(ggplot2)
p <- ggplot(data = dados,
            mapping = aes(x = Mortalidade, Amamentação)) + 
  geom_point()

# ex1.b) ####

# algumas estatisticas
summary(dados)
# covariancia
cov(dados$Mortalidade, dados$Amamentação)
# correlacao
cor(dados$Mortalidade, dados$Amamentação)
# regressao linear simples
regressao <- lm(Mortalidade ~ Amamentação, data = dados)
summary(regressao)
# biblioteca para impressao dos resultados da regressao
#install.packages("stargazer")
library(stargazer)
stargazer(regressao, type = "text")
# grafico com linha de regressao
p + geom_smooth(method = "lm", se = FALSE)
# calculando previsao para outras provincias
pais <- data.frame(Província = c("Oberbayern", "Pfalz"), Amamentação = c(37, 85))
predict(regressao, pais)

# ex1.c) ####

stargazer(regressao, type = "text")

# EX.2 ####

dados <- as.data.frame(t(read.table("./batimento")))
names(dados) <- c("Idade", "Batimento")

regressao <- lm(Batimento ~ Idade, data = dados)
summary(regressao)
stargazer(regressao, type = "text")

novos_dados <- data.frame(Idade = c(10, 76, 234, 5, 12, 34))
predict(regressao, novos_dados)

# LISTA_02 ####

# Exercicio_01 ####

# instala o pacote
install.packages("UsingR")
# carrega o pacote, as duas funcoes fazem a mesma coisa
require(UsingR)
library(UsingR)
# carrega a base de dados
data("father.son")

# cria regresao
reg <- lm(sheight ~ fheight, data = father.son)
# infos da regressao
summary(reg)

# cria um grafico
library(ggplot2)
g <- ggplot(data = father.son,
       mapping = aes(x = fheight, y = sheight))
# adiciona camadas ao grafico
g + geom_point() + 
  geom_smooth(method = "lm")

# Exercicio_02 ####

# centra os valores dos filhos a media e adiciona a coluna sc
father.son$sc <- father.son$sheight - mean(father.son$sheight)
# centra os valores dos pais a media e adiciona a coluna sc
father.son$fc <- father.son$fheight - mean(father.son$fheight)

# cria modelo de regressao
regc <- lm(sc ~ fc, data = father.son)

# instala pacote de apresentacao de resultados de regressao
install.packages("stargazer")
# carrega o pacote
library(stargazer)
# resultados da regressao no formato de tabela
stargazer(reg, regc, type = "text")

# Exercicio_03 ####


# Normalizar os valores: xi - xbarra / dp(x)
# criar uma coluna com os valores normalizados dos filhos
father.son$sn <- father.son$sc / sd(father.son$sheight)
# criar uma coluna com os valores normalizados dos pais
father.son$fn <- father.son$fc / sd(father.son$fheight)

# regressao normalizada
regn <- lm(sn ~ fn, data = father.son)

# Exercicio_04 ####
# para predizer um valor, ele precisa estar no formato de data.frame
# por isso o codigo data.frame(fheight = c(63))
predict(reg, data.frame(fheight = c(63)))

















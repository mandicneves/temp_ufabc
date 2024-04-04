# Ex_01 ####

# instalando pacote
# install.packages("UsingR")
# carregando pacote na sessao
library(UsingR)
library(stargazer)
# carregando base de dados
data("father.son")

# criando modelo de regressao linear
reg <- lm(sheight ~ fheight, data = father.son)
# infos sobre a regressao
summary(reg)

# criando grafico
library(ggplot2)
p <- ggplot(data = father.son,
              mapping = aes(x = fheight, y = sheight))
p + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "red")

# fazendo na mao
x <- father.son$fheight
y <- father.son$sheight
b1 <- cor(x, y) * sd(y) / sd(x)
b0 <- mean(y) - (b1 * mean(x))
rbind(coef(reg), c(b0, b1))

# Ex_02 ####

xc <- x - mean(x)
yc <- y - mean(y)
regc <- lm(yc ~ xc + 0 )

stargazer(reg, regc, type = "text")

# Ex_03 ####

xn <- xc / sd(x)
yn <- yc / sd(y)
regn <- lm(yn ~ xn + 0)

plot(density(xn))


stargazer(reg, regn, type = "text")

# Ex_04 ####

predict(reg, data.frame(fheight = c(63)))

# Ex_05 ####

# sd(y) = 2 sd(x) =>
# sd(y) / sd(x) = 2
0.3 * 2

# Ex_06 ####

1 - (0.6 * 0.5)

# Ex_07 ####

# Verdade

# Ex_08 ####

# sd(y) 2 =  sd(x) =>
# sd(y) / sd(x) = 1/2
0.3 * 0.5

# EX 1 ####

data("Seatbelts") # carregando base de dados
Seatbelts <- as.data.frame(Seatbelts) # transformando em um dataframe
View(Seatbelts) # visualizando dados

modelo <- lm(DriversKilled ~ kms + PetrolPrice, data = Seatbelts) # criando modelo
summary(modelo) # resumo do modelo
round(summary(modelo)$coef, 4) # coeficientes arredondados

# Interpretacao dos resultados


# Ex 2 ####

summary(Seatbelts$kms) # na maior faixa, os dados estao na casa das dezenas de milhares
summary(Seatbelts$PetrolPrice) # unidades reais, precos relativos a algum indice

# criando modelo com todas as variaveis padronizadas
modelo_std <- lm(DriversKilled ~ kms + PetrolPrice, data = data.frame(scale(Seatbelts)))
round(summary(modelo_std)$coef, 4)

# modelo com as variaveis independentes padronizadas
round(summary(lm(DriversKilled ~ scale(kms) + scale(PetrolPrice), data = Seatbelts))$coef, 4)

# usando uma biblioteca para fazer a padronizacao
library(lm.beta)
round(summary(lm.beta(modelo))$coef, 4)

# Ex 3 ####

# predicao do modelo para kms = media de kms e petrolprice = media de petrolprice
predict(modelo, data.frame
        (kms = mean(Seatbelts$kms), PetrolPrice = mean(Seatbelts$PetrolPrice)))

# Ex 4 ####

# salvando as variaveis de interesse em novos objetos
dk <- Seatbelts$DriversKilled
kms <- Seatbelts$kms
pp <- Seatbelts$PetrolPrice

# regressao de driverskilled por kms
residuos_dk <- resid(lm(dk ~ kms))
# regressao de petrolprice por kms
residuos_pp <- resid(lm(pp ~ kms))
# comparando resultdos
summary(lm(residuos_dk ~ residuos_pp + 0))
round(summary(modelo)$coef, 1)

# pq? o modo como a regressao realiza o ajuste remove a associacao linear da variavel constante, removendo seu impacto em todo o resto, no resultado e nos outros regressores 
# podemos pensar esse efeito como pegar os residuos e, uma vez removidos, ajustar o modelo de regressao apenas com os residuos

# Ex 5 ####

# regressao de driverskilled por petrolprice
residuos_dk <- resid(lm(dk ~ pp))
# regressao de petrolprice por petrolprice
residuos_kms <- resid(lm(kms ~ pp))
# comparando resultdos
summary(lm(residuos_dk ~ residuos_kms + 0))
round(summary(modelo)$coef, 7)




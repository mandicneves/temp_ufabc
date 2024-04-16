# PARTE_A ###

# PARTE_B ####

# EX1 ####

prestigio <- read.table("https://www.john-fox.ca/AppliedRegression/datasets/Prestige.txt")
str(prestigio)
View(prestigio_sem_na)
prestigio$type <- as.factor(prestigio$type)

library(dplyr)
library(tidyr)
library(tidyverse)

 prestigio_sem_na <- prestigio %>% 
  drop_na() %>%
  mutate(prof = as.numeric(type == "prof"),
         wc = as.numeric(type == "wc"),
         bc = as.numeric(type == "bc"))


model <- lm(prestige ~ income + education + type, data = prestigio_sem_na)
summary(model)

library(car)
linearHypothesis(model, c("typeprof = 0", "typewc = 0"))

# equacao bc: (-0.62) + 0.001*income + 3.67*education
# equacao wc: (-0.62 - 2.74) + 0.001*income + 3.67*education
# equacao prof: (-0.62 + 6.04) + 0.001*income + 3.67*education

# EX2 ####




model_i <- lm(prestige ~ (income + education) * type, data = prestigio_sem_na)
round(summary(model_i)$coef, 4)

# equacao bc: (2.27) + (0.0035)*income + (1.7133)*education
# equacao wc: (2.27 - 33.53) + (0.0035 - 0.0021)*income + (1.7133 + 4.2909)*education
# equacao prof: (2.27 + 15.35) + (0.0035 - 0.0029)*income + (1.7133 + 1.3878)*education

linearHypothesis(model_i, c("income = 0", "income:typeprof = 0"))

lm(prestige ~ (income + education) * prof, data = prestigio_sem_na)

# equacao prof: (-4.08 + 21.71) + (0.0026 - 0.002)*income + (3.006 + 0.095)*education

# EX3 ####

# install.packages("effects")
library(effects)

efeito_renda <- effect("income:type", model_i)
plot(efeito_renda, multiline = T, rug = T, main = "Efeito da Interacção entre Income e Type")
efeito_educ <- effect("education:type", model_i)
plot(efeito_educ, multiline = T, rug = F, main = "Efeito da Interacção entre Education e Type")

prestigio_sem_na %>% 
  group_by(type) %>%
  summarise(qtde = n(),
            media_prestigio = mean(prestige),
            media_renda = mean(income), 
            media_educ = mean(education)) %>%
  arrange(media_prestigio)



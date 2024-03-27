library(tidyverse)

# EXERCICIO 1

# 1.1 Importando base de dados ####

giss_nasa <- read.csv("./dados/giss_nasa.csv", skip = 1, na.strings = "***")
View(giss_nasa)

# 1.2 Criando grafico de linhas para um mes especifico ####

ggplot(giss_nasa, aes(x = Year, y = Feb)) + geom_line()


# 1.3 Criando grafico de linhas para estacao ####

giss_nasa %>% 
  select(c(Year, DJF, MAM, JJA, SON)) %>%
  pivot_longer(cols = -Year, names_to = "Meses", values_to = "Temp") %>%
  ggplot(aes(x = Year, y = Temp, color = Meses)) + geom_line()

# 1.4 Criando grafico de linhas para temp media anual ####


ggplot(data = giss_nasa,aes(x = Year, y = J.D, color = J.D)) + 
  geom_line(linewidth = 0.5) + 
  scale_color_gradient(low = "darkblue", high = "red", guide = "none") + 
  labs(x = "Ano", y = "Desvio em relação\nà temperatura média (ºC)") + 
  theme_bw() + 
  theme(panel.grid.minor.y  = element_line(colour = "gray")) + 
  geom_hline(yintercept = mean(giss_nasa$J.D, na.rm = T), linetype = "dashed", color = "black") + 
  geom_text(aes(x = 2010, y = -0.1, label = "Média de Temperatura\n1880 - 2014"), color = "black")

# 1.5 Qual sua opinião?

# 1.6 Discutir os três gráficos

# 1.7 Comparar graficos

# EXERCICIO 2 ####

# 2.1 ####

intervalos <- round((seq(-0.3, 1.05, 0.05)), 2)
rotulos <- as.character(intervalos[1:27])

giss_nasa %>%
  filter(Year >= 1981 & Year <= 2010) %>%
  select(JJA) %>%
  unlist() %>%
  cut(breaks = intervalos, labels = rotulos) %>%
  table() %>% view()

# 2.2 ####

ggplot(data = giss_nasa,
       mapping = aes(x = JJA)) +
  geom_histogram(col = "black", binwidth = 0.05)

giss_nasa %>%
  filter(Year >= 1951 & Year <= 2010) %>%
  mutate(periodo = ifelse(.$Year >= 1951 & .$Year <= 1980, "1951-1980", "1981-2010")) %>%
  ggplot(aes(x = JJA)) + 
  geom_histogram(aes(fill = periodo), color = "Black", binwidth = 0.05, alpha = 0.4)

# 2.3 ####

giss_nasa %>%
  filter(Year >= 1951 & Year <= 1980) %>%
  select(Year, Jun, Jul, Aug) %>%
  pivot_longer(cols = -Year, names_to = "Meses", values_to = "Temp") %>%
  ggplot(aes(x = Temp)) + 
  geom_density(aes(fill = Meses), alpha = 0.4)

giss_nasa %>%
  filter(Year >= 1981 & Year <= 2010) %>%
  select(Year, Jun, Jul, Aug) %>%
  pivot_longer(cols = -Year, names_to = "Meses", values_to = "Temp") %>%
  ggplot(aes(x = Temp)) + 
  geom_density(aes(fill = Meses), alpha = 0.4)

# 2.4 ####



# 2.5 ####

quantile(giss_nasa$JJA, na.rm = T, seq(0, 1, 0.1))

giss_nasa %>%
  filter(Year >= 1951 & Year <= 1980) %>%
  select(JJA) %>%
  unlist() %>%
  quantile(seq(0, 1, 0.1))

# 2.6 ####

giss_nasa %>%
  filter(Year >= 1981 & Year <= 2010) %>%
  select(JJA) %>%
  unlist() %>%
  quantile(seq(0, 1, 0.1))

# 2.7 ####

estatisticas <- list(media = mean, mediana = median, variancia = var)

giss_nasa %>%
  filter(Year >= 1921 & Year <= 1950) %>%
  summarise_at(vars(DJF:SON), estatisticas) %>%
  mutate(periodo = "1921-1950") %>%
  bind_rows(
    giss_nasa %>%
      filter(Year >= 1951 & Year <= 1980) %>%
      summarise_at(vars(DJF:SON), estatisticas) %>%
      mutate(periodo = "1951-1980") 
  ) %>% 
  gather(estacao, valor, -periodo) %>%
  separate(estacao, into = c("estacao", "estatistica"), sep = "_") %>%
  mutate(valor = round(valor, 3)) %>%
  spread(estatistica, valor)

# EXERCICIO 3

co2 <- readxl::read_xlsx("./dados/Carbon_Emissions.xlsx")

library(janitor)

co2 <- readxl::read_xlsx("./dados/Carbon_Emissions.xlsx") %>% 
  clean_names()

# 3.1

# 3.2

# 3.3 ####

co2 %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = trend), col = "red") + 
  geom_line(aes(y = interpolated), col = "darkblue", alpha = 0.5) + 
  labs(title = "Níveis de CO2 ao longo do Tempo",
       x = "Ano",
       y = "Níveis de CO2",
       color = "Tipo de Dados") +
  theme_minimal() +
  scale_color_manual(values = c("darkblue", "red"), name = "Tipo de Dados", labels = c("Trend", "Interpolated"))


# 3.4 ####

co2 <- co2 %>% 
  mutate(date = paste(year, month, "1", sep = "-"),
         date = as.Date(date))

date_nasa <- giss_nasa %>% 
  select(1:13) %>% 
  clean_names() %>%
  gather(mes, temp, c("jan":"dec")) %>%
  mutate(date = paste(year, mes, "1", sep = "-"),
         date = as.Date(date, "%Y-%B-%d")) %>%
  arrange(date) %>%
  filter(date >= min(co2$date) & date <= max(co2$date)) %>%
  mutate(trend = co2$trend)

date_nasa %>%
  ggplot(aes(x = temp, y = trend)) + 
  geom_point()

cor(date_nasa$temp, date_nasa$trend)

# 3.5 ####

date_nasa %>%
  ggplot(aes(x = trend, y = temp)) + 
  geom_point()

# 3.6

modelo <- lm(temp ~ trend, data = date_nasa)
summary(modelo)


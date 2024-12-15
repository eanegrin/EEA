rm(list = ls())

library(tidyverse)
library(tidymodels)
library(haven)
library(labelled)
library(GGally)
library(corrr)

data <- read_dta("C:/Eugenio/Maestria/EEA/Trabajo Final/gss_2022/GSS2022.dta")

# Limpieza y creacion de variables --------------------------------------------

# seleccion de variables mas interesantes

datos <- data %>% 
  select(year, id, wrkstat, age, educ, paeduc, maeduc,
          sex, race, conrinc, wrkgovt1, wrkgovt2,hrs1,hrs2,wrkslf)

# wrkstat: labor force status
# educ: highest year of school completed
# paeduc: father's highest year of school completed
# maeduc: mother's highest year of school completed
# conrinc: Inflation-adjusted personal income.


# read_dta genera un dataframe con metadata que cambia el output de funciones como str() por eso lo limpiamos

datos <- remove_labels(datos)

eliminar_atributos <- function(x) {
  attributes(x) <- NULL
  return(x)
}

datos <- as.data.frame(lapply(datos, eliminar_atributos))

datos <- datos %>% filter(
  age >= 18,
  age <=65,
  wrkstat %in% c(1,2), # full y part-times
  wrkslf == 2 # empleado
)

datos <- datos %>% mutate(
  exp_potencial = age - (educ + 6),
  salario_hora = case_when(is.na(hrs1) ~ conrinc/hrs2,
                           TRUE ~ conrinc/hrs1),
  sexo = case_when(sex == 1 ~ "Hombre",
                   sex == 2 ~ "Mujer"),
  raza = case_when(race == 1 ~ "White",
                   race == 2 ~ "Black",
                   race == 3 ~ "Other")
)

datos <- datos %>%
  filter(!is.infinite(salario_hora))

# para corregir 7 valores que quedan con exp_potencial < 0
datos <- datos %>% mutate(
  exp_potencial = if_else(exp_potencial < 0, 0, exp_potencial)
)

datos$sexo <- factor(datos$sexo)
datos$raza <- factor(datos$raza)

# descarto variables que no necesito

datos <- datos %>% select(-c(wrkstat,sex,race,conrinc,wrkgovt1,wrkgovt2,hrs1,hrs2,wrkslf))


# eliminamos filas con nulls

datos <- datos %>% drop_na()

# EDA ------------------------------------------------

glimpse(datos)

datos %>%
  dplyr::select(where(is.numeric), -c(year, id))%>%
  summary()

ggplot(datos, aes(x = salario_hora)) +
  geom_histogram(bins = 50, aes(y = ..density..), position = "identity", alpha = 0.5, fill = "#1B9E77", color = "white") +
  geom_density(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, max(datos$salario_hora, na.rm = TRUE), by = 1000)) +
  theme(legend.position = "top") + 
  theme_classic()

summary(datos$salario_hora)

corr_chart <- ggpairs(datos %>% dplyr::select(where(is.numeric), sexo, -c(year, id)), aes(color = sexo),
                      title = "Matriz de correlaciones",
                      upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), legend = 25, progress=FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  theme_bw()
corr_chart

# Modelo de mincer usando MCO -------------------------------------------

datos$log_salario_hora <- log(datos$salario_hora)

mincer_mco <- lm(log_salario_hora ~ educ + exp_potencial + I(exp_potencial^2) +
                  sexo, data = datos)

summary(mincer_mco)

confint(mincer_mco)

# Modelo de mincer usando VI -------------------------------------------------------

cor.test(datos$educ, datos$maeduc, use = "complete.obs")
# la correlacion no es demasiado alta pero es significativa

# algunos en vez de la correlacion hacen un regresion y ven que el coef sea significativo y tenga el signo esperado
instrument_reg <- lm(educ ~ maeduc + exp_potencial + I(exp_potencial^2) + sexo, data = datos)
summary(instrument_reg)

# install.packages("estimatr")
library(estimatr)

mincer_vi <- iv_robust(log_salario_hora ~ educ + exp_potencial + I(exp_potencial^2) + sexo
                | maeduc + exp_potencial + I(exp_potencial^2) + sexo, data = datos)

summary(mincer_vi) # el efecto de la educacion es 2 veces mas grande

# libreria alternativa (no usa errores estandares robustos por lo que los p-values no son tan confiables)
# install.packages("AER")
library(AER)

mincer_vi_2 <- ivreg(log_salario_hora ~ educ + exp_potencial + I(exp_potencial^2) + sexo |
                     maeduc + exp_potencial + I(exp_potencial^2) + sexo, data = datos)
summary(mincer_vi_2)


library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(brms)
library(bayesplot)
library(posterior)

df <- read.csv("GHSH_Pooled_Data1.csv")
df <- na.omit(df)
 
df <- df %>%
  select(-Country ,-Currently_Drink_Alcohol, -Had_sexual_relation)

df_hombres <- df %>%
  filter(Sex == "Male")
df_hombres <- df_hombres %>% 
  select(-Sex)

df_H_13_15 <- df_hombres %>%
  filter(Age.Group == "13-15")

df_H_16_17 <- df_hombres %>%
  filter(Age.Group == "16-17")

# Data frame para mujeres
df_mujeres <- df %>%
  filter(Sex == "Female") 

df_mujeres <- df_mujeres %>% 
  select(-Sex)

df_M_13_15 <- df_mujeres %>%
  filter(Age.Group == "13-15")

df_M_16_17 <- df_mujeres %>%
  filter(Age.Group == "16-17")

df_H_13_15 <- df_H_13_15 %>%
  select(-Age.Group)

df_H_16_17 <- df_H_16_17 %>%
  select(-Age.Group)

df_M_13_15 <- df_M_13_15 %>%
  select(-Age.Group)

df_M_16_17 <- df_M_16_17 %>%
  select(-Age.Group)



get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

resumen_estadistico <- function(df) {
  df %>%
    select(where(is.numeric)) %>%             # sólo columnas numéricas
    imap_dfr(~ tibble(
      variable = .y,
      media    = mean(.x, na.rm = TRUE),
      mediana  = median(.x, na.rm = TRUE),
      moda     = get_mode(.x),
      sd       = sd(.x, na.rm = TRUE)
    ))
}


resumen_estadistico(df_H_13_15)
resumen_estadistico(df_H_16_17)
resumen_estadistico(df_M_13_15)
resumen_estadistico(df_M_16_17)


for (i in 5:(ncol(df) - 1)) {
  var <- names(df)[i]
  
  p1 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(fill = "skyblue", color = "white", bins = 20) +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightgreen", notch = TRUE) +
    labs(title = paste("Boxplot de", var), y = "Porcentaje") +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 1)  # uno debajo del otro
  
}

X <- df %>%
  mutate(
    Sex = ifelse(Sex == "Male",   0, 1),
    Age.Group = ifelse(Age.Group == "13-15",  1, 0),
  )

X <- X %>% select(-Year)

corr_mat <- cor(X, use = "complete.obs")


corrplot(corr_mat, method = "ellipse",  
         type = "lower",        
         diag = FALSE)          

# 1) Preparar datos: base = "Male", dummy = SexFemale
data <- df %>%
  mutate(
    Sex = factor(Sex, levels = c("Male", "Female")),       # nivel base = Male
    Age.Group = factor(Age.Group, levels = c("16-17", "13-15")),
    across(where(is.numeric), ~ .x / 100),
    Attempted_suicide = (Attempted_suicide * (n() - 1) + 0.5) / n()
  )

# 2) Priors apuntando a SexFemale
priors <- c(
  set_prior("normal(1, 0.5)",  class = "b", coef = "SexFemale"),
  set_prior("normal(0, 3)",   class = "b", coef = "Overwieght"),
  set_prior("normal(1.5, 2)",  class = "b", coef = "Use_Marijuana"),
  set_prior("normal(-1, 1)",   class = "b", coef = "Have_Understanding_Parents"),
  set_prior("normal(1, 2)",    class = "b", coef = "Missed_classes_without_permssion"),
  set_prior("normal(2, 0.5)",  class = "b", coef = "Had_fights"),
  set_prior("normal(2, 0.5)",  class = "b", coef = "Bullied"),
  set_prior("normal(2, 0.5)",  class = "b", coef = "No_close_friends"),
  set_prior("normal(2.5, 2)",  class = "b", coef = "Got_Seriously_injured"),
  set_prior("normal(2, 1)",    class = "b", coef = "Really_Get_Drunk"),
  set_prior("normal(2, 1)",  class = "b", coef = "Age.Group13M15")
)

# 3) Ajustar el modelo
modelo_beta <- brm(
  formula = Attempted_suicide ~ Sex + Overwieght + Use_Marijuana + 
    Have_Understanding_Parents + Missed_classes_without_permssion +
    Had_fights + Bullied + No_close_friends + 
    Got_Seriously_injured + Really_Get_Drunk + Age.Group,
  data    = data,
  family  = Beta(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  seed    = 123
)

summary(modelo_beta)

color_scheme_set("brightblue")

posterior_samples <- as_draws_df(modelo_beta)

pars_beta <- grep("^b_", names(posterior_samples), value = TRUE)
mcmc_intervals(posterior_samples,
               pars = pars_beta,
               prob = 0.95) +
  ggtitle("Intervalos de Credibilidad al 95%") +
  theme_minimal()

mcmc_areas(posterior_samples,
           pars = pars_beta,
           prob = 0.95) +
  ggtitle("Áreas de Posterior con IC 95%") +
  theme_minimal()


mcmc_trace(posterior_samples,
           pars = pars_beta) +
  ggtitle("Trazas MCMC por parámetro") +
  theme_minimal()

pp_check(modelo_beta, ndraws = 100) +
  ggtitle("Chequeo Posterior Predictivo") +
  theme_minimal()

loo_beta <- loo(modelo_beta)
print(loo_beta)

bayes_R2(modelo_beta)





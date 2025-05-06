library(tidyverse)

macae <- readRDS("dados/macae.rds")

###################################
####### SRS
###################################

# instanciamos um vetor numerico
alfabetizada_srs <- numeric(2000)

# tiramos 2000 amostras de n=1200 
for (i in 1:2000) {
  sample <- macae %>%
    slice_sample(n=1200, replace=FALSE)
  
  alfabetizada_srs[[i]] <- mean(sample$alfabetizada)
}

# histograma da variavel de interesse
hist(alfabetizada_srs)

# var_srs é benchmark
var_srs <- var(alfabetizada_srs)

###################################
####### SRS com estratificacao
###################################

# 1. Taxa de alfabetização e tamanho de cada setor
tab_setor <- macae %>% 
  group_by(codigo_setor) %>% 
  summarise(p   = mean(alfabetizada),
            Nh  = n(),
            .groups = "drop") %>% 
  arrange(p)                      # ordena do menor p ao maior

# 2. Cria 4 faixas com ~25 % da população cada
tab_setor <- tab_setor %>% 
  mutate(cumN = cumsum(Nh),
         faixa_bal = cut(cumN,
                         breaks = quantile(cumN, probs = seq(0, 1, 0.25)),
                         include.lowest = TRUE,
                         labels = FALSE))

# 3. Junta de volta à base
macae_bal <- macae %>% 
  left_join(select(tab_setor, codigo_setor, faixa_bal), by = "codigo_setor")

# vamos fazer a estratificacao com alocacao proporcional
n_entrevistas <- macae_bal %>%
  group_by(subdistrito, faixa_bal) %>%
  summarise(
    Nh = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Wh = Nh / sum(Nh),
    nh = round(1200 * Wh)
  )

sum(n_entrevistas$nh)

macae_bal %>%
  group_by(subdistrito, faixa_bal) %>%
  summarise(
    p = mean(alfabetizada),
    var = p * (1 - p),
    Nh = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Wh = Nh / sum(Nh),
    Wh2 = Wh^2,
    var_strat = (Wh2 * var)
  ) %>%
  summarise(variance = sum(var_strat)) %>%
  pull(variance)

# group map retorna uma lista de tibbles (cada entrada sendo um tibble
# para cada categoria no agrupamento), que juntamos com bind_rows --
# i.e., uma amostra de tamanho n = nh para cada subdistrito

alfabetizada_estratificada <- numeric(2000)

for (i in 1:2000) {
  sample <- macae_bal %>%
    inner_join(n_entrevistas, by=c("subdistrito", "faixa_bal")) %>%
    group_by(subdistrito, faixa_bal) %>%
    group_map(
      ~ slice_sample(.x, n = unique(.x$nh), replace=FALSE),
      .keep = TRUE
    ) %>%
    bind_rows()
  
  # matematicamente, isso é uma média ponderada
  alfabetizada_estratificada[[i]] <- mean(sample$alfabetizada)
}

table(sample$faixa_bal)

length(unique(sample$codigo_setor))

# histograma da variavel de interesse
hist(alfabetizada_estratificada)

# var_srs é benchmark
var_estratificada <- var(alfabetizada_estratificada)

# design effect
design_effect_estratificada <- var_estratificada / var_srs

###################################
####### amostra por conglomerado
###################################

# pra fazer amostra por conglomerado, basta amostrar em duas etapas
# primeiro, amostramos conglomerados de maneira proporcional ao tamanho
# depois, amostramos algum $n$ de entrevistas em todos os conglomerados

alfabetizada_conglomerados <- numeric(2000)

probabilidades <- macae %>%
  group_by(codigo_setor) %>%
  summarise(
    Nh = n()
  ) %>%
  mutate(
    prob_inclusao = Nh / (sum(Nh))
  )

for (i in 1:2000) {
  setores_censitarios <- probabilidades %>%
    slice_sample(n = 80, weight_by = prob_inclusao)
  
  sample <- macae %>%
    filter (codigo_setor %in% unique(setores_censitarios$codigo_setor)) %>%
    group_by(codigo_setor) %>%
    group_map(
      ~ slice_sample(.x, n = 15, replace = FALSE),
      .keep = TRUE
    ) %>%
    bind_rows()
  
  alfabetizada_conglomerados[[i]] <- mean(sample$alfabetizada)
}

# histograma da variavel de interesse
hist(alfabetizada_conglomerados)

# var_srs é benchmark
var_conglomerados <- var(alfabetizada_conglomerados)

# design effect
design_effect_conglomerados <- var_conglomerados / var_srs

###################################
####### visualizacao
###################################

df_resultados <- tibble(
  valor = c(alfabetizada_srs, alfabetizada_estratificada, alfabetizada_conglomerados),
  metodo = rep(c("SRS", "Estratificada", "Conglomerados"), each = 2000)
)

ggplot(df_resultados, aes(x = valor, fill = metodo, color = metodo)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribuição da proporção de alfabetizados",
       x = "Proporção de alfabetizados",
       y = "Densidade") +
  theme_minimal()

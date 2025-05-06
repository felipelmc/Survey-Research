
set.seed(42)

macae <- readRDS("dados/macae.rds")

alfabetizada_srs <- numeric(2000)

for (i in 1:2000) {
  sample <- macae %>%
    slice_sample(n=1200, replace=FALSE)

  alfabetizada_srs[[i]] <- mean(sample$alfabetizada)
}

var_srs <- var(alfabetizada_srs)
# 
# ##################################################################################################
# 
# 1. Calcular os indicadores originais para obter o nh
n_entrevistas <- macae %>%
  mutate(subdistrito = case_when(
    subdistrito %in% c("Nova Cidade", "Barra De Macaé") ~ "Nova Cidade / Barra De Macaé",
    TRUE ~ subdistrito
  )) %>%
  group_by(subdistrito) %>%
  summarise(
    Nh = n(),
    prop_alfabetizada = mean(alfabetizada),
    var_alfabetizada = var(alfabetizada),
    .groups = "drop"
  ) %>%
  mutate(
    Wh = Nh / sum(Nh),
    nh = round(1200 * Wh)
  )


macae_estratificada <- macae %>%
  mutate(subdistrito = case_when(
    subdistrito %in% c("Nova Cidade", "Barra De Macaé") ~ "Nova Cidade / Barra De Macaé",
    TRUE ~ subdistrito
  )) %>%
  inner_join(n_entrevistas, by=c("subdistrito"))

alfabetizada_estratificada <- numeric(2000)

for (i in 1:2000) {
  sample <- macae_estratificada  %>%
    group_by(subdistrito) %>%
    group_map(
      ~ slice_sample(.x, n = unique(.x$nh), replace=FALSE),
      .keep = TRUE
    ) %>%
    bind_rows()

  alfabetizada_estratificada[[i]] <- mean(sample$alfabetizada)
}

var_estratificada_subdistrito <- var(alfabetizada_estratificada)

# design effect
design_effect_estratificada_subdistrito <- var_estratificada_subdistrito / var_srs

cat(sprintf("Design effect subdistrito: %.3f\n", design_effect_estratificada_subdistrito))

##################################################################################################



##################################################################################################


# # Carregar a base
# df <- readRDS("dados/macae.rds")
# 
# # Calcular proporção e população por bairro
# estratos <- df %>%
#   group_by(distrito, subdistrito, bairro) %>%
#   summarise(
#     Nh = n(),
#     prop_alfabetizada = mean(alfabetizada),
#     .groups = "drop"
#   )
# 
# # Ordenar bairros por proporção de alfabetização
# estratos <- estratos %>%
#   arrange(prop_alfabetizada) %>%
#   mutate(cumsum_pop = cumsum(Nh),
#          total_pop = sum(Nh),
#          prop_acumulada = cumsum_pop / total_pop)
# 
# # Criar quintis com base na proporção acumulada da população
# estratos <- estratos %>%
#   mutate(
#     estrato_quartil = cut(
#       prop_acumulada,
#       breaks = seq(0, 1, length.out = 5),  # 5 quintis = 6 cortes
#       labels = 1:4,
#       include.lowest = TRUE
#     )
#   )
# 
# # Juntar de volta ao df original
# df <- df %>%
#   inner_join(estratos %>% select(distrito, subdistrito, bairro, estrato_quartil),
#              by = c("distrito", "subdistrito", "bairro"))
# 
# table(df$estrato_quartil)
# 
# # 1. Calcular os indicadores originais para obter o nh
# n_entrevistas <- df %>%
#   group_by(estrato_quartil) %>%
#   summarise(
#     Nh = n(),
#     prop_alfabetizada = mean(alfabetizada),
#     var_alfabetizada = var(alfabetizada),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Wh = Nh / sum(Nh),
#     nh = round(1200 * Wh)
#   )
# 
# macae_estratificada <- df %>%
#   inner_join(n_entrevistas, by=c("estrato_quartil"))
# 
# alfabetizada_estratificada <- numeric(2000)
# 
# for (i in 1:2000) {
#   sample <- macae_estratificada  %>%
#     group_by(estrato_quartil) %>%
#     group_map(
#       ~ slice_sample(.x, n = unique(.x$nh), replace=FALSE),
#       .keep = TRUE
#     ) %>%
#     bind_rows()
#   
#   alfabetizada_estratificada[[i]] <- mean(sample$alfabetizada)
# }
# 
# var_estratificada_subdistrito <- var(alfabetizada_estratificada)
# 
# # design effect
# design_effect_estratificada_subdistrito <- var_estratificada_subdistrito / var_srs




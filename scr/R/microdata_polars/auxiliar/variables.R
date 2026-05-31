#### Pipeline de dominios derivados ----
pipeline <- list(
  list("taric",    "euros_taric",         euros_taric),
  list("taric",    "euros_taric_pais",     euros_taric_pais),
  list("sectores", "euros_pais",           euros_pais),
  list("taric",    "kg_pais",              kg_pais),
  list("taric",    "kg_taric",             kg_taric),
  list("taric",    "kg_taric_pais",        kg_taric_pais),
  list("sectores", "euros_sectores",       euros_sectores),
  list("sectores", "euros_sectores_pais",  euros_sectores_pais)
)

#### Filtros provincia ----
filtros_provincia <- list(
  madrid           = 28L
)

##### Completa ----
# filtros_provincia <- list(
#   nodeterminado    = 0L,
#   andalucia        = c(4L,11L,14L,18L,21L,23L,29L,41L),
#   aragon           = c(22L,44L,50L),
#   asturias         = 33L,
#   baleares         = 7L,
#   canarias         = c(35L,38L),
#   cantabria        = 39L,
#   castillalamancha = c(2L,13L,16L,19L,45L),
#   castillayleon    = c(5L,9L,24L,34L,37L,40L,42L,47L,49L),
#   cataluna         = c(8L,17L,25L,43L),
#   galicia          = c(15L,27L,32L,36L),
#   madrid           = 28L
#   murcia           = 30L,
#   navarra          = 31L,
#   paisvasco        = c(1L,20L,48L),
#   rioja            = 26L,
#   valencia         = c(3L,12L,46L),
#   ceuta            = 51L,
#   melilla          = 52L
# )

#### Mapeo ambito ----
mapeo_ambito_cod_comunidad <- list(
  # nodeterminado=0L,
  # andalucia=1L,
  # aragon=2L,
  # asturias=3L,
  # baleares=4L,
  # canarias=5L,
  # cantabria=17L,
  # castillalamancha=7L,
  # castillayleon=6L,
  # cataluna=8L,
  # galicia=10L,
  madrid=15L,
  # murcia=11L,
  # navarra=12L,
  # paisvasco=14L,
  # rioja=16L,
  # valencia=13L,
  # ceuta=51L,
  # melilla=52L,
  espana=99L
)

##### Completa ----
# mapeo_ambito_cod_comunidad <- list(
#   nodeterminado=0L,
#   andalucia=1L,
#   aragon=2L,
#   asturias=3L,
#   baleares=4L,
#   canarias=5L,
#   cantabria=17L,
#   castillalamancha=7L,
#   castillayleon=6L,
#   cataluna=8L,
#   galicia=10L,
#   madrid=15L,
#   murcia=11L,
#   navarra=12L,
#   paisvasco=14L,
#   rioja=16L,
#   valencia=13L,
#   ceuta=51L,
#   melilla=52L,
#   espana=99L
# )

#### Ámbitos
ambitos  <- c("espana", names(filtros_provincia))

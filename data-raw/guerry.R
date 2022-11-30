## code to prepare `guerry` dataset goes here

guerry <- geodaData::guerry |>
  sf::st_transform(27572)

usethis::use_data(guerry, overwrite = TRUE, internal = TRUE)

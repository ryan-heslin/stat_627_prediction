chocolate <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv")

ingredients_lookup <- c(
  B = "beans", C = "cocoa_butter", L = "lecithin", S = "sugar", Sa = "salt",
  "S*" = "nonsugar_sweetener",
  V = "vanilla"
)

chocolate$cocoa_percent <- gsub("%$", "", chocolate$cocoa_percent) |>
  as.numeric()
chocolate <- data.frame(chocolate, local({
  split_traits <- strsplit(chocolate$most_memorable_characteristics, ",\\s?")
  n_traits <- max(lengths(split_traits))
  rapply(split_traits, f = `length<-`, classes = "ANY", value = n_traits) |>
    matrix(ncol = n_traits, byrow = TRUE) |>
    `colnames<-`(paste0("characteristic", seq_len(n_traits))) |>
    as.data.frame()
}))
chocolate$most_memorable_characteristics <- NULL

# Making dummy columns the old-fashioned way
chocolate <- data.frame(chocolate, local({
  gsub("^\\d+-\\s", "", chocolate$ingredients) |>
    strsplit(",") |>
    lapply(sort) |>
    sapply(match, x = names(ingredients_lookup), nomatch = 0) |>
    t() |>
    `class<-`("logical") |>
    `colnames<-`(ingredients_lookup) |>
    as.data.frame()
}))

saveRDS(chocolate, here::here("data", "chocolate.Rds"))

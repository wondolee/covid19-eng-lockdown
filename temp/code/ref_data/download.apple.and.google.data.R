#remotes::install_github("joachim-gassen/tidycovid19",force=TRUE)
rm(list=ls())
require(tidycovid19)
apple.df <- download_apple_mtr_data(type = "country_city", silent = TRUE, cached = TRUE)
apple.eng <- apple.df %>% dplyr::filter(iso3c == "GBR",sub_region=="England")
apple.eng %>% dplyr::filter(date>="2020-01-01", date<"2020-07-02") %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = driving, color = city)) +
  ggplot2::geom_line()

google.df <- download_google_cmr_data(type = "country_region", silent = TRUE, cached = TRUE)
google.eng <- google.df %>% dplyr::filter(iso3c == "GBR")

google.eng %>% dplyr::filter(date>="2020-01-01", date<"2020-07-02") %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    retail_recreation = mean(retail_recreation, na.rm = TRUE)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = retail_recreation)) +
  ggplot2::geom_line()

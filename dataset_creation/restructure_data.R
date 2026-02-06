library(tidyverse)

# this script takes the language_country data queried from wikipedia and restructures it into language per country

data_path = "data/raw/lang_data/lang_ts/"
file_names = list.files(data_path)

df_list = list()
i = 1
for (file in file_names) {
  df_next = read_csv(paste0(data_path, file))
  df_list[[i]] = df_next
  i = i + 1
}

# turn all df columns into charachter to allow binding
df_list = lapply(df_list, function(df) df %>% mutate_all(as.character))

df_full = bind_rows(df_list)

# pivot longer and adjust datatypes and column names
df_long = df_full %>%
  pivot_longer(cols = contains("2"),
               names_to = "year",
               values_to = "page_views") %>%
  filter(!is.na(page_views)) %>%
  mutate(page_views = as.numeric(page_views)) %>%
  rowwise() %>%
  mutate(year = str_split(year, "_")[[1]][3]) %>%
  mutate(country = case_when(country == "--" ~ "unknown",
                             TRUE ~ country)) %>%
  rename("lang_id" = language, "country_code" = country)

df_long = df_long %>%
  relocate(country_code, year, lang_id)

df_long %>%
  filter(lang_id == "na")

# fix nambia
df_long = df_long %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))


# write_csv
write_csv(df_long, "data/processed/country_year_language_page_views.csv")


library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

df_long = read_csv("data/processed/country_year_language_page_views.csv")
wikipedia_countries = read_tsv("data/final_dataset/countries_wikipedia.tsv")
wikipedia_countries = wikipedia_countries %>%
  select(iso_code, iso_alpha3_code, name, data_risk_score, data_risk_classification)

wikipedia_countries = wikipedia_countries %>%
  mutate(iso_code = case_when(is.na(iso_code) ~ "NA",
                                TRUE ~ iso_code))

df_long = df_long %>% 
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))

ccs = df_long %>% 
  distinct(country_code)


country_data = rnaturalearth::ne_countries(scale = "large")
country_data = country_data %>%
  select(iso_a2_eh, continent, region_wb, income_grp) %>%
  as_tibble() %>%
  select(!geometry)

ccs = ccs %>%
  left_join(wikipedia_countries, join_by("country_code" == "iso_code")) %>%
  left_join(country_data, join_by("country_code" == "iso_a2_eh"))
  
  
# remove unknown country
ccs = ccs %>%
  filter(!is.na(name))

df_long = df_long %>%
  filter(country_code != "unknown")

df_long = df_long %>%
  group_by(country_code, year, lang_id) %>%
  summarize(page_views = sum(page_views))


# filter to countries not in rnaturalearthdata -adjust country code of those not in rnaturalearthdata
df_long = df_long %>%
  mutate(country_code = case_when(country_code %in% c("RE",
                                                      "YT",
                                                      "GP",
                                                      "GF",
                                                      "MQ") ~ "FR",
                                  country_code %in% c("CX",
                                                      "CC") ~ "AU",
                                  country_code == "BQ" ~ "NL",
                                  country_code %in% c("BV", "SJ") ~ "NO",
                                  country_code == "TK" ~ "NZ",
                                  TRUE ~ country_code))

df_long = df_long %>%
  group_by(country_code, year, lang_id) %>%
  summarize(page_views = sum(page_views))


# change naming
ccs = ccs %>%
  rename("country_name" = name,
         "country_code_a3" = iso_alpha3_code)



ccs %>%
  count(country_code) %>%
  filter(n > 1)

ccs %>%
  filter(country_code == "BR")

# adjust country names
ccs = ccs %>%
  mutate(country_name = case_when(country_code == "AU" ~ "Australia",
                                country_code == "BR" ~ "Brazil",
                                country_code == "FR" ~ "France",
                                country_code == "KZ" ~ "Kazakhstan",
                                TRUE ~ country_name))


ccs = ccs %>%
  group_by(country_code, country_code_a3, country_name) %>%
  summarize(data_risk_score = first(data_risk_score),
            data_risk_classification = first(data_risk_classification),
            continent = first(continent),
            region_wb = first(region_wb),
            income_grp = first(income_grp))

ccs = ccs %>%
  distinct(country_code, country_code_a3, country_name, continent, region_wb, income_grp)

# fix continents
ccs %>%
  filter(continent == "Seven seas (open ocean)")


ccs = ccs %>%
  mutate(continent = case_when(country_code == "SH" ~ "Africa",
                               country_code == "MV" ~ "Asia",
                               country_code == "TF" ~ "Antarctica",
                               country_code == "HM" ~ "Antarctica",
                               country_code == "MU" ~ "Africa",
                               country_code == "GS" ~ "South America",
                               country_code == "IO" ~ "Asia",
                               country_code == "SC" ~ "Africa",
                               country_code == "FR" ~ "Europe",
                               country_code == "AU" ~ "Oceania",
                               TRUE ~ continent))


ccs = ccs %>%
  filter(country_code %in% df_long$country_code) %>%
  distinct(country_code, country_code_a3, country_name, continent, region_wb, income_grp)


df_long = df_long %>%
  rename("language_code" = lang_id)

df_long %>%
  ungroup() %>%
  distinct(language_code)

# change to ISO language codes
ISO = read_csv("data/final_dataset/language_data.csv")

# langs not in ISO dataframe
df_long %>%
  ungroup() %>%
  distinct(language_code) %>%
  filter(!language_code %in% ISO$language_code)

# join with ISO
df_long %>%
  filter(language_code %in% c("ku", "kv"))

df_long = df_long %>%
  ungroup() %>%
  left_join(ISO %>%
              select(language_code, ISO6393), join_by(language_code))

df_long = df_long %>%
  mutate(ISO6393 = case_when(language_code == "kv" ~ "knc",
                             language_code == "ku" ~ "koi",
                             language_code == "simple" ~ "eng",
                             language_code == "be-tarask" ~ "bel",
                             language_code == "map-bms" ~ "jav",
                             language_code == "roa-tara" ~ "nap",
                             TRUE ~ ISO6393))

df_long = df_long %>%
  group_by(country_code, year, ISO6393) %>%
  summarize(page_views = sum(page_views))

df_long %>%
  ungroup() %>%
  distinct(ISO6393)

write_csv(df_long, "data/final_dataset/country_year_lang_views.csv")
write_csv(ccs, "data/final_dataset/country_data.csv")

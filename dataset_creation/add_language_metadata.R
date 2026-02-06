library(tidyverse)

# this script takes the language codes from wikipedia and adds ISOcodes, glottocodes, macroareas, coordinates, aes and langauge family from glottolog and SIL

iso_codes = read_tsv("data/raw/lang_data/ISOcodes.tsv")
wiki_codes = read_csv("data/raw/lang_data/lang_ids.csv")
wiki_data = read_csv("data/processed/country_year_language_page_views.csv")

wiki_codes %>%
  distinct(lang_id)

wiki_data %>%
  distinct(lang_id)


# subset to lang_id's for which data exists
wiki_codes %>%
  filter(!lang_id %in% wiki_data$lang_id)

wiki_codes = wiki_codes %>%
  filter(lang_id %in% wiki_data$lang_id)


glotto = read_csv("data/raw/clld/glottolog.csv")

iso_codes = iso_codes %>%
  select(Id, Part1, Ref_Name) %>%
  rename("ISO6393" = Id)


lang_codes = wiki_codes %>%
  left_join(iso_codes, join_by("lang_id" == "Part1")) %>%
  mutate(ISO6393 = case_when(is.na(ISO6393) ~ lang_id,
                             TRUE ~ ISO6393)) %>% 
  select(!Ref_Name)


# change Iso of macro languages and variaties not identified by glottolog
lang_codes %>%
  filter(!ISO6393 %in% glotto$ISO6393)

head(lang_codes)

lang_codes = lang_codes %>%
  mutate(ISO6393 = case_when(ISO6393 == "ara" ~ "arb",
                             ISO6393 == "pus" ~ "pbu",
                             ISO6393 == "nep" ~ "npi",
                             ISO6393 == "zho" ~ "cmn",
                             ISO6393 == "bh" ~ "bho",
                             ISO6393 == "cbk-zam" ~ "cbk",
                             ISO6393 == "est" ~ "ekk",
                             ISO6393 == "fas" ~ "pes",
                             ISO6393 == "grn" ~ "gug",
                             ISO6393 == "iku" ~ "ike",
                             ISO6393 == "kon" ~ "kng",
                             ISO6393 == "kau" ~ "knc",
                             ISO6393 == "aze" ~ "azj",
                             ISO6393 == "mlg" ~ "plt",
                             ISO6393 == "msa" ~ "zlm",
                             ISO6393 == "nah" ~ "nch",
                             ISO6393 == "nds-nl" ~ "gos",
                             ISO6393 == "orm" ~ "hae",
                             ISO6393 == "que" ~ "quy",
                             ISO6393 == "ori" ~ "ory",
                             ISO6393 == "srd" ~ "sro",
                             ISO6393 == "swa" ~ "swh",
                             ISO6393 == "yid" ~ "ydd",
                             ISO6393 == "ful" ~ "fuc",
                             ISO6393 == "aym" ~ "ayr",
                             ISO6393 == "cre" ~ "crm",
                             ISO6393 == "din" ~ "dip",
                             ISO6393 == "eml" ~ "egl",
                             ISO6393 == "kur" ~ "ckb",
                             ISO6393 == "kom" ~ "koi",
                             ISO6393 == "mon" ~ "khk",
                             ISO6393 == "sqi" ~ "als",
                             ISO6393 == "uzb" ~ "uzn",
                             ISO6393 == "zha" ~ "zzj",
                             ISO6393 == "lav" ~ "lvs",
                             ISO6393 == "be-tarask" ~ "bel",
                             ISO6393 == "simple" ~ "eng",
                             ISO6393 == "map-bms" ~ "jav",
                             ISO6393 == "roa-tara" ~ "nap",
                             TRUE ~ ISO6393))
                             

# merge with glotto
lang_codes = lang_codes %>%
  left_join(glotto, join_by(ISO6393)) %>%
  select(!c(ID, language, level, subclassification, med, medovertime, bib, category))


### fill missing data
colSums(is.na(lang_codes))


# adjust macroarea
lang_codes = lang_codes %>%
  mutate(macroarea = case_when(ISO6393 == "ile" ~ "Eurasia",
                               ISO6393 == "arz" ~ "Africa",
                               ISO6393 == "eng" ~ "Eurasia",
                               ISO6393 == "ace" ~ "Papunesia",
                               TRUE ~ macroarea))

lang_codes %>%
  filter(is.na(aes)) 

# adjust threat levels
lang_codes = lang_codes %>%
  mutate(aes = case_when(ISO6393 == "ayr" ~ 1,
                         ISO6393 == "crm" ~ 2,
                         ISO6393 == "dip" ~ 1,
                         ISO6393 == "egl" ~ 3,
                         ISO6393 %in% c("bos", "hrv", "pol", "srp", "ckb", "khk", "nno", "als", "zzj", "uzn") ~ 1,
                         ISO6393 == "koi" ~ 2,
                         TRUE ~ aes))

lang_codes = lang_codes %>%
  rename("language_code" = lang_id)

lang_codes %>%
  filter(is.na(family))

lang_codes = lang_codes %>%
  mutate(family = case_when(ISO6393 == "eus" ~ "Basque",
                            TRUE ~ family))


lang_codes %>%
  filter(is.na(ISO6393))

# make ISO unique identifier
lang_codes %>%
  count(ISO6393) %>%
  filter(n > 1)

mltpl = c("ckb", "koi", "bel", "eng", "jav", "nap")


lang_codes %>%
  filter(ISO6393 %in% mltpl)

lang_codes = lang_codes %>%
  filter(!language_code %in% c("kv", "ku", "be-tarask", "simple", "map-bms", "roa-tara"))

lang_codes = lang_codes %>%
  rename("latitude" = Latitude,
         "longitude" = Longitude)


lang_codes %>%
  distinct(language_code)

lang_codes %>%
  distinct(ISO6393)

write_csv(lang_codes, "data/final_dataset/language_data.csv")


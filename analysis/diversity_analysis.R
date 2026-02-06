library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(xtable)
library(scales)
library(DescTools)
library(tmap)
library(treemapify)

# How has the diversity in wikipedia use changed from 2015 to 2023?

# read data
df_wiki = read_csv("data/final_dataset/country_year_lang_views.csv")
df_language = read_csv("data/final_dataset/language_data.csv")
df_country = read_csv("data/final_dataset/country_data.csv")
df_geo = ne_countries(scale = "large")

df_wiki = df_wiki %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))
df_country = df_country %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))

df_wiki %>%
  distinct(ISO6393) # 340 languages

df_wiki %>%
  distinct(country_code) # 239 languages

# set up geo
df_geo = df_geo %>%
  select(iso_a2_eh,
         geometry) %>%
  group_by(iso_a2_eh) %>%
  summarize(geometry = first(geometry))

df_country %>%
  distinct(country_code, country_name) %>%
  filter(!country_code %in% df_geo$iso_a2_eh)

df_wiki = df_wiki %>%
  left_join(df_country, join_by(country_code))

mid_cords = st_coordinates(st_point_on_surface(df_geo$geometry))
df_geo$mid_long = mid_cords[,1]
df_geo$mid_lat = mid_cords[,2]

# theme
theme_set(theme_minimal())

# filter to countries present in all years between 2015 and 2023
countries = df_wiki %>%
  filter(year != 2024) %>%
  group_by(country_code) %>%
  summarize(years = n_distinct(year)) %>%
  filter(years == 9) %>%
  pull(country_code)

length(countries)

df_wiki = df_wiki %>%
  filter(year != 2024) %>%
  filter(country_code %in% countries)


# filter countries with very low page_views, e.g. 1.000.000

low_pw = df_wiki %>%
  group_by(year, country_code) %>%
  summarize(page_views = sum(page_views)) %>%
  filter(page_views < 1000000) %>%
  ungroup() %>%
  distinct(country_code) %>%
  pull(country_code)

df_wiki = df_wiki %>%
  filter(!country_code %in% low_pw)

# within countries, filter to languages only found in 2015
df_wiki = df_wiki %>%
  group_by(country_code, ISO6393) %>%
  filter(any(year == 2015)) %>%
  ungroup()

df_wiki = df_wiki %>%
  group_by(country_code,country_name, year) %>%
  mutate(prop = page_views / sum(page_views))


# calculate diversity
df_diversity = df_wiki %>%
  group_by(country_code,country_name, year) %>%
  mutate(prop = page_views / sum(page_views)) %>%
  summarize(diversity = 1 / sum(prop^2),
            continent = unique(continent),
            income_grp = unique(income_grp),
            region_wb = unique(region_wb))




# spaghetti plot
df_diversity %>%
  ggplot(aes(y = diversity, x = year, fill = country_code, color = country_code)) + 
  geom_line() + 
  theme(legend.position = "none")


# map diversity
# 2015
df_diversity %>%
  filter(year %in% 2015) %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(fill = diversity, geometry = geometry)) + 
  geom_sf(col = "black") + 
  scale_fill_viridis_c() + 
  theme_void()

# 2023
df_diversity %>%
  filter(year == 2023) %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(fill = diversity, geometry = geometry)) + 
  geom_sf(col = "black") + 
  scale_fill_viridis_c() + 
  theme_void()


# log ratio
log_ratio = df_diversity %>%
  filter(year %in% c(2015, 2023)) %>%
  pivot_wider(names_from = year, values_from = diversity) %>%
  rename("diversity_2015" = `2015`,
         "diversity_2023" = `2023`) %>%
  mutate(ldr = log2(diversity_2023 / diversity_2015))

map_log_ratio = log_ratio %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  filter(country_code != "AQ") %>%
  ggplot(aes(fill = ldr, geometry = geometry)) + 
  geom_sf(col = "black") + 
  scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"),
                       midpoint = 0) + 
  theme_void() + 
  theme(legend.position = "bottom")
  
print(map_log_ratio)
ggsave("analysis/plots/log_ratio_map.png", map_log_ratio, width = 12, height = 8, dpi = 300)

log_div_ratio = log_ratio %>%
  ggplot(aes(x = ldr,
             y = reorder(country_code, ldr),
             fill = continent)) + 
  geom_bar(stat = "identity") +
  theme( axis.text.y = element_blank(),
        legend.position = c(0.9, 0.2)) +
  labs(y = "Countries",
       x = "Log diversity ratio (2015-2023)")

print(log_div_ratio)
ggsave("analysis/plots/log_div_ratio.png", width = 8, height = 6, dpi = 300)

# calculate mean log ratio
Desc(log_ratio$ldr)
MeanCI(log_ratio$ldr)


# some fluctuations
variability = df_diversity %>%
  group_by(country_name) %>%
  summarize(var = sd(diversity)^2)

variability %>%
  ggplot(aes(y = var, x = country_name)) + 
  geom_point()

variability %>%
  ggplot(aes(y = var, x = country_name)) + 
  geom_point() +
  geom_label_repel(aes(label = country_name))

get_stream_graph = function(country){
  df_wiki %>%
    filter(country_name == country) %>%
    ggplot(aes(y = prop, x = year, fill = ISO6393)) + 
    geom_area() + 
    theme(legend.position = "none") 
}

# plot stream graphs for Brazil, Uzbekistan, Iraq and Tanzania

stream_graphs = df_wiki %>%
  filter(country_name %in% c("Brazil", "Iraq", "Tanzania", "Uzbekistan")) %>%
  rename("proportion" = prop) %>%
  ggplot(aes(y = proportion, x = year, fill = ISO6393)) + 
  geom_area() + 
  facet_wrap(~country_name) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14))

plot(stream_graphs)
ggsave("analysis/plots/stream_graph_v2.png", stream_graphs, width = 8, height = 8, dpi = 300)
´
  



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
  distinct(ISO6393) # 340

df_wiki %>%
  distinct(country_code) # 239

# set up geo
df_geo = df_geo %>%
  select(iso_a2_eh,
         geometry) %>%
  group_by(iso_a2_eh) %>%
  summarize(geometry = first(geometry))

df_country %>%
  distinct(country_code, country_name) %>%
  filter(!country_code %in% df_geo$iso_a2_eh)


mid_cords = st_coordinates(st_point_on_surface(df_geo$geometry))
df_geo$mid_long = mid_cords[,1]
df_geo$mid_lat = mid_cords[,2]

# theme
theme_set(theme_bw())

df_wiki %>%
  filter(is.na(country_code))

# join datasets
df = df_wiki %>%
  left_join(df_language, join_by(ISO6393)) %>%
  left_join(df_country, join_by(country_code))


# fix namibia
df = df %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                   TRUE ~ country_code))

df %>%
  filter(country_code == "NA")

colSums(is.na(df))


## what is the total number of page views?
sum(df$page_views) %>% print()
# 1.759941e+12

## what years are covered?
min(as.numeric(df$year)) # 2015
max(df$year) # 2024

## how many countries and languages are included?
df %>% distinct(country_code) %>% nrow() #250 countries/territories
df %>% distinct(language_code) %>% nrow() # 340 languages

### how does the total number of page views vary by year?
df %>%
  group_by(year) %>%
  summarise(page_views = sum(page_views)) %>%
  ggplot(aes(y = page_views,
             x = year)) + 
  geom_bar(stat = "identity",
           fill = "yellow",
           color = "black") +
  labs(title = "Temporal distribution of wikipedia page views") +
  geom_text(aes(label = as.character(page_views)),
            vjust = -0.2) +
  labs(y = "Number of Tweets")

# there is a peak at 2020 and then it starts falling

## how does the number of page views vary by continent and year?

df %>%
  group_by(continent, year) %>%
  summarise(page_views = sum(page_views)) %>%
  ggplot(aes(y = page_views,
             x = year, 
             fill = continent)) + 
  geom_bar(stat = "identity",
           position = "stack",
           color = "black") + 
  labs(x = "Number of page views",
         title = "Temporal distribution of page views")
           
df %>%
  group_by(continent, year) %>%
  summarise(page_views = sum(page_views)) %>%
  group_by(year) %>%
  mutate(page_views = page_views / sum(page_views) * 100) %>%
  ggplot(aes(y = page_views,
             x = as.factor(year), 
             fill = continent)) + 
  geom_bar(stat = "identity",
           position = "stack",
           color = "black") + 
  labs(x = "Year",
       y = "Page Views (%)",
       title = "Temporal and continental distribution of page views")


df %>%
  group_by(continent, year) %>%
  summarise(page_views = sum(page_views))  %>%
  ggplot(aes(y = page_views,
             x = year,
             fill = continent,
             color = continent)) + 
  geom_area(color = "black") + 
  labs(title = "temporal and continental distribution of page views") 

# decline in Europe and Asia since 2020, North America has reconvered

# bar
df %>%
  group_by(year, continent) %>%
  summarise(page_views = sum(page_views)) %>%
  mutate(percent = round(page_views / sum(page_views),4) * 100) %>%
  ggplot(aes(y = percent, x = year, fill = continent, color = continent)) +
  geom_area(col = "black") +
  labs(title = "percent page views from each continent every year")


# violin plots
df_labs = df %>%
  group_by(year, country_code) %>%
  summarise(page_views = sum(page_views)) %>%
  mutate(log_10_pw = log10(page_views)) %>%
  group_by(year) %>%
  summarize(n = n(),
            median = median(log_10_pw),
            mean = mean(log_10_pw),
            sd = sd(log_10_pw)) %>%
  mutate(year = as.factor(year))

df_line = df %>%
  group_by(year, country_code) %>%
  summarise(page_views = sum(page_views)) %>%
  mutate(log_10_pw = log10(page_views)) %>%
  group_by(year) %>%
  summarize(mean = list(MeanCI(log_10_pw))) %>%
  rowwise() %>%
  mutate(est = unlist(mean)[1],
         lci = unlist(mean)[2],
         uci = unlist(mean)[3],
         year = as.factor(year)) %>%
  select(!mean)



box_vil_plot = df %>%
  group_by(year, country_code) %>%
  summarise(page_views = sum(page_views),
            continent = unique(continent)) %>%
  mutate(percent = round(page_views / sum(page_views),4) * 100,
         year = as.factor(year)) %>%
  ggplot(aes(y = log10(page_views), x = year)) + 
  geom_boxplot(fill = "lightblue", outliers = FALSE) + 
  geom_violin(alpha = 0.5, fill = "lightblue") + 
  #geom_point(data = df_line, aes(x = year, y = est), inherit.aes = FALSE) + 
  #geom_errorbar(data = df_line, aes(x = year, ymax = uci, ymin = lci), inherit.aes = FALSE, width = 0.2) + 
  geom_text(data = df_labs, mapping = aes(label = paste0("n = ", n, 
                                                        "\n med = ", round(median,2),
                                                        "\n mean = ", round(mean,2),
                                                        "\n sd = ", round(sd, 2)), y = 12, x = year),
            inherit.aes = FALSE) + 
  lims(y = c(3, 13)) + 
  geom_jitter(aes(color = continent), alpha = 0.5) + 
  labs(x = "",
       y = "Page Views (log10)")

print(box_vil_plot)

ggsave("analysis/plots/box_vil_plot.png", box_vil_plot, width = 12, height = 6, dpi = 300)

df %>%
  group_by(year, country_code) %>%
  summarise(page_views = sum(page_views),
            continent = unique(continent)) %>%
  mutate(percent = round(page_views / sum(page_views),4) * 100) %>%
  ungroup() %>%
  ggplot(aes(y = log10(page_views), x = year, fill = country_code)) + 
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none")
  

# what countries have the most / least page views in any given year?
df %>%
  group_by(year, country_name) %>%
  summarise(page_views = sum(page_views),
            continent = unique(continent)) %>%
  mutate(percent = round(page_views / sum(page_views),4) * 100) %>%
  arrange(page_views) %>%
  head()

# mean number of views per year
df %>%
  group_by(country_name) %>%
  summarise(mean_page_views = mean(page_views)) %>%
  arrange(-mean_page_views) %>%
  head()


# how does the number of countries vary by year?
df %>%
  group_by(year) %>%
  summarise(n_countries = n_distinct(country_code)) %>%
  ggplot(aes(y = n_countries,
             x = year)) + 
  geom_bar(stat = "identity",
           fill = "yellow",
           color = "black") +
  geom_point() + 
  geom_line(linetype = 1, colour = "navy", size = 1) + 
  labs(title = "Number of countries across time")

# sharp decline for 2024
countries_2024 = df %>%
  filter(year == 2024) %>%
  distinct(country_code, country_name)

countries_2023 = df %>%
  filter(year == 2023) %>%
  distinct(country_code, country_name)

countries_not_in_2024 = countries_2023 %>%
  filter(!country_code %in% countries_2024$country_code)

print(countries_not_in_2024)
# substantial part of the world -> analysis incluidng 2023 possible

## how does the number of years covered vary per country?

df %>%
  group_by(country_name) %>%
  summarize(continent = unique(continent),
            n_years = n_distinct(year)) %>%
  ggplot(aes(y = reorder(country_name, n_years), 
         x = n_years,
         fill = continent)) + 
  geom_bar(stat = 'identity')

# how many countries are present across all years?
df %>%
  group_by(country_name) %>%
  summarize(continent = unique(continent),
            n_years = n_distinct(year)) %>%
  filter(n_years >= 10)

# in total, 195 countries and territory have data for all years
df %>%
  group_by(country_name) %>%
  summarize(continent = unique(continent),
            n_years = n_distinct(year)) %>%
  filter(n_years == 10)

# plot number of years for which data exists
map_years_covered = df %>%
  group_by(country_code) %>%
  summarize(n_years = n_distinct(year)) %>%
  mutate(n_years = as.factor(n_years)) %>%
  left_join(df_geo, join_by(country_code == "iso_a2_eh")) %>% 
  st_as_sf() %>%
  tm_shape() + 
  tm_polygons(fill = "n_years",
              fill.scale = tm_scale_categorical(values = "div"),
              fill.legend = tm_legend(title = "Years")) + 
  tm_borders(col = "black") + 
  tm_crs("auto") + 
  tm_place_legends_inside() + 
  tm_layout(frame = FALSE)


ggsave("anlaysis/plots/map_year_coverage_country.png", width = 12, height = 6, dpi = 300)

## how are the number of page views per country distributed on a map?

# total number of page views
colSums(is.na(df_geo))
df %>%
  filter(country_code == "NA")

df %>%
  group_by(country_code) %>%
  summarize(country_code = unique(country_code),
            country_name = unique(country_name),
            page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf() %>%
  ggplot(aes(geometry = geometry, fill = percent)) +
  geom_sf(color = "black") + 
  scale_fill_gradient2() + 
  theme_void()

# USA heavily dominating, followed by Japan, germany, etc. 
df %>%
  group_by(country_code) %>%
  summarize(country_code = unique(country_code),
            country_name = unique(country_name),
            page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  arrange(-percent) %>%
  mutate(cumulative = cumsum(percent)) %>%
  select(country_name, percent, cumulative) %>%
  slice_max(percent, n = 10) %>%
  xtable()


# median number of page views
df %>%
  group_by(country_code, year) %>%
  summarize(country_code = unique(country_code),
            country_name = unique(country_name),
            continent = unique(continent),
            page_views = sum(page_views)) %>%
  group_by(country_code) %>%
  summarize(median_page_views = median(page_views)) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf() %>%
  ggplot(aes(geometry = geometry, fill = median_page_views)) +
  geom_sf(color = "black") + 
  scale_fill_viridis_c() + 
  labs(title = "Distribution of median number of page views across years")

# India, western europe and north America has many page views

#### distribution of page views

# ecdf
df %>%
  group_by(country_code) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views/sum(page_views) * 100) %>%
  ggplot(aes(y = page_views)) + 
  stat_ecdf()

# top wikipedia viewers
top_page_views = df %>%
  group_by(country_name) %>%
  summarize(page_views = sum(page_views),
            mediapage_views = median(page_views)) %>%
  mutate(percent = page_views/sum(page_views) * 100) %>%
  arrange(desc(percent))

top_page_views = top_page_views %>%
  mutate(rank_perc = rank(percent)) %>%
  mutate(country_name = case_when(rank_perc < max(rank_perc) - 20 ~ "other",
                                  TRUE ~ country_name)) %>%
  group_by(country_name) %>%
  summarize(percent = sum(percent)) %>%
  arrange(desc(percent))

top_page_views = top_page_views %>%
  rowwise() %>%
  mutate(labels = paste0(country_name, "\n", as.character(round(percent,2 )), " %"))

pie(x = top_page_views$percent, labels = top_page_views$labels,
    main = "top 10 countries according to number of page views")

# 22.5% of all page views are from the US


df %>%
  group_by(country_name) %>%
  summarize(page_views = sum(page_views),
            continent = unique(continent),
            income_grp = unique(income_grp)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  arrange(-percent) %>%
  select(country_name, continent, percent) %>%
  slice_max(percent, n = 10) %>%
  xtable()

# histogram
df %>%
  group_by(country_code) %>%
  summarize(page_views = sum(page_views)) %>%
  ggplot(aes(x = page_views)) + 
  geom_histogram()

# differentiate on continent
df %>%
  group_by(country_code) %>%
  summarize(page_views = sum(page_views),
            continent = unique(continent)) %>%
  ggplot(aes(x = page_views)) +
  geom_histogram() + 
  facet_wrap(~continent,
             scale = "free")

# heavy right tail distribution of page views everywhere

#### languages ######

# distribution of languages across macroarea
df_language %>%
  group_by(macroarea) %>%
  summarize(langs = n()) %>%
  ggplot(aes(y = langs, x = macroarea, fill = macroarea)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(langs))) + 
  labs(title = "Distribution of languages across macroarea")

# 64 african languages, 218 Eurasian languages, 17 North American languages, 47 Papunesian languages and 7 south american languages
# no Australian languages; 

# more even distriubtion than the twitter languages; but still very eurasian focused; still quite good match with the underlying distriubtion of languages

# distribution of page views across macroarea
df %>%
  group_by(macroarea) %>%
  summarize(page_views = sum(page_views)) %>%
  ggplot(aes(y = page_views, x = macroarea, fill = macroarea)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(page_views))) + 
  labs(title = "Distribution of page_views across macroarea")

# very heavy dominance of eurasian languages

# distribution of languages across language family
df_language %>%
  distinct(family) %>%
  nrow() # 32 different language families

df_language %>%
  group_by(family) %>%
  summarize(languages = n()) %>%
  mutate(percent = languages / sum(languages) * 100) %>%
  ggplot(aes(x = percent, y = family, fill = family)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(languages))) + 
  theme(legend.position = "none") + 
  labs(title = "Distribution of languages across language families")

# mostly indo european langauges, but quite a lot of language families represented
df %>%
  group_by(family) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  ggplot(aes(x = percent, y = family, fill = family)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(round(percent,2)))) + 
  theme(legend.position = "none") + 
  labs(title = "Distribution of page_views across language families")

# 85 percent of page_views are indo-european

##### distribution across languages

df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  ggplot(aes(x = percent,
             y = reorder(language_name, percent))) + 
  geom_bar(stat = 'identity') +
  geom_label_repel(aes(label = as.character(page_views)))
# very heavy right tail

# top 10
df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  arrange(desc(page_views)) %>%
  select(!page_views) %>%
  slice_max(percent, n = 10) %>%
  mutate(cumulative = cumsum(percent)) %>%
  xtable()

# top 20
df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  arrange(desc(page_views)) %>%
  select(!page_views) %>%
  slice_max(percent, n = 20) %>%
  mutate(cumulative = cumsum(percent)) %>%
  xtable()

# 49.8 percent of page_views are in english

# pie chart
top_page_views = df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views/sum(page_views) * 100) %>%
  mutate(rank_perc = rank(percent)) %>%
  mutate(language_name = case_when(rank_perc < max(rank_perc) - 9 ~ "other",
                                  TRUE ~ language_name)) %>%
  group_by(language_name) %>%
  summarize(percent = sum(percent)) %>%
  arrange(desc(percent))

top_page_views = top_page_views %>%
  rowwise() %>%
  mutate(labels = paste0(language_name, "\n", as.character(round(percent,2 )), " %"))

pie(x = top_page_views$percent, labels = top_page_views$labels,
    main = "Top 10 most viewed language wikipedia across the world")


# ecdf
df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views/sum(page_views) * 100) %>%
  ggplot(aes(x = page_views)) + 
  stat_ecdf()


### how does the distribution vary per year?

# bar plot
df %>%
  group_by(year, language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  group_by(year) %>%
  mutate(ranking = rank(-percent)) %>%
  mutate(top_10 = case_when(ranking < 11 ~ language_name,
                            TRUE ~ "other")) %>%
  group_by(top_10, year) %>%
  summarize(percent = sum(percent)) %>%
  ggplot(aes(y = percent, x = year,  fill = top_10)) + 
  geom_bar(stat = 'identity',
           position = 'stack',
           color = "black") + 
  labs(title = "distribution of languages on wikipedia across years")


# line plot
df_line = df %>%
  group_by(year, language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100)

labs_df = df_line %>%
  group_by(language_name) %>%
  filter(percent == max(percent)) %>%
  summarize(max_percent = max(percent),
            year_max = first(year))

df_line %>%
  ggplot(aes(y = percent, x = year, fill = language_name, color = language_name)) +
  geom_point() + 
  geom_line() +
  geom_label_repel(data = labs_df, aes(x = year_max, y = max_percent, label = language_name, color = language_name),
                   inherit.aes = FALSE) + 
  theme(legend.position = "none") + 
  labs(title = "timeseries of the distribution of page_views globally")

# fairly stable, but english has been increasing its percentage of the total distribution since 2021 while russian clearly has decreased. Coincedes with the
# the decline in page views from Asia and europe and the increase of page views from North America


### map of language distribution
data = df %>%
  group_by(language_code) %>%
  summarize(language_code = unique(language_code),
            language_name = unique(language_name),
            family = unique(family),
            threat = as.factor(unique(aes)),
            latitude = unique(latitude),
            longitude = unique(longitude),
            page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))

df_geo %>%
  ggplot() +
  geom_sf(color = "black") + 
  geom_point(data = data,
             aes(y = latitude, x = longitude,
                 size = percent,
                 color = family, alpha = 0.5)) + 
  guides(color = "none",
         alpha = "none") + 
  theme_minimal() + 
  theme_void()

lang_map = df_geo %>%
  ggplot() +
  geom_sf(color = "black") + 
  geom_point(data = data,
              aes(y = latitude, x = longitude, color = family, alpha = 0.5)) + 
  guides(color = "none",
         alpha = "none") + 
  theme_minimal() + 
  theme_void()

ggsave("analysis/plots/lang_map_pos.png", lang_map, width = 12, height = 6, dpi = 300)


df %>%
  group_by(language_name) %>%
  summarize(page_views = sum(page_views))

#### colour country according to the most dominant wikipedia language

countries_for_label = c("United States", "Czech Republic", "Sweden", "Finland", "Russia", "South Korea", 
                        "Vietnam", "Senegal", "Brazil", "Netherlands", "Israel", "Turkey", "Azerbaijan", 
                        "Thailand", "Indonesia", "Slovakia", "Bulgaria", "Italy", "Yemen", "Mexico", "Armenia", "Hungary", "Gabon",
                        "Taiwan", "Latvia")

df_labs = df %>%
  group_by(country_name, country_code, language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  group_by(country_name) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  filter(country_name %in% countries_for_label) %>%
  slice_max(percent, n = 1) %>%
  group_by(language_name) %>%
  slice_max(percent, n = 1) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh"))
  

map_dom_lang = df %>%
  group_by(country_code, country_name, language_name) %>%
  summarize(page_views = sum(page_views)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>% 
  filter(ranking < 2) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(geometry = geometry, fill = language_name, alpha = percent)) + 
  geom_sf(color = "black") + 
  labs() +
  geom_label_repel(data = df_labs,
                   aes(x = mid_long, y = mid_lat, label = language_name, fill = language_name), 
                   inherit.aes = FALSE,
                   max.overlaps = 25) + 
  theme_void() +
  theme(legend.position = "None")
plot(map_dom_lang)
ggsave("analysis/plots/map_dom_lang.png", map_dom_lang, width = 12, height = 6, dpi = 300)  


# bar plot showing the number of countries where the language is dominant
df %>%
  group_by(country_code, language_name) %>%
  summarize(page_views = sum(page_views),
            family = unique(family)) %>%
  mutate(percent = page_views / sum(page_views) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>% 
  filter(ranking < 2) %>%
  group_by(language_name) %>%
  summarize(n_countries = n(),
            family = unique(family)) %>%
  ggplot(aes(x = n_countries, y = reorder(language_name, n_countries), fill = family)) + 
  geom_bar(stat = 'identity') +
  labs(x = "# countries, most common",
       y = "language",
       title = "most commonly occuring dominant language") + 
  geom_label(aes(label = n_countries))


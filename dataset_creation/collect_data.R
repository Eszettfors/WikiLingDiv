library(httr)
library(jsonlite)
library(tidyverse)
library(time)

#https://doc.wikimedia.org/generated-data-platform/aqs/analytics-api/reference/page-views.html - documentation.

# request the existing wikipedia projects
list_of_wikipedia_projects = 'https://commons.wikimedia.org/w/api.php?action=sitematrix&smtype=language&format=json'

# request the json
wikipedia_list_json = GET(list_of_wikipedia_projects, add_headers(`User-Agent` = "hannes.essfors@gmail.com"))
wikipedia_list_json_text = content(wikipedia_list_json, as = "text", encoding = "UTF8")

# load the json file as a dictionary
wikipedias = fromJSON(wikipedia_list_json_text)

# extract the site matrix
sites = wikipedias$sitematrix

# loop through the site matrix and collect language codes
lang_wiki_codes = c()
lang_wiki_names = c()
i = 0
for (i in 2:length(sites)){
  entry = sites[i]
  id = entry[[1]]["code"][[1]]
  name = entry[[1]]["localname"][[1]]
  codes = entry[[1]]$site$code
  if ("wiki" %in% codes){
     lang_wiki_codes = c(lang_wiki_codes, id)
     lang_wiki_names = c(lang_wiki_names, name)
  }
}

# store names in a df
df_lang_names = data.frame(lang_wiki_codes, lang_wiki_names)
df_lang_names
colnames(df_lang_names) = c("lang_id", "language_name")

# write to csv
write_csv(df_lang_names, "data/raw/lang_data/lang_ids.csv")


## URL for page views
wiki_url = "https://wikimedia.org/api/rest_v1/metrics/pageviews/top-by-country/"

# years to collect data for (endpoint serves from 2015)
years = as.character(2015:2024)

# months to collect data for
months = as.character(1:12)
for (i in 1:length(months)){
  if(nchar(months[i]) < 2 )
  months[i] = paste0("0", months[i])
}


get_language_wikipedia_y_m = function(lang, year, month){
  # takes a langauge code and year and month and returns a dataframe with page views per country. If request isn't 200, return nothing
  domain = paste0(lang, ".wikipedia")
  request = GET(paste0(wiki_url, domain, "/all-access/spider/", year, "/", month), add_headers(`User-Agent` = "hannes.essfors@gmail.com"))

  if(request$status_code != 200){
    return()
  } 
  
  cntnt = content(request, as =  "text", encoding = "UTF-8")
  text = fromJSON(cntnt)
  
  views = text$items$countries
  
  
  df = as_tibble(views[[1]]) %>%
    select(country, views_ceil)

  return(df)
}

get_language_wikipedia_y_m("fr", "2021", "01")

get_language_wikipedia_y = function(language, year){
  # this function takes a language and year, requests the page views for each country for each month in the given year and returns the aggregated number as 
  
  # define an empty dataframe to hold data
  df = data.frame("country" = character(0), "views_ceil" = numeric(0))
  
  for (month in months){
    df2 = get_language_wikipedia_y_m(language, year, month)
    if(!is.null(df2)){
      df = rbind(df, df2)
    } 
  }
  
  # summarize for the year
  df = df %>%
    group_by(country) %>%
    summarize(views_ceil = sum(views_ceil))
  
  return(df)
  
  
}

get_language_wikipedia_y("fr", "2020")

get_language_wikipedia = function(language){
  # this function takes a language, creates a data frame for each year and joins those dataframes into a time series dataframe
  
  df = data.frame("country" = character(0), "views_ceil" = numeric(0))
  for (year in years){
    df1 = get_language_wikipedia_y(language, year)
    df1 = df1 %>%
      rename_with(~ paste0("views_ceil_", year), .cols = views_ceil)
    df = df %>%
      full_join(df1, join_by(country))
  }
  
  df = df %>%
    select(!views_ceil)
  
  return(df)
  
}


# get all language wikipedias for all years and write to csv

i = 0
for (language in lang_wiki_codes){
  
  # get each wikipedia
  t1 = Sys.time()
  
  df = get_language_wikipedia(language)
  
  i = i + 1
  print(paste(i, "language out of", length(lang_wiki_codes), "done"))
   
  t2 = Sys.time()
  t_res = t2 - t1
  print(t_res)
  
  df = df %>%
    mutate(language = language) %>%
    relocate(language)
  
  # write to csv
  output = paste0("data/raw/lang_data/lang_ts/",language,"_ts.csv")
  write_csv(df, output)
}

output = paste0("data/raw/lang_data/lang_ts/",language,"_ts.csv")
write_csv(df_se, output)


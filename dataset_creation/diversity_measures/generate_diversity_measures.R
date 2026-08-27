library(tidyverse)

# this script takes the wikipedia data and generates diversity measures for each country and year

#read wiki data
df_wiki = read_csv("data/final_dataset/country_year_lang_views.csv")
df_langs = read_csv("data/final_dataset/language_data.csv")
sim_m = read_rds("data/processed/lexical_similarity_matrix.rds")

# fix namibia
df_wiki = df_wiki %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                             TRUE ~ country_code))


# generate similarity measures ----------------
# pull langs from data
langs = df_wiki %>%
  distinct(ISO6393) %>%
  pull()

# generate diversity
# for each year, generate q = 0, q = 1 and q = 2 for each muncipality both naive and non naive

get_prop_vec = function(counts){
  #takes a vector with counts and turns it into a proportion vector
  prop_vec = counts / sum(counts)
  return(prop_vec)
}


get_richness = function(counts){
  # takes a vector with counts and calculates the richness
  counts = counts[counts != 0]
  richness = length(counts)
  return(richness)
}

get_exp_shannon = function(prop_vec){
  # takes a vector of proportions and calculates the exponent shannon entropy
  
  prop_vec = get_prop_vec(prop_vec)
  
  prop_vec = prop_vec[prop_vec != 0]
  log_vec = log(prop_vec)
  entropy = -sum(prop_vec*log_vec)
  return(exp(entropy))
}

get_inv_simp = function(prop_vec){
  # takes a vector of proportions and calculates the inverse simpson
  
  prop_vec = get_prop_vec(prop_vec)
  prop_vec = prop_vec[prop_vec != 0]
  squared_prop = prop_vec*prop_vec
  inv_simp = 1/sum(squared_prop)
  
  return(inv_simp)
}



subset_and_reorder = function(matrix, labels){
  # this function takes a matrix and labels and input and subsets the matrix to match the values in the label.
  
  matrix = matrix[labels, labels]
  
  return(matrix)
}

subset_langs = function(sim_m, langs, counts){
  
  # subset to langs found in the similarity matrix
  sim_langs = rownames(sim_m)
  
  # save the indices of the langs that exist in the similarity matrix
  valid_idx = langs %in% sim_langs
  
  # Subset langs and counts according to the indices
  langs = langs[valid_idx]
  counts = counts[valid_idx]
  return(list(langs, counts))
  
}


get_shannon_diversity = function(langs, counts, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  
  # subset the languages and their counts to match that of the sim vector in case not all languages are covered 
  langs_and_counts = subset_langs(sim_m, langs, counts)
  langs = langs_and_counts[[1]]
  counts = langs_and_counts[[2]]
  
  
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  
  prop_vec = get_prop_vec(counts)
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop_vec)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop_vec * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}
langs2 = c("swe", "dan", "deu", "eng")
test_vec2 = c(20, 10, 5, 5)
get_shannon_diversity(langs2, test_vec2, sim_m)

langs3 = c("dan", "swe", "deu", "hun")
test_vec3 = c(10, 20, 5, 5)

get_shannon_diversity(langs3, test_vec3, sim_m)


get_diversity_q = function(langs, counts, sim_m, q = 0){
  # a general function to implement diversity for any q
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(langs, counts, sim_m))
  }
  
  # subset the languages and their counts to match that of the sim vector in case not all languages are covered 
  langs_and_counts = subset_langs(sim_m, langs, counts)
  langs = langs_and_counts[[1]]
  counts = langs_and_counts[[2]]
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  # proportion vector
  prop_vec = get_prop_vec(counts)
  
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop_vec
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop_vec * expected_order))^(1/(1-q))
  
  return(D)
}


get_diversity_q(langs2, test_vec2, sim_m, q = 1)
get_diversity_q(langs3, test_vec3, sim_m, q = 1)


get_naive_diversity_q = function(langs, counts, q = 0){
  # a general function to implement naive diversity for any q
  
  prop_vec = get_prop_vec(counts)
  
  I = diag(length(prop_vec))
  colnames(I) = langs
  rownames(I) = langs
  
  # get diversity
  D = get_diversity_q(langs, prop_vec, I, q)
  
  return(D)
}

langs2
langs3

get_diversity_q(langs2, test_vec2, sim_m, q = 2)
get_naive_diversity_q(langs2, test_vec2, q = 2)

get_diversity_q(langs3, test_vec3, sim_m, q = 2)
get_naive_diversity_q(langs3, test_vec3, q = 2)


get_mean_pairwise_dissimilarity = function(sim_m){
  # This function takes a similarity matrix and calculates the mean pairwise similarities between the languages
  
  return(mean(1 - sim_m[upper.tri(sim_m)]))
  
}

div_measures = df_wiki %>%
  group_by(country_code, year) %>%
  summarize(tot_views = sum(page_views),
            richness = get_richness(page_views),
            exp_shannon = get_exp_shannon(page_views),
            inv_simpson = get_inv_simp(page_views),
            lex_div_q_0 = get_diversity_q(ISO6393, page_views, sim_m, q = 0),
            lex_div_q_1 = get_diversity_q(ISO6393, page_views, sim_m, q = 1),
            lex_div_q_2 = get_diversity_q(ISO6393, page_views, sim_m, q = 2))

# export
write_csv(div_measures, file = "data/final_dataset/diversity_measures.csv")


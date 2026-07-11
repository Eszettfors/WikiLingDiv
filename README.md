# WikiLingDiv

This is the code repository accompanying the paper "WikiLingDiv: a dataset for quantifying digital linguistic diversity using
Wikipedia pageviews" submitted to SIGHUM (LaTeCH-CLfL) 2026.

Using the Wikimedia API, we construct a temperospatial dataset for studying digital linguistic diversity using the number of page views directed towards different language editions of Wikipedia in a given country and year.

# Read the paper
Here you can read the published paper:
https://aclanthology.org/2026.latechclfl-1.19/

# Explore the data
We have developed a dashboard to study the temporal and spatial dynamics of the dataset:
https://f39e09-hannes-essfors.shinyapps.io/wikilingdiv_dashboard/ 

# Code
The code utilized for creating the dataset and deriving measures of diversity from it is found in dataset_creation. The scripts should be run in the order of `collect_data.R`, `restructure_data.R`, `add_language_metadata.R`, `add_country_metadata.R`, `generate_diversity_measures.R` to replicate the data collection and processing of the dataset. The processed and analysis-ready dataset is found in data/final_dataset. To replicate the analysis of the paper, run the scripts in `analysis`.

# Citation

If you use the dataset, please cite:
*  Essfors, H. (2026). WikiLingDiv [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18526766


If you use the dataset or the code in academic work, please also cite:
* Hannes Essfors and Andreas Baumann. 2026. WikiLingDiv: a dataset for quantifying digital linguistic diversity using Wikipedia page views. In Proceedings of the 10th Joint SIGHUM Workshop on Computational Linguistics for Cultural Heritage, Social Sciences, Humanities and Literature 2026, pages 202–211, Rabat, Morocco. Association for Computational Linguistics.



# Licence
This work is openly licensed via CC BY 4.0.

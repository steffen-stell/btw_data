# Tidy German elections data

This repo provides tidy election data for German federal elections. There are 
two data sets. Both are available in .csv and .rds format. 

The `btw_returns` data set provides electoral district level vote counts for 
every federal election since 1953.

The `btw_candidates` data set contains the candidates for each election since 
2002. 

Both have been augmented with unique wikidata ids for the parties
such that parties can be properly identified across time, and potentially 
further data can be added. The data has been sourced from publications of the 
Bundeswahlleiter.

Furthermore, the code folder contains a script to scrape federal elections 
polling data from wahlrecht.de. 
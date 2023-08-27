### Litsearch R
# Alina Koppold, 01. July 2023
# Fear conditioning and data transformation procedures 
# of psychophysiological outcome measures


# SETUP
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(devtools)
install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)

### native search terms
# fear conditioning AND psychophysiology AND humans 

## loading PubMed results
setwd("/Users/koppold/Desktop/data_transformation/data/")
naive_results <- import_results(file="pubmed-fearcondit-set.nbib")
nrow(naive_results)
naive_results[1, "title"]
naive_results[1, "keywords"]
naive_results[4, "keywords"]

#How many articles are missing keywords? 
#We can count up the number of NA values to find out.
sum(is.na(naive_results[, "keywords"]))


# The method="tagged" argument lets extract_terms() know that 
# we are getting keywords that article authors themselves have 
# provided (or ‘tagged’ the article with).
extract_terms(keywords=naive_results[, "keywords"], 
              method="tagged")

#only single words
keywords <- extract_terms(keywords=naive_results[, "keywords"], method="tagged", min_n=1)

# not all articles even provide keywords
# keywords based on titels
# argument for doing so has the curious name "fakerake", 
# because in fact litsearchr uses a slightly simplified version 
# of the full RAKE technique
# min_n, and max_n to choose whether we include single words or multi-word phrases
# min_freq to exclude words that occur in too few of the titles in our original search
extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=3, min_n=2)

# In language analysis, such frequently-occurring but uninformative 
# words are often called ‘stopwords’. If we are going to work with 
# litsearchr a lot and will often need to filter out the same set 
# of stopwords, then it can be handy to keep these words in a text 
# file. We can then read this file into R when we need them, and we 
# can add to the file each time we encounter more words that we 
# know are irrelevant.
clinpsy_stopwords <- read_lines("clin_psy_stopwords.txt")
clinpsy_stopwords

all_stopwords <- c(get_stopwords("English"), clinpsy_stopwords)
title_terms <- extract_terms(
  text=naive_results[, "title"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=all_stopwords
)

title_terms


# Let’s finish by adding together the search terms we got from the 
# article titles and those we got from the keywords earlier, 
# removing duplicates.

terms <- unique(c(keywords, title_terms))

# NETWORK ANALYSIS
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1]
dfm <- create_dfm(elements=docs, features=terms)
dfm[1:3, 1:4]
g <- create_network(dfm, min_studies=3)
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

# Pruning
# Now let’s use the network to rank our search terms by importance, 
# with the aim of pruning away some of the least important ones.
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

# We would like to discard some of the terms that only rarely occur 
# together with the others. What rule can we use to make a decision 
# about which to discard? To get an idea of how we might approach 
# this question, let’s visualize the strengths of the terms.

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)
cutoff_fig

cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.8)

cutoff_cum
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")
get_keywords(reduce_graph(g, cutoff_cum))

cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_change
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")
g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)

extra_terms <- c(
  "psychophysiology", 
  "heart rate"
)

selected_terms <- c(selected_terms, extra_terms)

selected_terms

### GROUPING
grouped_terms <-list(
  conditioning =selected_terms[c(1,17,19,36:40, 78, 83:84)],
  physiology=selected_terms[c(27, 41,66, 68,70,72:75, 97:98)],
  humans=selected_terms[c(48:49)]
)

grouped_terms

### WRITING A NEW SEARCH 
write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=FALSE,
  closure="left",
  writesearch=TRUE
)
cat(read_file("search-inEnglish.txt"))

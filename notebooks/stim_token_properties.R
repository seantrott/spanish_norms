## SAWC: tokenization properties for sentence pair stimuli across models

## figure and details for number of token differences across sentence pairs for 
#  each model
## figure and details for number of tokens to represent target words, for each 
#  model

library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis) 




all_colors <- viridis::viridis(10, option = "mako")
my_colors <- all_colors[c(3, 5, 7)]  # Selecting specific colors from the palette
my_colors <- all_colors[c(3, 7)]

### Read in all data

setwd("Desktop/backburner_projects/projects_sean/sawc/")
tokprop = read_csv("data/processed/models/token_properties.csv")

### Get number of token differences across sentence pairs
tokprop$Num_token_diffs <- abs(tokprop$Num_tokens_s1 - tokprop$Num_tokens_s2)

### Compute the average, max, and modal number of token differences per model
# create the ``mode'' function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


df_averaged <- tokprop %>%
  group_by(Model) %>%
  summarize(avg_token_diffs = mean(Num_token_diffs, na.rm = TRUE),
            max_token_diffs = max(Num_token_diffs, na.rm = TRUE),
            mod_token_diffs = mode(Num_token_diffs)) 

### Save to a .csv file
write.csv(df_averaged, "token_diffs_across_sentence_pairs_per_model.csv", row.names=FALSE, quote=FALSE) 


### Rename the models to something friendlier to read
# Find rows containing "albert-base"
rows_albert_base <- grepl("albert-base", tokprop$Model)
tp_albert_base <- tokprop[rows_albert_base, ]
tp_albert_base%>%
  mutate(Model = "ALBERT-base")

rows_albert_large <- grepl("albert-large", tokprop$Model)
tp_albert_large <- tokprop[rows_albert_large, ]
tp_albert_large%>%
  mutate(Model = "ALBERT-large")

rows_albert_tiny <- grepl("albert-tiny", tokprop$Model)
tp_albert_tiny <- tokprop[rows_albert_tiny, ]
tp_albert_tiny%>%
  mutate(Model = "ALBERT-tiny")

rows_albert_xlarge <- grepl("albert-xlarge", tokprop$Model)
tp_albert_xlarge <- tokprop[rows_albert_xlarge, ]
tp_albert_xlarge%>%
  mutate(Model = "ALBERT-xlarge")

rows_albert_xxlarge <- grepl("albert-xxlarge", tokprop$Model)
tp_albert_xxlarge <- tokprop[rows_albert_xxlarge, ]
tp_albert_xxlarge%>%
  mutate(Model = "ALBERT-xxlarge")

rows_bertbaseml <- grepl("bert-base-multilingual", tokprop$Model)
tp_bertbaseml <- tokprop[rows_bertbaseml, ]
tp_bertbaseml%>%
  mutate(Model = "mBERT")

rows_betocased <- grepl("bert-base-spanish-wwm-cased", tokprop$Model)
tp_betocased <- tokprop[rows_betocased, ]
tp_betocased%>%
  mutate(Model = "BETO-cased")

rows_betouncased <- grepl("bert-base-spanish-wwm-uncased", tokprop$Model)
tp_betouncased <- tokprop[rows_betouncased, ]
tp_betouncased%>%
  mutate(Model = "BETO-uncased")

rows_betouncased <- grepl("bert-base-spanish-wwm-uncased", tokprop$Model)
tp_betouncased <- tokprop[rows_betouncased, ]
tp_betouncased%>%
  mutate(Model = "BETO-uncased")

rows_distilbeto <- grepl("distilbert-base-spanish", tokprop$Model)
tp_distilbeto <- tokprop[rows_distilbeto, ]
tp_distilbeto%>%
  mutate(Model = "DistilBETO")

rows_robertabne <- grepl("roberta-base-bne", tokprop$Model)
tp_robertabne <- tokprop[rows_robertabne, ]
tp_robertabne%>%
  mutate(Model = "RoBERTa-BNE")

rows_robertabne_large <- grepl("roberta-large-bne", tokprop$Model)
tp_robertabne_large <- tokprop[rows_robertabne_large, ]
tp_robertabne_large%>%
  mutate(Model = "RoBerta-BNE-large")

rows_xlmroberta <- grepl("xlm-roberta-base", tokprop$Model)
tp_xlmroberta <- tokprop[rows_xlmroberta, ]
tp_xlmroberta%>%
  mutate(Model = "XLM-RoBERTa")

### Put all these new dfs together
df_all = tp_albert_base %>%
  bind_rows(tp_albert_large) %>%
  bind_rows(tp_albert_tiny) %>%
  bind_rows(tp_albert_xlarge) %>%
  bind_rows(tp_albert_xxlarge) %>%
  bind_rows(tp_bertbaseml) %>%
  bind_rows(tp_betocased) %>%
  bind_rows(tp_betouncased) %>%
  bind_rows(tp_distilbeto) %>%
  bind_rows(tp_robertabne) %>%
  bind_rows(tp_robertabne_large) %>%
  bind_rows(tp_xlmroberta)

nrow(df_all)



### Get number of token differences across sentence pairs
tokprop$Num_token_diffs <- abs(tokprop$Num_tokens_s1 - tokprop$Num_tokens_s2)
df_all$Num_token_diffs <- abs(df_all$Num_tokens_s1 - df_all$Num_tokens_s2)

### Plot distribution of token differences by model
ggplot(df_all, aes(x = Num_token_diffs)) + 
  geom_histogram(bins = 5, alpha = .6) + 
  theme_minimal() +
  facet_wrap(~Model) + 
  theme(text = element_text(size = 15),
        legend.position="none")
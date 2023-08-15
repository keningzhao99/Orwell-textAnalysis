# initial setup

# run necessary packages
library(tidytext) # text mining using tidy tools
library(textdata) # get sentiment lexicons
library(tidyverse)
library(scales)
library(igraph) # Graphing Bigrams
library(ggraph) # Graphing Bigrams



# import raw data 
animal_farm <- read_lines(
  file = "http://gutenberg.net.au/ebooks01/0100011.txt",
  skip_empty_rows = TRUE,
  skip = 37, # remove metadata about book
  n_max = 2500) # remove appendix

nineteen_84 <- read_lines(
  file = "http://gutenberg.net.au/ebooks01/0100021.txt",
  skip_empty_rows = TRUE,
  skip = 38, # remove metadata about book
  n_max = 8344) # remove appendix

air <- read_lines(
  file = "http://gutenberg.net.au/ebooks02/0200031.txt",
  skip_empty_rows = TRUE,
  skip = 44, # remove metadata about book
  n_max = 8340) # remove appendix

elephant <- read_lines(
  file = "http://gutenberg.net.au/ebooks02/0200141.txt",
  skip_empty_rows = TRUE,
  skip = 39, # remove metadata about book
  n_max = 3000) # remove appendix

homage <- read_lines(
  file = "http://gutenberg.net.au/ebooks02/0201111.txt",
  skip_empty_rows = TRUE,
  skip = 40, # remove metadata about book
  n_max = 8000) # remove appendix

daughter <- read_lines(
  file = "http://gutenberg.net.au/ebooks02/0200011.txt",
  skip_empty_rows = TRUE,
  skip = 39, # remove metadata about book
  n_max = 10000) # remove appendix


# Load stop words: words that are not useful for an analysis, typically extremely common words such as “the”, “of”, “to”, and so forth in English
data(stop_words)

# Load all the books and then separate into each individual word
animal_df <- tibble(text = animal_farm) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [I-X]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [I-X]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

animal_tidy_df <- animal_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

daughter_df <- tibble(text = daughter) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

daughter_tidy_df <- daughter_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

nine_df <- tibble(text = nineteen_84) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [1-9]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [1-9]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

nine_tidy_df <- nine_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

air_df <- tibble(text = air) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^PART [IVXLCDM]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^PART [IVXLCDM]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

air_tidy_df <- air_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

homage_df <- tibble(text = homage) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

homage_tidy_df <- homage_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# combine all the books
sum_df_raw <- bind_rows(mutate(animal_df, Title = "Animal Farm"),
                    mutate(daughter_df, Title = "A Clergyman's Daughter"),
                    mutate(nine_df, Title = "Nineteen Eighty-Four"),
                    mutate(air_df, Title = "Coming up for Air"),
                    mutate(homage_df, Title = "Homage to Catalonia")
)

sum_df <- bind_rows(mutate(animal_tidy_df, Title = "Animal Farm"),
                    mutate(daughter_tidy_df, Title = "A Clergyman's Daughter"),
                    mutate(nine_tidy_df, Title = "Nineteen Eighty-Four"),
                    mutate(air_tidy_df, Title = "Coming up for Air"),
                    mutate(homage_tidy_df, Title = "Homage to Catalonia")
)

# load sentiment lexicons
tns <- getNamespace("textdata")
assignInNamespace(x = "printer", value = function(...) 1, ns = tns)
                  
bing <- tidytext::get_sentiments("bing") 
afinn <- tidytext::get_sentiments("afinn")
nrc <- tidytext::get_sentiments("nrc") 

# sentiment lexicons limitations
text_pos <- "This is my favorite book. I like it."
text_neg <- "This is not my favorite book. I don't like it."

df_limitations <- tibble(
  text = c(text_pos, text_neg),
  examples = c("text_pos", "text_neg")
)

df_limitations %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word") %>%
  inner_join(bing, by = "word")


# Data Frame used for conducting correlation study 
raw_frequency_plot <- sum_df %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(Title, word) %>%
  group_by(Title) %>%
  mutate(Proportion = n / sum(n)) %>% 
  select(-n)


  


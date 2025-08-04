# Loading required libraries
library(tidyverse)
library(tidytext)
library(textstem)
library(wordcloud)
library(RColorBrewer)
library(readxl)

data <- read_excel("/Users/guxujun/Library/CloudStorage/Dropbox/UMB/comments_ehr.xlsx")
View(data)

# Reshape data to treat each comment column as a document
comment_columns <- names(data)[grepl("^Comments", names(data))]

data_long <- data %>%
  select("Deidentified ID", all_of(comment_columns)) %>%
  pivot_longer(cols = all_of(comment_columns), names_to = "comment_category", values_to = "text") %>%
  filter(!is.na(text) & text != "") %>%
  # Aggregate text by comment category across all patients
  group_by(comment_category) %>%
  summarize(text = paste(text, collapse = " ")) %>%
  ungroup()

# Text preprocessing: Tokenize, remove stop words, stem, and clean
data_tokens <- data_long %>%
  unnest_tokens(word, text, token = "words") %>%
  # Remove stop words
  anti_join(stop_words, by = "word") %>%
  # Remove numbers and punctuation
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(str_detect(word, "^[a-zA-Z]+$")) %>%
  # Stem words to reduce inflectional forms
  mutate(word = lemmatize_words(word)) %>%
  # Remove short words
  filter(nchar(word) > 2)

# Calculate term frequency (TF)
tf <- data_tokens %>%
  count(comment_category, word, sort = TRUE) %>%
  group_by(comment_category) %>%
  mutate(tf = n / sum(n)) %>%
  ungroup()

# Calculate inverse document frequency (IDF)
idf <- data_tokens %>%
  count(word, sort = TRUE) %>%
  mutate(idf = log(nrow(data_long) / n))

# Compute TF-IDF
tf_idf <- tf %>%
  left_join(idf, by = "word") %>%
  mutate(tf_idf = tf * idf) %>%
  select(comment_category, word, tf_idf) %>%
  arrange(comment_category, desc(tf_idf))

# Summarize top TF-IDF terms per comment category
top_tf_idf <- tf_idf %>%
  group_by(comment_category) %>%
  slice_max(order_by = tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup()

# Print top TF-IDF terms
print("Top 5 TF-IDF terms per comment category:")
print(top_tf_idf)

# Visualize TF-IDF for each comment category
tf_idf_plots <- tf_idf %>%
  group_by(comment_category) %>%
  slice_max(order_by = tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(word, tf_idf, comment_category), y = tf_idf, fill = comment_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~comment_category, scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Top 10 TF-IDF Terms by Comment Category",
       x = "Term",
       y = "TF-IDF Score") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(tf_idf_plots)

# Save the TF-IDF results to a CSV file
write_csv(tf_idf, "tf_idf_by_comment_category.csv")

# Word cloud for most significant terms across all comment categories
top_terms <- tf_idf %>%
  group_by(word) %>%
  summarize(total_tf_idf = sum(tf_idf)) %>%
  slice_max(order_by = total_tf_idf, n = 50, with_ties = FALSE)

wordcloud(words = top_terms$word, freq = top_terms$total_tf_idf,
          min.freq = 0.1, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
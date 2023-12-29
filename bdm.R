library(tm)
library(textstem)
library(qdap)
library(dplyr)
library(wordcloud2)
library(dplyr)
library(tidytext)
library(textdata)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(stringr)
# select one from below
# Read the CSV file - assuming it has a column named 'text'
data <- read.csv(file.choose())  # You can replace 'file.choose()' with the path to your CSV file
# Read the text file from local machine , choose file interactively
x1 <- readLines(file.choose())

# Extract the 'text' column
x1 <- tolower(x1)
x1 <- removeWords(x1, c(stopwords(), 'i'))
x1 <- removePunctuation(x1)
x1 <- removeNumbers(x1)
x1 <- str_replace_all(x1, "\\s{2,}", " ")
x1 <- rm_non_words(x1)
x2 <- all_words(x1)
x3 <- x2 %>% arrange(-FREQ) %>% top_n(100)

# Wordcloud
wordcloud2(x3, shuffle = FALSE, minRotation = 0, maxRotation = 0)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(Corpus(VectorSource(x1)))
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)

# Display the top 10 most frequent words
head(dtm_d, 10)

# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col = "cyan", main = "Top 10 most frequent words",
        ylab = "Word frequencies")

# THEMATIC ANALYSIS SETUP
# Define the themes and their corresponding colors
themes <- c("Assessment", "Performance", "Engagement", "Personalization", "Analytics", "Intervention", "Adaptation", "Outcomes")
theme_colors <- c("blue", "green", "purple", "orange", "red", "yellow", "pink", "brown", "violet", "cyan")

# Create a function to count theme occurrences in text
count_theme_occurrences <- function(x1, themes) {
  theme_counts <- sapply(themes, function(theme) {
    sum(gregexpr(theme, x1, ignore.case = TRUE)[[1]] >= 0)
  })
  return(theme_counts)
}

# Combine the text into a single string
combined_text <- paste(x1, collapse = " ")

# Count theme occurrences in your text
theme_counts <- count_theme_occurrences(combined_text, themes)

# Create a data frame to store the theme counts
theme_counts_df <- data.frame(theme = themes, count = theme_counts)

## CREATING THE GRAPH

# Create a bar plot with colored bars
theme_plot <- ggplot(theme_counts_df, aes(x = theme, y = count, fill = theme)) +
  geom_bar(stat = "identity") +
  labs(title = "Theme Occurrences in Text", y = "Count") +
  scale_fill_manual(values = theme_colors) +  # Assign theme-specific colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(theme_plot)

# Create a bar plot for sentiment scores
x4 = get_nrc_sentiment(x1)
x4_long <- tidyr::gather(x4, key = "sentiment", value = "score")
ggplot(x4_long, aes(x = sentiment, y = score, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment Analysis Results", x = "Sentiment", y = "Sentiment Score") +
  theme_minimal()
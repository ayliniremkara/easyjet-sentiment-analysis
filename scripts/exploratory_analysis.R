# ── 0. Install packages if needed (run once, then re-comment) ────────────────
# install.packages(c("readxl", "dplyr", "tm", "wordcloud", "RColorBrewer"))

# ── 1. Load packages ─────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# ── 2. Set working directory to the script's folder ──────────────────────────
tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if (nzchar(script_dir)) setwd(script_dir)
  }
}, error = function(e) invisible(NULL))

# ── 3. Load sentiment output created by sentiment_analysis.R ─────────────────
input_path <- "outputs/Case18_EasyJet_Sentiment.xlsx"
if (!file.exists(input_path)) {
  stop("Sentiment output not found. Run sentiment_analysis.R first.")
}

sheet_name <- excel_sheets(input_path)[2]
df <- as.data.frame(read_excel(input_path, sheet = sheet_name))

required_cols <- c("comment", "comment_likes", "Sentiment_Label")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing columns in output file: ", paste(missing_cols, collapse = ", "))
}

df$comment_likes <- as.numeric(df$comment_likes)
df$Sentiment_Label <- trimws(df$Sentiment_Label)

sentiment_levels <- c("Positive", "Neutral", "Negative")

# ── 4. Sentiment frequency table ─────────────────────────────────────────────
counts <- table(factor(df$Sentiment_Label, levels = sentiment_levels))
total <- nrow(df)
percentage <- round(as.numeric(counts) / total * 100, 2)

distribution_table <- data.frame(
  Sentiment = names(counts),
  Count = as.integer(counts),
  Percentage = percentage,
  row.names = NULL
)

cat("\n=== Sentiment Frequency Table ===\n")
cat(sprintf("Total comments: %d\n\n", total))
print(distribution_table, row.names = FALSE)

# ── 5. Top-liked comment(s) per sentiment ────────────────────────────────────
# Returns ALL comments tied at the maximum like count for each sentiment.
top_liked_by_sentiment <- function(data) {
  results <- lapply(sentiment_levels, function(label) {
    subset_df <- data[data$Sentiment_Label == label, ]

    if (nrow(subset_df) == 0L) {
      return(data.frame(
        Sentiment = label,
        Username = NA_character_,
        Likes = NA_integer_,
        Comment = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    max_likes <- max(subset_df$comment_likes, na.rm = TRUE)
    top_rows <- subset_df[subset_df$comment_likes == max_likes, ]

    data.frame(
      Sentiment = label,
      Username = if ("username" %in% names(top_rows)) top_rows$username else NA_character_,
      Likes = as.integer(top_rows$comment_likes),
      Comment = top_rows$comment,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

top_liked <- top_liked_by_sentiment(df)

cat("\n=== Top-Liked Comment(s) per Sentiment ===\n")
for (i in seq_len(nrow(top_liked))) {
  row <- top_liked[i, ]
  cat(sprintf(
    "\n[%s]\n  User   : %s\n  Likes  : %d\n  Comment: %s\n",
    row$Sentiment,
    ifelse(is.na(row$Username), "(unknown)", row$Username),
    ifelse(is.na(row$Likes), 0L, row$Likes),
    ifelse(is.na(row$Comment), "(none)", row$Comment)
  ))
}

# ── 6. Text cleaning ─────────────────────────────────────────────────────────
custom_stopwords <- c(
  stopwords("en"),
  "easyjet", "easyjets", "assistant", "bot", "will"
)

clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, custom_stopwords)
  text <- stripWhitespace(text)
  return(text)
}

df$cleaned_comments <- sapply(df$comment, clean_text)

# ── 7. Detailed word frequency analysis ──────────────────────────────────────
get_top_words <- function(texts, n = 20) {
  words <- unlist(strsplit(paste(texts, collapse = " "), " "))
  words <- words[nchar(words) > 0]
  freq <- sort(table(words), decreasing = TRUE)
  head(freq, n)
}

top_all <- get_top_words(df$cleaned_comments)
top_pos <- get_top_words(df$cleaned_comments[df$Sentiment_Label == "Positive"])
top_neu <- get_top_words(df$cleaned_comments[df$Sentiment_Label == "Neutral"])
top_neg <- get_top_words(df$cleaned_comments[df$Sentiment_Label == "Negative"])

cat("\n=== Top 20 Words — All Comments ===\n")
print(top_all)

cat("\n=== Top 20 Words — Positive ===\n")
print(top_pos)

cat("\n=== Top 20 Words — Neutral ===\n")
print(top_neu)

cat("\n=== Top 20 Words — Negative ===\n")
print(top_neg)

# ── 8. Comparative top-20 word frequency table ───────────────────────────────
make_df <- function(freq_vec, word_col, count_col) {
  data.frame(
    Word = names(freq_vec),
    Count = as.integer(freq_vec),
    stringsAsFactors = FALSE,
    row.names = NULL
  ) |> setNames(c(word_col, count_col))
}

comp_table <- cbind(
  make_df(top_all, "All_Word", "All_n"),
  make_df(top_pos, "Positive_Word", "Positive_n"),
  make_df(top_neu, "Neutral_Word", "Neutral_n"),
  make_df(top_neg, "Negative_Word", "Negative_n")
)

cat("\n=== Comparative Top-20 Word Frequency Table ===\n")
print(comp_table, row.names = FALSE)

# ── 9. Word clouds → outputs/ ────────────────────────────────────────────────
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
cat("\nGenerating word clouds...\n")

# All comments
png("outputs/wordcloud_all.png", width = 800, height = 600)
wordcloud(df$cleaned_comments,
          max.words = 80,
          min.freq = 2,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
title(main = "Word Cloud — All Comments")
dev.off()

# Per sentiment
for (sentiment in sentiment_levels) {
  subset_comments <- df$cleaned_comments[df$Sentiment_Label == sentiment]
  png(paste0("outputs/wordcloud_", sentiment, ".png"), width = 800, height = 600)
  wordcloud(subset_comments,
            max.words = 80,
            min.freq = 2,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2"))
  title(main = paste("Word Cloud —", sentiment))
  dev.off()
}

cat("Word clouds saved to outputs/\n")

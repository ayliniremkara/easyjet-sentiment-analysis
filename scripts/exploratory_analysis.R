# ── 0. Packages
library(readxl)

# ── 1. Load sentiment output ──────────────────────────────────────────────
input_path <- "data/raw/Dataset1/Case18_EasyJet_Sentiment.xlsx"
if (!file.exists(input_path)) {
  stop("Sentiment output not found. Run sentiment_analysis.R first.")
}

sheet_name <- excel_sheets(input_path)[2]
df <- as.data.frame(read_excel(input_path, sheet = sheet_name))

required_cols <- c("comment", "comment_likes", "Sentiment_Label")
missing_cols  <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing columns in output file: ", paste(missing_cols, collapse = ", "))
}

df$comment_likes   <- as.numeric(df$comment_likes)
df$Sentiment_Label <- trimws(df$Sentiment_Label)

# ── 2. Sentiment distribution table ────────────────────────────────────────
sentiment_levels <- c("Positive", "Neutral", "Negative")

counts  <- table(factor(df$Sentiment_Label, levels = sentiment_levels))
total   <- nrow(df)
density <- round(as.numeric(counts) / total * 100, 2)

distribution_table <- data.frame(
  Sentiment  = names(counts),
  Count      = as.integer(counts),
  Density_pct = density,
  row.names  = NULL
)

cat("\n=== Sentiment Distribution ===\n")
cat(sprintf("Total comments: %d\n\n", total))
print(distribution_table, row.names = FALSE)

# ── 3. Top-liked comment per sentiment ───────────────────────────────────────

#' Return the comment with the most likes for each sentiment category.
#'
#' @param data  Data frame with at least Sentiment_Label, comment, comment_likes.
#' @return      A data frame with one row per sentiment label.
top_liked_by_sentiment <- function(data) {
  results <- lapply(sentiment_levels, function(label) {
    subset_df <- data[data$Sentiment_Label == label, ]

    if (nrow(subset_df) == 0L) {
      return(data.frame(
        Sentiment    = label,
        Username     = NA_character_,
        Likes        = NA_integer_,
        Comment      = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    top_row <- subset_df[which.max(subset_df$comment_likes), ]

    data.frame(
      Sentiment = label,
      Username  = if ("username" %in% names(top_row)) top_row$username else NA_character_,
      Likes     = as.integer(top_row$comment_likes),
      Comment   = top_row$comment,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

top_liked <- top_liked_by_sentiment(df)

cat("\n=== Top-Liked Comment per Sentiment ===\n")
for (i in seq_len(nrow(top_liked))) {
  row <- top_liked[i, ]
  cat(sprintf(
    "\n[%s]\n  User   : %s\n  Likes  : %d\n  Comment: %s\n",
    row$Sentiment,
    ifelse(is.na(row$Username), "(unknown)", row$Username),
    ifelse(is.na(row$Likes),    0L,          row$Likes),
    ifelse(is.na(row$Comment),  "(none)",    row$Comment)
  ))
}

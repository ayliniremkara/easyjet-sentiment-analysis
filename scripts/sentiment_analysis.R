# ── 0. Install R packages if needed (run once, then re-comment) ───────────────
#install.packages(c("readxl", "openxlsx", "reticulate"))

# ── 1. Load packages ──────────────────────────────────────────────────────────
library(readxl)     # read Excel for detection + preview
library(openxlsx)   # load / modify / save workbook in place
library(reticulate) # bridge to Python / transformers

# ── 2. Set working directory to the script's folder ───────────────────────────
tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if (nzchar(script_dir)) setwd(script_dir)
  }
}, error = function(e) invisible(NULL))

# ── 3. Locate Python (hard stop if absent; skip Windows Store stub) ───────────
py_path <- Sys.which("python")
# Reject the Windows App Store stub — it is inaccessible and not a real interpreter
if (grepl("WindowsApps", py_path, fixed = TRUE)) py_path <- ""
if (!nzchar(py_path)) py_path <- Sys.which("python3")
if (grepl("WindowsApps", py_path, fixed = TRUE)) py_path <- ""
if (!nzchar(py_path)) {
  stop("Python not found on PATH. Install Python 3 and ensure 'python' or ",
       "'python3' is accessible before running this script.")
}
cat("Using Python:", py_path, "\n")

# ── 4. Install missing Python packages via pip ────────────────────────────────
use_python(py_path, required = TRUE)

required_py <- c("transformers", "torch", "scipy")
missing_py  <- required_py[!sapply(required_py, py_module_available)]
if (length(missing_py) > 0) {
  message("Installing missing Python packages: ", paste(missing_py, collapse = ", "))
  ret <- system2(py_path, args = c("-m", "pip", "install", "--quiet", missing_py))
  if (ret != 0L) stop("pip install failed for: ", paste(missing_py, collapse = ", "))
}

# ── 5. Keep all HuggingFace downloads inside the project ──────────────────────
hf_cache <- normalizePath("hf-cache", mustWork = FALSE)
dir.create(hf_cache, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(HF_HOME = hf_cache)

# ── 6. Wire reticulate to Python: add python-libs/ to sys.path, import libs ───
py_libs <- normalizePath("python-libs", mustWork = FALSE)
dir.create(py_libs, showWarnings = FALSE, recursive = TRUE)

py_run_string(paste0("import sys\nif r'", py_libs, "' not in sys.path: sys.path.insert(0, r'", py_libs, "')"))

transformers <- import("transformers")
torch        <- import("torch")

# ── 7. Build the sentiment-analysis pipeline ──────────────────────────────────
HF_MODEL <- "cardiffnlp/twitter-roberta-base-sentiment-latest"
cat("\nLoading model:", HF_MODEL, "\n")
sentiment_pipeline <- transformers$pipeline(
  task  = "sentiment-analysis",
  model = HF_MODEL
)

# ── 8. Read the first sheet; auto-detect the first character/text column ───────
input_path  <- "data/raw/Dataset1/Case18_Easyjet.xlsx"
output_path <- "data/raw/Dataset1/Case18_EasyJet_Sentiment.xlsx"

stopifnot("Input and output paths must differ" =
            normalizePath(input_path,  mustWork = FALSE) !=
            normalizePath(output_path, mustWork = FALSE))

sheet_name <- excel_sheets(input_path)[2]   # always the second sheet
df         <- read_excel(input_path, sheet = sheet_name)

comment_col_idx  <- 2                        # always the second column
comment_col_name <- names(df)[comment_col_idx]
cat("Sheet used       :", sheet_name, "\n")
cat("Comment column   :", comment_col_name,
    "(column index", comment_col_idx, ")\n")

# ── 9. Run the pipeline; extract plain labels and percentage scores ─────────────
comments <- as.character(df[[comment_col_idx]])
comments[is.na(comments) | nchar(trimws(comments)) == 0] <- "."

cat("\nRunning sentiment pipeline — this may take a moment...\n")
raw_results <- sentiment_pipeline(as.list(comments))

labels         <- sapply(raw_results, function(x) tools::toTitleCase(x$label))
confidence_pct <- round(sapply(raw_results, function(x) x$score) * 100, 2)

cat("\nSentiment distribution:\n")
print(table(labels))

# ── 10. Open original workbook; append Sentiment_Label and Confidence_% ────────
wb <- loadWorkbook(input_path)   # preserves all existing formatting

label_col <- comment_col_idx + 2
conf_col  <- comment_col_idx + 3

# Headers in row 1
writeData(wb, sheet = sheet_name, x = "Sentiment_Label",
          startCol = label_col, startRow = 1, colNames = FALSE)
writeData(wb, sheet = sheet_name, x = "Confidence_%",
          startCol = conf_col,  startRow = 1, colNames = FALSE)

# Values starting in row 2
writeData(wb, sheet = sheet_name, x = data.frame(labels),
          startCol = label_col, startRow = 2, colNames = FALSE)
writeData(wb, sheet = sheet_name, x = data.frame(confidence_pct),
          startCol = conf_col,  startRow = 2, colNames = FALSE)

# ── 11. Save as a new file — never overwrite the input ────────────────────────
saveWorkbook(wb, file = output_path, overwrite = TRUE)
cat("\nSaved output workbook:", output_path, "\n")

# ── 12. Verify the output file exists and print a preview ─────────────────────
if (file.exists(output_path)) {
  cat("Output file confirmed.\n\n")
  preview <- read_excel(output_path, sheet = sheet_name)
  print(head(preview))
} else {
  stop("Output file was NOT created — check path and permissions.")
}

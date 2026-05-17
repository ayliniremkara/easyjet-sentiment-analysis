# Load packages
library(readxl)
library(readr)

# Dataset 1
case18_easyjet <- read_excel(
  "data/raw/Dataset1/Case18_Easyjet.xlsx"
)

# Dataset 2 - KPI Video 1
kpi_video_1 <- read_csv(
  "data/raw/Dataset2/KPIs_Video_1.csv"
)

# Dataset 2 - KPI Video 2
kpi_video_2 <- read_csv(
  "data/raw/Dataset2/KPIs_Video_2.csv"
)

# Inspect datasets
head(case18_easyjet)
head(kpi_video_1)
head(kpi_video_2)
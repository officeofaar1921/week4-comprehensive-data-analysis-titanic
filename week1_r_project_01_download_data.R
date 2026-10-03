# Week 1 - Dataset download
# Run this script first.

url <- "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/titanic.csv"
out_file <- file.path("data", "titanic.csv")

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

download.file(url, out_file, mode = "wb")
cat("Downloaded dataset to:", out_file, "\n")

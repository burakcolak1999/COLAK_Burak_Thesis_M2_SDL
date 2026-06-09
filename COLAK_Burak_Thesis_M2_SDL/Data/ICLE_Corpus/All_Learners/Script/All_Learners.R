# Appendix B: R Code for ICLE Corpus Statistical Analysis (ALL LEARNERS)
# Purpose: This script performs statistical analysis on the ICLE Corpus all learner irregular verb error dataset. 
# It provides a comprehensive overview by evaluating the distribution of targeted 
# irregular verb types across language families, while enabling a detailed 
# cross-tabulation of language families by specific error types to identify 
# systematic morphological challenges. Additionally, it performs a micro-analysis 
# to highlight the most error-prone verb forms. The original code remains 
# unchanged to ensure full transparency and reproducibility.

# ── 1. Necessary Libraries and Data Loading ──────────────────
library(dplyr)
library(readr)

# Set working directory
# Note: Ensure your working directory points to the folder containing the data file.
setwd("~/Desktop/COLAK_Burak_Thesis/Data : Irregular Verb Errors/ICLE Errors/All Learners/CSV")

# Read the full dataset
df <- read_delim("irregular_verbs_mca_data_full_list.csv", delim = ";", skip = 3, trim_ws = TRUE)

# ── 2. Global Cross-Tabulation (Raw Frequencies) ─────────────
cat("--- GLOBAL CROSS-TABULATION: ALL LANGUAGE FAMILIES BY ERROR TYPES ---\n")
global_ct <- table(df$Language_Family, df$Error_Type)
print(global_ct)

# ── 3. Global Row Percentages (Proportional Analysis) ───────
cat("\n--- ROW PERCENTAGES BY ALL LANGUAGE FAMILIES ---\n")
global_prop <- prop.table(global_ct, margin = 1) * 100
print(round(global_prop, 2))

# ── 4. Interaction: Language Families by Irregular Verb Types ──
cat("\n--- ALL LANGUAGE FAMILIES BY IRREGULAR VERB TYPES ---\n")
verb_family_ct <- table(df$Language_Family, df$`Irregular Verb Type`)
print(verb_family_ct)

# ── 5. Dynamic Micro-Analysis: Top 3 Most Problematic Verbs ──
# This code automatically identifies the 3 verbs with the highest total error counts
top_3_verbs <- df %>%
  count(Verb) %>%
  slice_max(n, n = 3) %>%
  pull(Verb)

cat("\n--- ERROR BREAKDOWN FOR TOP 3 MOST FREQUENT ERROR-PRONE VERBS:", paste(top_3_verbs, collapse = ", "), "---\n")

top_verbs_breakdown <- df %>%
  filter(Verb %in% top_3_verbs) %>%
  count(Verb, Error_Type) %>%
  group_by(Verb) %>%
  mutate(Percentage = (n / sum(n)) * 100)

print(top_verbs_breakdown)
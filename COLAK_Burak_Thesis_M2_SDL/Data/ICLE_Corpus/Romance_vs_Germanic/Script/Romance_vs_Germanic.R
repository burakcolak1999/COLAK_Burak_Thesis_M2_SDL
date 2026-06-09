# Appendix C: R Code for Comparative Analysis (Romance & Germanic Families)
# Purpose: This script performs a comparative statistical analysis on the ICLE Corpus 
# dataset focusing on Romance and Germanic language families. It provides a 
# systematic analytical framework by evaluating the distribution of errors across 
# different language backgrounds and education durations. Furthermore, it facilitates 
# a detailed visualization of error patterns and examines morphological stability 
# through verb-type analysis and inferential statistics (Chi-Square Test) to identify 
# significant differences between these linguistic groups. The original code remains 
# unchanged to ensure full transparency and reproducibility.

# --- Load necessary libraries ---
library(tidyverse)
library(ggplot2)

# --- Load the dataset ---
df <- read.csv("irregular_verbs_mca_data_romance_germanic.csv", skip = 2, sep = ";")

# --- 1. Comparative Tables ---
cat("--- 4.1.2 Error Distribution by Language Family ---\n")
print(df %>% 
        group_by(Language_Family, Error_Type) %>% 
        summarise(Count = n(), .groups = 'drop') %>% 
        mutate(Percentage = round(Count / sum(Count) * 100, 2)))

cat("\n--- 4.1.2 Error Analysis by Education Duration ---\n")
print(df %>% 
        group_by(Years.of.English.at.School, Error_Type) %>% 
        summarise(Count = n(), .groups = 'drop'))

# --- 2. Visualization: Group 1 (3-4 years + Unknown) ---
df_group1 <- df %>% 
  filter(Years.of.English.at.School %in% c("3 years", "4 years", "Unknown"))

ggplot(df_group1, aes(x = Error_Type, fill = Language_Family)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Years.of.English.at.School) +
  theme_minimal() +
  labs(title = "Distribution of Error Types (3-4 Years and Unknown)", 
       x = "Error Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 3. Visualization: Group 2 (5-6+ years) ---
df_group2 <- df %>% 
  filter(Years.of.English.at.School %in% c("5 years", "6 years or more"))

ggplot(df_group2, aes(x = Error_Type, fill = Language_Family)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Years.of.English.at.School) +
  theme_minimal() +
  labs(title = "Distribution of Error Types (5-6+ Years)", 
       x = "Error Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 4. In-depth Verb Type Analysis ---
cat("\n--- 4.1.2 Verb Type Analysis by Language Family ---\n")
print(df %>% 
        group_by(Language_Family, Irregular.Verb.Type) %>% 
        summarise(Count = n(), .groups = 'drop'))

# --- 5. Statistical Analysis ---
cat("\n--- 4.1.2 Chi-Square Test Result ---\n")
contingency_table <- table(df$Language_Family, df$Error_Type)
print(chisq.test(contingency_table))
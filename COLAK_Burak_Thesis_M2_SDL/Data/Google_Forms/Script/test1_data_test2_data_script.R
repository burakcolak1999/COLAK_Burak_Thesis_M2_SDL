# ==============================================================================
# Appendix E: Test 1 and Test 2 Analysis
# ==============================================================================

library(dplyr)
library(stringr)

# --- 1. PROCESS TEST 1 (Columns 5 to 29 contain X1 to X25) ---
test1_cleaned <- test1_data

# Automatically trim whitespace from all character columns
test1_cleaned[] <- lapply(test1_cleaned, function(x) if(is.character(x)) trimws(x) else x)

test1_cleaned <- test1_cleaned %>%
  mutate(
    # Multiple-Choice (X1 to X14) -> Columns 5 to 18
    X1_err  = case_when(.[[5]] == "maded" ~ "overgeneralized_ed", TRUE ~ "correct"), 
    X2_err  = "correct",
    X3_err  = case_when(.[[7]] == "writed" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X4_err  = "correct",
    X5_err  = case_when(.[[9]] %in% c("catched", "caughted") ~ "overgeneralized_ed", TRUE ~ "correct"),
    X6_err  = case_when(.[[10]] == "finded" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X7_err  = "correct",
    X8_err  = case_when(.[[12]] == "have been" ~ "auxiliary_error", TRUE ~ "correct"),
    X9_err  = case_when(.[[13]] == "had have" ~ "auxiliary_error", TRUE ~ "correct"),
    X10_err = case_when(.[[14]] == "has came" ~ "auxiliary_error", TRUE ~ "correct"),
    X11_err = case_when(.[[15]] == "boughted" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X12_err = case_when(.[[16]] == "losen" ~ "incorrect_participle", TRUE ~ "correct"),
    X13_err = case_when(.[[17]] == "ate" ~ "incorrect_participle", TRUE ~ "correct"),
    X14_err = case_when(.[[18]] == "saw" ~ "incorrect_participle", TRUE ~ "correct"),
    
    # Grammaticality Judgment (X15 to X18) -> Columns 19 to 22
    X15_err = case_when(.[[19]] == "Correct" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X16_err = case_when(.[[20]] == "Correct" ~ "incorrect_participle", TRUE ~ "correct"),
    X17_err = case_when(.[[21]] == "Incorrect" ~ "incorrect_participle", TRUE ~ "correct"),
    X18_err = case_when(.[[22]] == "Incorrect" ~ "incorrect_participle", TRUE ~ "correct"),
    
    # Fill-in-the-Blanks (X19 to X25) -> Columns 23 to 29
    X19_err = case_when(.[[23]] %in% c("Didn't sang", "Haven't sang", "Haven't sing") ~ "incorrect_participle", TRUE ~ "correct"),
    X20_err = case_when(.[[24]] == "Didn't cutted" ~ "overgeneralized_ed", .[[24]] == "Haven't cut" ~ "auxiliary_error", TRUE ~ "correct"),
    X21_err = case_when(.[[25]] == "Didn't puted" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X22_err = case_when(.[[26]] %in% c("Didn't payed", "Haven't payed") ~ "overgeneralized_ed", .[[26]] == "Didn't paid" ~ "incorrect_participle", TRUE ~ "correct"),
    X23_err = case_when(.[[27]] == "Didn't thought" ~ "incorrect_participle", TRUE ~ "correct"),
    X24_err = case_when(.[[28]] %in% c("Didn't took", "Haven't took", "Haven't take", "Didn't taken") ~ "incorrect_participle", TRUE ~ "correct"),
    X25_err = case_when(.[[29]] == "Hasn't drived" ~ "overgeneralized_ed", .[[29]] %in% c("Didn't drove", "Hasn't drive", "Hasn't drove") ~ "incorrect_participle", .[[29]] == "Didn't drive" ~ "auxiliary_error", TRUE ~ "correct")
  )

# --- 2. PROCESS TEST 2 (Columns 5 to 24 contain X1 to X20) ---
test2_cleaned <- test2_data

test2_cleaned[] <- lapply(test2_cleaned, function(x) if(is.character(x)) trimws(x) else x)

test2_cleaned <- test2_cleaned %>%
  mutate(
    X1_err  = "correct", # Column 5
    X2_err  = case_when(str_detect(.[[6]], "payed") ~ "overgeneralized_ed", str_detect(.[[6]], "paid") ~ "incorrect_participle", TRUE ~ "correct"),
    X3_err  = case_when(str_detect(.[[7]], "Wroted") ~ "overgeneralized_ed", str_detect(.[[7]], "Written") ~ "incorrect_participle", TRUE ~ "correct"),
    X4_err  = case_when(str_detect(.[[8]], "Goed") ~ "overgeneralized_ed", TRUE ~ "correct"),
    X5_err  = case_when(.[[9]] %in% c("Puted", "Putted") ~ "overgeneralized_ed", TRUE ~ "correct"),
    X6_err  = case_when(str_detect(.[[10]], "comed") ~ "overgeneralized_ed", str_detect(.[[10]], "came") ~ "incorrect_participle", TRUE ~ "correct"),
    X7_err  = case_when(.[[11]] == "Been" ~ "incorrect_participle", .[[11]] == "Had been" ~ "auxiliary_error", TRUE ~ "correct"),
    X8_err  = case_when(.[[12]] == "Builded" ~ "overgeneralized_ed", TRUE ~ "correct"),
    X9_err  = case_when(str_detect(.[[13]], "sayed") ~ "overgeneralized_ed", str_detect(.[[13]], "said") ~ "incorrect_participle", TRUE ~ "correct"),
    X10_err = case_when(.[[14]] == "Chosed" ~ "overgeneralized_ed", .[[14]] == "Choose" ~ "incorrect_participle", TRUE ~ "correct"),
    X11_err = case_when(.[[15]] %in% c("Has been", "Was") ~ "auxiliary_error", TRUE ~ "correct"),
    X12_err = case_when(.[[16]] %in% c("Had fled", "Had flight", "Had fly", "Had flough", "Had flew") ~ "incorrect_participle", .[[16]] %in% c("Flied", "Flyed") ~ "overgeneralized_ed", .[[16]] %in% c("Flew", "Has flown", "Flow") ~ "auxiliary_error", TRUE ~ "correct"),
    X13_err = case_when(.[[17]] %in% c("Had became", "Has became") ~ "incorrect_participle", .[[17]] %in% c("Has become", "Became") ~ "auxiliary_error", TRUE ~ "correct"),
    X14_err = case_when(.[[18]] %in% c("Has broke", "Had broke") ~ "incorrect_participle", .[[18]] == "Broke" ~ "auxiliary_error", TRUE ~ "correct"),
    X15_err = case_when(.[[19]] %in% c("Haven't drank", "Hadn't drank", "Haven't drink", "Hadn't drink", "Didn't drank") ~ "incorrect_participle", .[[19]] %in% c("Haven't drunk", "Didn't drink") ~ "auxiliary_error", TRUE ~ "correct"),
    X16_err = case_when(.[[20]] == "Have understand" ~ "incorrect_participle", .[[20]] %in% c("Understood", "Had understood") ~ "auxiliary_error", TRUE ~ "correct"),
    X17_err = case_when(.[[21]] %in% c("Had rose", "Had rise", "Had rosen") ~ "incorrect_participle", .[[21]] %in% c("Has rised", "Had rised") ~ "overgeneralized_ed", .[[21]] == "Has risen" ~ "auxiliary_error", TRUE ~ "correct"),
    X18_err = case_when(.[[22]] %in% c("Haven't spoke", "Didn't spoken", "Didn't spoke") ~ "incorrect_participle", .[[22]] %in% c("Didn't speak", "Hadn't spoken") ~ "auxiliary_error", TRUE ~ "correct"),
    X19_err = case_when(.[[23]] == "Had forgot" ~ "incorrect_participle", .[[23]] %in% c("Have forgotten", "Forgot", "Had forget") ~ "auxiliary_error", TRUE ~ "correct"),
    X20_err = case_when(.[[24]] %in% c("Didn't had", "Hadn't have") ~ "incorrect_participle", .[[24]] %in% c("Hasn't had", "Didn't have") ~ "auxiliary_error", TRUE ~ "correct")
  )

# --- 3. VERIFICATION ---
cat("\n--- PIPELINE COMPLETED SUCCESSFULLY ---\n")
print(paste("Test 1 Cleaned Columns:", ncol(test1_cleaned)))
print(paste("Test 2 Cleaned Columns:", ncol(test2_cleaned)))


# ==============================================================================
# ERROR TYPE FREQUENCY CALCULATIONS
# ==============================================================================

library(tidyr)

cat("\n=========================================\n")
cat("   TEST 1: GLOBAL ERROR DISTRIBUTION     ")
cat("\n=========================================\n")

test1_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Error_Type) %>%
  summarise(
    Count = n(),
    Percentage = round((n() / (nrow(test1_cleaned) * 25)) * 100, 2)
  ) %>%
  arrange(desc(Count)) %>%
  print()

cat("\n=========================================\n")
cat("   TEST 2: GLOBAL ERROR DISTRIBUTION     ")
cat("\n=========================================\n")

test2_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Error_Type) %>%
  summarise(
    Count = n(),
    Percentage = round((n() / (nrow(test2_cleaned) * 20)) * 100, 2)
  ) %>%
  arrange(desc(Count)) %>%
  print()


# ==============================================================================
# GLOBAL ERROR VISUALIZATION
# ==============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Prepare and combine the summary data
t1_summary <- test1_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Error_Type) %>%
  summarise(Count = n(), Total_Tasks = nrow(test1_cleaned) * 25) %>%
  mutate(Percentage = (Count / Total_Tasks) * 100, Test = "Test 1 (Mixed Formats)")

t2_summary <- test2_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Error_Type) %>%
  summarise(Count = n(), Total_Tasks = nrow(test2_cleaned) * 20) %>%
  mutate(Percentage = (Count / Total_Tasks) * 100, Test = "Test 2 (Production)")

combined_summary <- bind_rows(t1_summary, t2_summary)

# 2. Create the Academic Bar Plot
error_plot <- ggplot(combined_summary, aes(x = Error_Type, y = Percentage, fill = Test)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", size = 0.3) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(0.8), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Test 1 (Mixed Formats)" = "#4A6572", "Test 2 (Production)" = "#F9AA33")) +
  scale_x_discrete(labels = c("auxiliary_error" = "Auxiliary Error", 
                              "incorrect_participle" = "Incorrect Participle", 
                              "overgeneralized_ed" = "Overgeneralized -ed")) +
  labs(
    x = "Linguistic Error Categories",
    y = "Global Error Rate (%)",
    fill = "Assessment Type"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 12)),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  ylim(0, 25)

# Display the plot
print(error_plot)

# Save the plot in high definition for the thesis document
# ggplot2 will save it directly to your current working directory
ggsave("global_error_distribution_plot.png", plot = error_plot, width = 8, height = 5, dpi = 300)


# ==============================================================================
# QUESTION-LEVEL ERROR RANKINGS 
# ==============================================================================

cat("\n=========================================\n")
cat("   TEST 1: TOP 5 MOST ERROR-PRONE QUESTIONS ")
cat("\n=========================================\n")

test1_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Question, Error_Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count)) %>%
  head(5) %>%
  print()

cat("\n=========================================\n")
cat("   TEST 2: TOP 5 MOST ERROR-PRONE QUESTIONS ")
cat("\n=========================================\n")

test2_cleaned %>%
  select(ends_with("_err")) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Question, Error_Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count)) %>%
  head(5) %>%
  print()


# ==============================================================================
# EMPIRICAL ANALYSIS: PERFORMANCE BY NATIVE LANGUAGE (TEST 1 & TEST 2)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. PREPARE UNIFIED DATA (Native Language)
lang_data_master <- bind_rows(
  test1_cleaned %>% select(Native_Language, ends_with("_err")) %>% 
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 1"),
  test2_cleaned %>% select(Native_Language, ends_with("_err")) %>% 
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 2")
)

# 2. CONSOLE OUTPUT: APA 7 READY TABLES
cat("\n--- TABLE: PERFORMANCE BY NATIVE LANGUAGE (TEST 1 & 2) ---\n")
lang_data_master %>%
  group_by(Test, Native_Language, Response_Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Response_Type, values_from = Count, values_fill = 0) %>%
  print()

# 3. VISUALIZATION: PERFORMANCE BY NATIVE LANGUAGE
ggplot(lang_data_master, aes(x = Response_Type, fill = Native_Language)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Test) +
  theme_minimal() +
  labs(title = "Performance Distribution by Native Language",
       subtitle = "Comparative Analysis (Errors & Correct Answers)",
       x = "Response Category", 
       y = "Frequency",
       fill = "Native Language") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# EMPIRICAL CROSS-TABULATION FOR AGE GROUPS (TEST 1 & TEST 2)
# ==============================================================================

cat("\n==================================================\n")
cat("   TEST 1: ERROR TYPES BY AGE")
cat("\n==================================================\n")

test1_cleaned %>%
  select(Age, ends_with("_err")) %>%
  pivot_longer(cols = ends_with("_err"), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Age, Error_Type) %>%
  summarise(Error_Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Error_Type, values_from = Error_Count, values_fill = 0) %>%
  print()

cat("\n==================================================\n")
cat("   TEST 2: ERROR TYPES BY AGE")
cat("\n==================================================\n")

test2_cleaned %>%
  select(Age, ends_with("_err")) %>%
  pivot_longer(cols = ends_with("_err"), names_to = "Question", values_to = "Error_Type") %>%
  filter(Error_Type != "correct") %>%
  group_by(Age, Error_Type) %>%
  summarise(Error_Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Error_Type, values_from = Error_Count, values_fill = 0) %>%
  print()


# ==============================================================================
# EMPIRICAL CROSS-TABULATION FOR COUNTRIES (TEST 1 & TEST 2)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. PREPARE UNIFIED DATA (All responses: Errors + Correct)
master_data <- bind_rows(
  test1_cleaned %>% select(Country, ends_with("_err")) %>% 
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 1"),
  test2_cleaned %>% select(Country, ends_with("_err")) %>% 
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 2")
)

# 2. GENERATE SINGLE MASTER GRAPH
ggplot(master_data, aes(x = Response_Type, fill = Country)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Test) +
  theme_minimal() +
  labs(title = "Performance Distribution by Country (Errors & Correct)",
       x = "Response Category", 
       y = "Frequency",
       fill = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. CONSOLE OUTPUT (APA 7 Table Data)
cat("\n--- SUMMARY TABLE FOR APA 7 (MASTER DATA) ---\n")
master_data %>%
  group_by(Test, Country, Response_Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Response_Type, values_from = Count, values_fill = 0) %>%
  print()


# ============================================================================== 
# PERFORMANCE BY YEARS OF ENGLISH (0-10 & 11-14 YEARS)
# ============================================================================== 

library(dplyr) 
library(tidyr) 
library(ggplot2) 

# 1. PREPARE UNIFIED DATA AND APPLY CATEGORICAL GROUPING 
# We clean 'Years_English' (remove "years" text) to ensure accurate grouping
master_data_years <- bind_rows( 
  test1_cleaned %>% select(Years_English, ends_with("_err")) %>%  
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 1"), 
  test2_cleaned %>% select(Years_English, ends_with("_err")) %>%  
    pivot_longer(ends_with("_err"), values_to = "Response_Type") %>% mutate(Test = "Test 2") 
) %>% 
  # Clean text to numeric: "13 years" becomes 13
  mutate(Years_English = as.numeric(gsub("[^0-9]", "", Years_English))) %>%
  mutate(Years_Group = case_when( 
    Years_English <= 10 ~ "0-10 Years", 
    Years_English > 10  ~ "11-14 Years", 
    TRUE ~ "Unknown" 
  )) 

# 2. CONSOLE OUTPUT: APA 7 READY TABLES 
cat("\n--- TABLE: PERFORMANCE BY YEARS OF ENGLISH (GROUPED) ---\n") 
master_data_years %>% 
  group_by(Test, Years_Group, Response_Type) %>% 
  summarise(Count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = Response_Type, values_from = Count, values_fill = 0) %>% 
  print() 

# 3. VISUALIZATION: PERFORMANCE BY YEARS OF ENGLISH (GROUPED) 
ggplot(master_data_years, aes(x = Response_Type, fill = Years_Group)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~Test) + 
  theme_minimal() + 
  labs(title = "Performance Distribution by Years of English Instruction", 
       subtitle = "Grouped Analysis (0-10 vs 11-14 Years)", 
       x = "Response Category",  
       y = "Frequency", 
       fill = "Years of English") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# PARTICIPANT-LEVEL ERROR SUMMARY
# ==============================================================================

cat("\n==================================================\n")
cat("   TEST 1: AVERAGE ERRORS PER PARTICIPANT")
cat("\n==================================================\n")

test1_cleaned %>%
  mutate(Total_Errors = rowSums(select(., ends_with("_err")) != "correct")) %>%
  summarise(
    Average_Errors = mean(Total_Errors),
    Median_Errors = median(Total_Errors),
    Max_Errors_by_Single_Student = max(Total_Errors)
  ) %>%
  print()

cat("\n==================================================\n")
cat("   TEST 2: AVERAGE ERRORS PER PARTICIPANT")
cat("\n==================================================\n")

test2_cleaned %>%
  mutate(Total_Errors = rowSums(select(., ends_with("_err")) != "correct")) %>%
  summarise(
    Average_Errors = mean(Total_Errors),
    Median_Errors = median(Total_Errors),
    Max_Errors_by_Single_Student = max(Total_Errors)
  ) %>%
  print()


# ==============================================================================
# MORPHOLOGICAL ERROR DISTRIBUTION 
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. PROCESS FUNCTION (Gathers only the actual processed error columns)
process_test_corrected <- function(cleaned_df, test_name, mapping_logic) {
  cleaned_df %>%
    # Select only the error recoded columns
    select(ends_with("_err")) %>%
    # Convert to long format
    pivot_longer(cols = everything(), names_to = "Q_Num", values_to = "Status") %>%
    # Create an index for mapping (1 to 25 for T1, 1 to 20 for T2)
    group_by(Q_Num) %>% 
    mutate(Q_Idx = as.numeric(str_extract(Q_Num, "\\d+"))) %>%
    ungroup() %>%
    # Map to morphological type
    mutate(verb_type = mapping_logic(Q_Idx)) %>%
    mutate(Test = test_name)
}

# 2. MAPPING LOGICS (Same as your original logic)
map_t1 <- function(idx) {
  case_when(
    idx %in% c(1,4,5,9,11,12,22,23) ~ "Weak",
    idx %in% c(2,3,6,7,10,13,14,15,16,17,18,19,24,25) ~ "Strong",
    idx %in% c(20,21) ~ "Invariable",
    idx %in% c(8) ~ "Suppletive",
    TRUE ~ "Strong"
  )
}

map_t2 <- function(idx) {
  case_when(
    idx %in% c(8) ~ "Weak",
    idx %in% c(1,2,3,6,9,10,12,13,14,15,16,17) ~ "Strong",
    idx %in% c(5) ~ "Invariable",
    idx %in% c(4, 7, 11) ~ "Suppletive",
    TRUE ~ "Strong"
  )
}

# 3. RUN UPDATED PIPELINE
all_data_corrected <- bind_rows(
  process_test_corrected(test1_cleaned, "Test 1", map_t1),
  process_test_corrected(test2_cleaned, "Test 2", map_t2)
)

# 4. GENERATE TRUE ERROR TABLE
# This will count ONLY actual errors (excluding "correct" responses)
table_data_output <- all_data_corrected %>%
  filter(Status != "correct") %>% # Filters out the real correct answers
  group_by(verb_type, Test) %>%
  summarise(Error_Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Test, values_from = Error_Count, values_fill = 0) %>%
  mutate(verb_type = factor(verb_type, levels = c("Weak", "Strong", "Suppletive", "Invariable"))) %>%
  arrange(verb_type)

cat("\n--- TRUE MORPHOLOGICAL ERROR DISTRIBUTION TABLE ---\n")
print(table_data_output)

# 5. PLOT TRUE ERROR DATA
plot_data_corrected <- all_data_corrected %>%
  filter(Status != "correct")

ggplot(plot_data_corrected, aes(x = factor(verb_type, levels=c("Weak", "Strong", "Suppletive", "Invariable")), 
                                fill = Test)) +
  geom_bar(position = "dodge", color = "black", size = 0.3) +
  theme_minimal() +
  labs(title = "True Error Frequency by Morphological Verb Type",
       x = "Morphological Verb Type", y = "Actual Error Count")


# ==============================================================================
# INDEPENDENT CHI-SQUARE FOR TEST 1 VS TEST 2 
# ==============================================================================

# Adjusted matrix: 3 rows (Error Types) x 2 columns (Test 1 and Test 2)
global_error_matrix <- matrix(
  c(15, 81,    # Auxiliary Error
    77, 70,    # Incorrect Participle
    22, 18),   # Overgeneralized -ed
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Auxiliary", "Participle", "Overgeneralized"), c("Test1", "Test2"))
)

cat("\n==================================================\n")
cat("   VALID INDEPENDENT CHI-SQUARE (TEST 1 VS TEST 2)")
cat("\n==================================================\n")

# Run the Chi-Square test and store the result
chi2_result <- chisq.test(global_error_matrix)
print(chi2_result)

# ==============================================================================
# CRAMER'S V (EFFECT SIZE) CALCULATION
# ==============================================================================

chi2_stat <- chi2_result$statistic          # Chi-squared statistic value
n_total   <- sum(global_error_matrix)        # Total sample size (N)
min_dim   <- min(nrow(global_error_matrix) - 1, ncol(global_error_matrix) - 1) # min(r-1, c-1)

# Cramer's V Formula
cramers_v <- sqrt(chi2_stat / (n_total * min_dim))

cat("\n==================================================\n")
cat("   CRAMER'S V (EFFECT SIZE)")
cat("\n==================================================\n")
cat("Cramer's V:", round(cramers_v, 4), "\n")

# Interpretation based on df = 1 (min(r-1, c-1) = 1)
cat("Interpretation (df = 1): ")
if (cramers_v < 0.1) {
  cat("Negligible / Very small effect\n")
} else if (cramers_v < 0.3) {
  cat("Small effect\n")
} else if (cramers_v < 0.5) {
  cat("Medium effect\n")
} else {
  cat("Large effect\n")
}

# ==============================================================================
# STANDARDIZED PEARSON RESIDUALS
# ==============================================================================

cat("\n==================================================\n")
cat("   STANDARDIZED PEARSON RESIDUALS")
cat("\n==================================================\n")

# Extract standardized residuals from the chi2_result object
std_residuals <- chi2_result$stdres
print(round(std_residuals, 4))

cat("\nNote: Residuals > 2 or < -2 indicate statistically significant deviations\n")
cat("from the expected frequencies (alpha = 0.05).\n")


# install.packages("vcd") # Commented out to prevent reinstalling every time

# Load necessary library for mosaic plots
library(vcd)

# 1. Define the matrix with named dimensions to get the correct axis titles
global_error_matrix <- matrix(
  c(15, 81,    # Auxiliary Error
    77, 70,    # Incorrect Participle
    22, 18),   # Overgeneralized -ed
  nrow = 3, byrow = TRUE,
  dimnames = list(
    Error_Type = c("Auxiliary", "Participle", "Overgeneralized"), # Named 'Error_Type' instead of just a list
    Test       = c("Test 1", "Test 2")                           # Named 'Test' instead of just a list
  )
)

# 2. Create the Mosaic Plot
mosaic(global_error_matrix, 
       shade = TRUE, 
       legend = TRUE,
       main = "Mosaic Plot of Error Distribution (Test 1 vs Test 2)",
       sub = "Chi-square Analysis of Linguistic Error Categories")
# ============================================================
#  Appendix D: Multiple Correspondence Analysis – Irregular Verb Errors
#
#  Your research question:
#    Which learner profiles are associated with which error patterns?
#
#  Active variables (these construct the geometry — your error space):
#    Irregular_Verb_Type (6 levels), Error_Type (3 levels)
#
#  Illustrative / supplementary variables (projected post-hoc
#  onto your error space — your learner profile space):
#    Verb, Country, Native_Language, Language_Family,
#    Years_of_English_at_School
#
#  Rationale for your analysis:
#    By making the linguistic error variables active, you ensure
#    the map axes are optimised to separate error profiles. The
#    learner background categories are then projected into that
#    space, revealing which learner profiles gravitate toward
#    which error clusters — directly answering your research 
#    question. Reversing this (background active, errors 
#    illustrative) would optimise the map for learner separation
#    and risk burying the error-pattern variation you are 
#    interested in.
# ============================================================


# ── 0. Packages ──────────────────────────────────────────────
pkgs <- c("FactoMineR", "factoextra", "ggplot2", "ggrepel", "dplyr", "readr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(FactoMineR) 
library(factoextra)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)

set.seed(42)


# ── 1. Load data ─────────────────────────────────────────────
path <- "irregular_verbs_mca_data_romance_germanic.csv" # replace this fake path with your the path to you data file on your computer

df_raw <- read.table(path, header = TRUE, sep = ";", skip = 2, check.names = FALSE)

# Standardise column names
names(df_raw) <- gsub("\\s+", "_", trimws(names(df_raw)))
names(df_raw) <- gsub("[^A-Za-z0-9_]", "", names(df_raw))

cat("Columns found:\n"); print(names(df_raw))
cat("\nRows:", nrow(df_raw), "\n")


# ── 2. Select & rename relevant columns ──────────────────────
col_map <- c(
  # Active first
  Irregular_Verb_Type = grep("Irregular",         names(df_raw), value = TRUE)[1],
  Error_Type          = grep("Error_Type",         names(df_raw), value = TRUE)[1],
  # Illustrative second
  Verb                = grep("^Verb$",             names(df_raw), value = TRUE)[1],
  Country             = grep("Country",            names(df_raw), value = TRUE)[1],
  Native_Language     = grep("Native",             names(df_raw), value = TRUE)[1],
  Language_Family     = grep("Language_Family",    names(df_raw), value = TRUE)[1],
  Years_English       = grep("Years",              names(df_raw), value = TRUE)[1]
)

cat("\nColumn mapping used:\n"); print(col_map)

df <- df_raw %>%
  select(all_of(col_map)) %>%
  setNames(names(col_map)) %>%
  mutate(across(everything(), ~ factor(trimws(as.character(.)))))

# Drop rows with any NA in active columns
active_vars <- c("Irregular_Verb_Type", "Error_Type")
df <- df %>% filter(if_all(all_of(active_vars), ~ !is.na(.)))

cat("\nFinal dataset dimensions:", nrow(df), "rows x", ncol(df), "cols\n")
cat("\nLevel counts per variable:\n")
for (v in names(df)) cat(" ", v, ":", nlevels(df[[v]]), "levels –",
                         paste(levels(df[[v]]), collapse = ", "), "\n")


# ── 3. Build the MCA input data-frame ────────────────────────
suppl_vars <- c("Verb", "Country", "Native_Language", "Language_Family", "Years_English")

df_mca    <- df %>% select(all_of(active_vars), all_of(suppl_vars))
n_active  <- length(active_vars)
n_suppl   <- length(suppl_vars)
suppl_idx <- (n_active + 1):(n_active + n_suppl)

cat("\nActive (geometry)    :", paste(active_vars, collapse = ", "), "\n")
cat("Illustrative (projected):", paste(suppl_vars, collapse = ", "), "\n")


# ── 4. Run MCA ───────────────────────────────────────────────
set.seed(42)
mca <- MCA(
  df_mca,
  ncp       = 5,
  quali.sup = suppl_idx,
  graph     = FALSE
)


# ── 5. Diagnostics ───────────────────────────────────────────
cat("\n\n========== Eigenvalues / Variance Explained ==========\n")
print(round(mca$eig, 4))

cat("\n========== Active category coordinates (Dim 1–2) ==========\n")
print(round(mca$var$coord[, 1:2], 4))

cat("\n========== Active category contributions (%) ==========\n")
print(round(mca$var$contrib[, 1:2], 3))

cat("\n========== Active category cos² ==========\n")
print(round(mca$var$cos2[, 1:2], 3))

cat("\n========== Illustrative category coordinates (Dim 1–2) ==========\n")
print(round(mca$quali.sup$coord[, 1:2], 4))

cat("\n========== Raw rownames of quali.sup coords (used for variable matching) ==========\n")
print(rownames(mca$quali.sup$coord))


# ── 6. Build tidy coordinate frames ──────────────────────────
pct   <- round(mca$eig[1:2, 2], 1)
x_lab <- paste0("Dimension 1 (", pct[1], "% of inertia)")
y_lab <- paste0("Dimension 2 (", pct[2], "% of inertia)")

## Active categories
act_df <- as.data.frame(mca$var$coord[, 1:2]) %>%
  setNames(c("Dim1", "Dim2")) %>%
  mutate(
    label    = rownames(mca$var$coord),
    cos2_sum = rowSums(mca$var$cos2[, 1:2]),
    contrib  = rowSums(mca$var$contrib[, 1:2])
  )

cat_to_var <- unlist(lapply(active_vars, function(v) {
  setNames(rep(v, nlevels(df_mca[[v]])), levels(df_mca[[v]]))
}))
act_df$variable <- cat_to_var[act_df$label]

## Illustrative categories
# FactoMineR prefixes supplementary category rownames with "VariableName_level"
# (e.g. "Country_Brazil"). We recover the variable name by matching the longest
# suppl_var name that the rowname starts with, which is robust to any
# level-name collisions and avoids NA from a direct lookup.
ill_df <- as.data.frame(mca$quali.sup$coord[, 1:2]) %>%
  setNames(c("Dim1", "Dim2")) %>%
  mutate(
    label    = rownames(mca$quali.sup$coord),
    variable = sapply(label, function(lbl) {
      # Try prefix match "VarName_" first (FactoMineR default)
      matched <- suppl_vars[startsWith(lbl, paste0(suppl_vars, "_"))]
      if (length(matched) > 0) {
        # Pick the longest match to handle names like "Language_Family"
        return(matched[which.max(nchar(matched))])
      }
      # Fallback: check if the label equals a known level across suppl vars
      for (v in suppl_vars) {
        if (lbl %in% levels(df_mca[[v]])) return(v)
      }
      return(NA_character_)   # should not occur; flagged for inspection
    })
  ) %>%
  filter(!is.na(variable))   # safety: drop any unmatched rows rather than show NA in legend

# Diagnostic: print the label → variable mapping to verify no NA slipped through
cat("\nIllustrative label -> variable mapping:\n")
print(ill_df %>% select(label, variable) %>% arrange(variable))

## Individual (row) scores
ind_df <- as.data.frame(mca$ind$coord[, 1:2]) %>%
  setNames(c("Dim1", "Dim2"))


# ── 7. Colour palettes ────────────────────────────────────────
# Active: linguistic error variables — warm tones
active_palette <- c(
  "Irregular_Verb_Type" = "#762a83",   # purple
  "Error_Type"          = "#d6604d"    # red-orange
)

# Illustrative: learner background variables — cool tones
suppl_palette <- c(
  "Verb"             = "#1b7837",   # green
  "Country"          = "#2166ac",   # dark blue
  "Native_Language"  = "#4393c3",   # mid blue
  "Language_Family"  = "#e08214",   # orange
  "Years_English"    = "#b35806"    # amber
)


# ── 8. Plot 1 – Active (error) categories only ───────────────
p1 <- ggplot(act_df, aes(x = Dim1, y = Dim2, colour = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_point(aes(size = cos2_sum), alpha = 0.88) +
  geom_text_repel(
    aes(label = label),
    size          = 3.4,
    max.overlaps  = 20,
    segment.color = "grey60",
    segment.size  = 0.3,
    fontface      = "bold",
    box.padding   = 0.5,
    point.padding = 0.3
  ) +
  scale_colour_manual(
    values       = active_palette,
    name         = "Active Variable",
    na.translate = FALSE,
    labels       = c("Irregular_Verb_Type" = "Irregular Verb Type",
                     "Error_Type"          = "Error Type")
  ) +
  scale_size_continuous(range = c(2.5, 7), name = "Cos² (Dim1+2)") +
  labs(
    title    = "MCA – Active Category Map (Error Space)",
    subtitle = "Axes optimised to separate error profiles · Point size ∝ cos²",
    x        = x_lab,
    y        = y_lab,
    caption  = "Active variables: Irregular Verb Type (6 levels) · Error Type (3 levels)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("mca_plot1_active_error_space.pdf", p1, width = 10, height = 7)
ggsave("mca_plot1_active_error_space.png", p1, width = 10, height = 7, dpi = 300)
cat("\nPlot 1 saved.\n")


# ── 9. Plot 2 – Learner background projected onto error space ─
#   This is the key plot for the research question:
#   "Which learner profiles cluster near which error types?"
p2 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  
  # Error categories greyed in background (reference geometry)
  geom_point(data = act_df,
             aes(x = Dim1, y = Dim2),
             colour = "grey65", size = 2.5, shape = 16, alpha = 0.55) +
  geom_text_repel(data = act_df,
                  aes(x = Dim1, y = Dim2, label = label),
                  colour        = "grey45",
                  size          = 2.8,
                  fontface      = "bold",
                  max.overlaps  = 15,
                  segment.color = "grey80",
                  segment.size  = 0.2,
                  box.padding   = 0.4) +
  
  # Learner background categories foregrounded as triangles
  geom_point(data = ill_df,
             aes(x = Dim1, y = Dim2, colour = variable),
             shape = 17, size = 4, alpha = 0.9) +
  geom_text_repel(data = ill_df,
                  aes(x = Dim1, y = Dim2, label = label, colour = variable),
                  size          = 3.4,
                  fontface      = "italic",
                  max.overlaps  = 20,
                  segment.color = "grey60",
                  segment.size  = 0.3,
                  box.padding   = 0.5,
                  point.padding = 0.3) +
  
  scale_colour_manual(values = suppl_palette, name = "Learner Background",
                      na.translate = FALSE) +
  labs(
    title    = "MCA – Learner Profiles Projected onto Error Space",
    subtitle = "▲ Learner background (illustrative)  ●  Error categories (active, greyed)",
    x        = x_lab,
    y        = y_lab,
    caption  = paste0(
      "Proximity of a learner-background triangle to an error circle indicates association.\n",
      "Illustrative: Verb · Country · Native Language · Language Family · Years of English"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("mca_plot2_learner_profiles_on_error_space.pdf", p2, width = 11, height = 7.5)
ggsave("mca_plot2_learner_profiles_on_error_space.png", p2, width = 11, height = 7.5, dpi = 300)
cat("Plot 2 saved.\n")


# ── 10. Plot 3 – Full combined map ───────────────────────────
p3 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  
  # Individual scores (open circles)
  geom_point(data = ind_df,
             aes(x = Dim1, y = Dim2),
             colour = "grey85", size = 1.8, shape = 1) +
  
  # Active error categories – filled circles, sized by cos²
  geom_point(data = act_df,
             aes(x = Dim1, y = Dim2, colour = variable, size = cos2_sum),
             shape = 16, alpha = 0.92) +
  geom_text_repel(data = act_df,
                  aes(x = Dim1, y = Dim2, label = label, colour = variable),
                  size          = 3.3,
                  fontface      = "bold",
                  max.overlaps  = 25,
                  segment.color = "grey60",
                  segment.size  = 0.3,
                  box.padding   = 0.5,
                  point.padding = 0.3) +
  
  # Illustrative learner-background categories – filled triangles
  geom_point(data = ill_df,
             aes(x = Dim1, y = Dim2, fill = variable),
             colour = "grey25", shape = 24, size = 3.8, alpha = 0.88) +
  geom_text_repel(data = ill_df,
                  aes(x = Dim1, y = Dim2, label = label, colour = variable),
                  size          = 3.0,
                  fontface      = "italic",
                  max.overlaps  = 25,
                  segment.color = "grey70",
                  segment.size  = 0.25,
                  box.padding   = 0.4,
                  point.padding = 0.3) +
  
  scale_colour_manual(
    values       = c(active_palette, suppl_palette),
    name         = "Variable",
    na.translate = FALSE     # suppress any residual NA level from the legend
  ) +
  scale_fill_manual(values = suppl_palette, guide = "none") +
  scale_size_continuous(range = c(2.5, 7), name = "Cos² (active)") +
  labs(
    title    = "MCA – Full Symmetric Map",
    subtitle = "● Active error categories  ▲ Illustrative learner-background categories  ○ Individuals",
    x        = x_lab,
    y        = y_lab,
    caption  = paste0(
      "Active (error space): Irregular Verb Type · Error Type\n",
      "Illustrative (learner profiles): Verb · Country · Native Language · Language Family · Years of English"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("mca_plot3_full_map.pdf", p3, width = 12, height = 8)
ggsave("mca_plot3_full_map.png", p3, width = 12, height = 8, dpi = 300)
cat("Plot 3 saved.\n")


# ── 11. Export numerical results ─────────────────────────────
write_csv(
  act_df %>% select(label, variable, Dim1, Dim2, cos2_sum, contrib),
  "mca_active_coordinates.csv"
)
write_csv(
  ill_df %>% select(label, variable, Dim1, Dim2),
  "mca_illustrative_coordinates.csv"
)
write_csv(
  as.data.frame(mca$eig) %>%
    mutate(dim = paste0("Dim", row_number())) %>%
    select(dim, everything()),
  "mca_eigenvalues.csv"
)
cat("CSV files saved.\n")
cat("\nAll done! Check your working directory for the output files.\n")

print(p1)
print(p2)
print(p3)

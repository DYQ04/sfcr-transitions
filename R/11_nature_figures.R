library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(scales)
library(gridExtra)
library(svglite)
library(ragg)

root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
base <- file.path(root, "outputs/reruns/six_country")
out <- file.path(base, "Nature_R_visualization")
main_dir <- file.path(out, "main")
supp_dir <- file.path(out, "supplement")
source_dir <- file.path(out, "source_data")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

pal <- c(
  ink = "#202124", muted = "#6B7280", grid = "#E5E7EB",
  blue = "#2F6C99", blue_dark = "#173B57", blue_light = "#BFD4E6",
  red = "#A33A2B", red_dark = "#70241F", red_light = "#E8B8A8",
  gold = "#B88216", gold_light = "#E9D39A", teal = "#2A7F78",
  plum = "#604A75", grey = "#C8CCD2"
)

cohort_order <- c("CHARLS", "HRS", "KLoSA", "MHAS", "SHARE", "ELSA")
transition_order <- c(
  "Independent -> Disabled",
  "Independent -> Dead",
  "Disabled -> Independent",
  "Disabled -> Dead"
)
sfcr_order <- c("Neither", "Child only", "Partner only", "Partner + child")
sfcr_map <- c(
  neither = "Neither",
  child_only = "Child only",
  partner_only = "Partner only",
  partner_child = "Partner + child"
)
sfcr_cols <- c(
  "Neither" = "#C9C4D4",
  "Child only" = pal["gold"],
  "Partner only" = pal["blue"],
  "Partner + child" = pal["red_dark"]
)
state_cols <- c(
  "Independent" = pal["blue"],
  "Disabled" = pal["gold"],
  "Dead" = pal["red_dark"]
)
transition_map <- c(
  independent_to_disabled = "Independent -> Disabled",
  independent_to_dead = "Independent -> Dead",
  disabled_to_independent = "Disabled -> Independent",
  disabled_to_dead = "Disabled -> Dead"
)

theme_nature <- function(base_size = 6.4) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.32, colour = pal["ink"]),
      axis.ticks = element_line(linewidth = 0.28, colour = pal["ink"]),
      axis.text = element_text(size = base_size - 0.45, colour = pal["ink"]),
      axis.title = element_text(size = base_size, colour = pal["ink"]),
      plot.title = element_text(size = base_size + 0.6, face = "bold", colour = pal["ink"], margin = margin(b = 3)),
      plot.subtitle = element_text(size = base_size - 0.2, colour = pal["muted"]),
      legend.title = element_text(size = base_size - 0.3),
      legend.text = element_text(size = base_size - 0.7),
      legend.key.height = unit(3.5, "mm"),
      legend.key.width = unit(4.2, "mm"),
      strip.text = element_text(size = base_size - 0.2, face = "bold"),
      panel.grid.major = element_line(linewidth = 0.18, colour = pal["grid"]),
      panel.grid.minor = element_blank(),
      plot.margin = margin(3, 4, 3, 4),
      plot.tag = element_text(size = base_size + 1.2, face = "bold", colour = pal["ink"])
    )
}
theme_set(theme_nature())

save_pub <- function(plot, file, width_mm = 183, height_mm = 135, dpi = 600) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  svglite::svglite(paste0(file, ".svg"), width = w, height = h)
  print(plot)
  dev.off()
  grDevices::cairo_pdf(paste0(file, ".pdf"), width = w, height = h, family = "Arial")
  print(plot)
  dev.off()
  ragg::agg_tiff(paste0(file, ".tiff"), width = w, height = h, units = "in", res = dpi, compression = "lzw")
  print(plot)
  dev.off()
  ragg::agg_png(paste0(file, ".png"), width = w, height = h, units = "in", res = 300)
  print(plot)
  dev.off()
}

read_src <- function(name) {
  read_csv(file.path(base, "figures_submission_ready/source_data", name), show_col_types = FALSE)
}
read_main <- function(name) {
  read_csv(file.path(base, "tables/main", name), show_col_types = FALSE)
}
read_supp <- function(name) {
  read_csv(file.path(base, "tables/supplement", name), show_col_types = FALSE)
}
read_res <- function(name) {
  read_csv(file.path(base, "results_summary", name), show_col_types = FALSE)
}
write_source <- function(df, name) {
  write_csv(df, file.path(source_dir, paste0(name, ".csv")))
}

d <- list(
  f1a = read_src("Figure_1A_cohort_contribution.csv"),
  f1b = read_src("Figure_1B_wave_timeline.csv"),
  f1d = read_src("Figure_1D_exclusions.csv"),
  f2a = read_src("Figure_2A_sfcr_distribution.csv"),
  f2b = read_src("Figure_2B_observed_transitions.csv"),
  f2c = read_src("Figure_2C_adl_prevalence.csv"),
  f2d = read_src("Figure_2D_interval_length.csv"),
  f3a = read_src("Figure_3A_transition_effects.csv"),
  f3b = read_src("Figure_3B_standardized_probabilities.csv"),
  f3c = read_src("Figure_3C_absolute_risk_difference.csv"),
  f3d = read_src("Figure_3D_partner_child_decomposition.csv"),
  f4a = read_src("Figure_4A_meta_summary.csv"),
  f4bc = read_src("Figure_4BC_maps.csv"),
  f5a = read_src("Figure_5A_subgroup_benefit.csv"),
  f5b = read_src("Figure_5B_first_onset_landmark.csv"),
  f5c = read_src("Figure_5C_proxy_sensitivity.csv"),
  f5d = read_src("Figure_5D_strict_adl.csv"),
  baseline = read_main("table_main_1_baseline_characteristics.csv"),
  tprob = read_main("table_main_2_transition_probabilities.csv"),
  main_models = read_main("table_main_3_main_models.csv"),
  heter = read_main("table_main_4_heterogeneity_and_sensitivity_summary.csv"),
  st1 = read_supp("st1_cohort_overview.csv"),
  st6 = read_supp("st6_missingness_by_cohort_wave.csv"),
  st8 = read_supp("st8_cohort_country_descriptives.csv"),
  st10 = read_supp("st10_meta_analysis_results.csv"),
  st12 = read_supp("st12_full_sensitivity_results.csv"),
  effects = read_res("cohort_country_effects.csv"),
  subgroup = read_res("subgroup_summary.csv")
)

panel_rows <- tibble(
  figure = character(), panel = character(), title = character(),
  meaning = character(), source_data = character()
)
add_panels <- function(rows) {
  panel_rows <<- bind_rows(panel_rows, rows)
}

heat_plot <- function(df, x, y, fill, title, fill_label, low = "#F6F7F9", high = pal["blue_dark"], fmt = percent_format(accuracy = 0.1)) {
  ggplot(df, aes({{ x }}, {{ y }}, fill = {{ fill }})) +
    geom_tile(colour = "white", linewidth = 0.25) +
    geom_text(aes(label = fmt({{ fill }})), size = 2.1, colour = pal["ink"]) +
    scale_fill_gradient(low = low, high = high, name = fill_label) +
    labs(title = title, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

platform_sources <- function() {
  prof <- d$f1a %>%
    left_join(d$st1, by = "cohort") %>%
    left_join(d$baseline, by = "cohort") %>%
    mutate(across(c(female_pct, partnered_pct, living_child_pct, adl_disabled_pct), ~ .x * 100))
  write_source(prof, "nature_Figure1A_C_profile")
  write_source(d$f1b, "nature_Figure1B_wave_timeline")
  write_source(d$f1d, "nature_Figure1E_exclusions")
}
fig2 <- function() {
  tprob_classified <- d$tprob %>% filter(!is.na(sfcr_cat4))
  tprob_unclassified <- d$tprob %>%
    filter(is.na(sfcr_cat4)) %>%
    mutate(note = "SFCR category could not be assigned because partner and/or living-child information was missing at the origin wave.")

  write_source(d$f2a, "nature_Figure2A_sfcr_distribution")
  write_source(d$f2b, "nature_Figure2B_observed_transitions")
  write_source(tprob_classified, "nature_Figure2C_transition_profile_by_sfcr")
  write_source(tprob_unclassified, "nature_Figure2C_sfcr_unclassified_intervals")
  write_source(d$f2c, "nature_Figure2D_adl_item_prevalence")
  write_source(d$f2d, "nature_Figure2E_interval_length")
  write_source(d$f4bc, "nature_Figure2F_country_post_disability_landscape")

  p_a <- d$f2a %>%
    mutate(cohort = factor(cohort, levels = cohort_order), sfcr_cat4 = factor(sfcr_cat4, levels = sfcr_order)) %>%
    ggplot(aes(prop, cohort, fill = sfcr_cat4)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.2) +
    scale_x_continuous(labels = percent_format()) +
    scale_fill_manual(values = sfcr_cols) +
    labs(title = "SFCR composition", x = "Proportion", y = NULL, fill = NULL)

  trans_mat <- d$f2b %>% mutate(state_label = str_to_title(state_label), next_state_label = str_to_title(next_state_label))
  p_b <- heat_plot(trans_mat, next_state_label, state_label, prop, "Observed transition matrix", "Share", high = pal["blue_dark"])

  tprof <- tprob_classified %>%
    mutate(sfcr_cat4 = recode(sfcr_cat4, !!!sfcr_map), sfcr_cat4 = factor(sfcr_cat4, levels = sfcr_order)) %>%
    pivot_longer(starts_with("next_"), names_to = "next_state", values_to = "prob") %>%
    mutate(next_state = str_to_title(str_remove(next_state, "next_")))
  p_c <- ggplot(tprof, aes(sfcr_cat4, prob, fill = next_state)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62) +
    facet_wrap(~ state_label, nrow = 1) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = state_cols) +
    labs(title = "Transition profile by classifiable SFCR group", x = NULL, y = "Probability", fill = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  adl <- d$f2c %>% mutate(cohort = factor(cohort, levels = cohort_order))
  p_d <- heat_plot(adl, `ADL item`, cohort, prevalence, "Item-level ADL burden", "Prevalence", high = pal["red_dark"])

  p_e <- d$f2d %>%
    mutate(cohort = factor(cohort, levels = cohort_order)) %>%
    ggplot(aes(interval_length, cohort)) +
    geom_violin(fill = pal["blue_light"], colour = NA, alpha = 0.75) +
    geom_boxplot(width = 0.13, fill = "white", outlier.shape = NA, linewidth = 0.25) +
    labs(title = "Interval length", x = "Years", y = NULL)

  land <- d$f4bc %>% select(cohort, cohort_country, transition, prob) %>% pivot_wider(names_from = transition, values_from = prob)
  p_f <- ggplot(land, aes(`Disabled -> Independent`, `Disabled -> Dead`, colour = cohort, label = cohort_country)) +
    geom_point(size = 1.7, alpha = 0.88) +
    ggrepel::geom_text_repel(data = land %>% arrange(desc(`Disabled -> Dead`)) %>% slice_head(n = 7), size = 1.8, max.overlaps = 15, min.segment.length = 0) +
    scale_colour_manual(values = c(CHARLS = pal["red_dark"], HRS = pal["blue_dark"], KLoSA = pal["teal"], MHAS = pal["gold"], SHARE = pal["plum"], ELSA = pal["muted"])) +
    scale_x_continuous(labels = percent_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Country post-disability landscape", x = "Recovery probability", y = "Death probability", colour = NULL)

  fig <- (p_a | p_b) / (p_c) / (p_d | p_e | p_f) +
    plot_layout(heights = c(0.92, 0.9, 1.05), guides = "collect") +
    plot_annotation(title = "Figure 2. Exposure landscape, observed state transitions, and ADL burden", tag_levels = "A") &
    theme(legend.position = "bottom")
  save_pub(fig, file.path(main_dir, "Figure_2_Nature_R_transition_landscape"), 183, 150)

  add_panels(tibble(
    figure = "Figure 2", panel = LETTERS[1:6],
    title = c("SFCR composition", "Observed transition matrix", "Transition profile by SFCR group", "Item-level ADL burden", "Interval length", "Country post-disability landscape"),
    meaning = c(
      "Shows the distribution of partner-child structural reserve categories across cohorts.",
      "Shows the crude state-transition matrix before regression modeling.",
      "Shows observed next-state probabilities stratified by classifiable SFCR category and origin state; intervals with missing partner or living-child information are excluded from this panel.",
      "Shows five harmonized ADL item prevalences, documenting item-level outcome reconstruction.",
      "Shows adjacent-wave follow-up time distributions across cohorts.",
      "Places cohort-country units by recovery and mortality probabilities after disability."
    ),
    source_data = c("nature_Figure2A_sfcr_distribution.csv", "nature_Figure2B_observed_transitions.csv", "nature_Figure2C_transition_profile_by_sfcr.csv", "nature_Figure2D_adl_item_prevalence.csv", "nature_Figure2E_interval_length.csv", "nature_Figure2F_country_post_disability_landscape.csv")
  ))
}

fig3 <- function() {
  write_source(d$f3a, "nature_Figure3A_pooled_effects")
  write_source(d$f3b, "nature_Figure3B_C_standardized_probabilities")
  write_source(d$f3c, "nature_Figure3D_absolute_risk_difference")
  write_source(d$f3d, "nature_Figure3E_decomposition")

  eff <- d$f3a %>% mutate(transition = factor(transition, levels = transition_order), contrast = factor(contrast, levels = c("Child only", "Partner only", "Partner + child")))
  p_a <- ggplot(eff, aes(rrr, transition, colour = contrast)) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = pal["muted"]) +
    geom_errorbarh(aes(xmin = rrr_lo, xmax = rrr_hi), height = 0.14, position = position_dodge(width = 0.58), linewidth = 0.42) +
    geom_point(position = position_dodge(width = 0.58), size = 1.45) +
    scale_colour_manual(values = sfcr_cols[c("Child only", "Partner only", "Partner + child")]) +
    labs(title = "Pooled transition-specific associations", x = "Relative risk ratio", y = NULL, colour = NULL)

  std <- d$f3b %>% mutate(next_state = str_to_title(next_state), sfcr_cat4 = factor(sfcr_cat4, levels = sfcr_order))
  p_b <- std %>% filter(origin_state == "Initially independent") %>%
    heat_plot(next_state, sfcr_cat4, prob, "Standardized probabilities: independent origin", "Probability", high = pal["blue_dark"])
  p_c <- std %>% filter(origin_state == "Initially disabled") %>%
    heat_plot(next_state, sfcr_cat4, prob, "Standardized probabilities: disabled origin", "Probability", high = pal["red_dark"])

  ard <- d$f3c %>% mutate(label = paste(str_remove(origin_state, "Initially "), "->", next_state), direction = if_else(risk_difference >= 0, "Higher", "Lower"))
  p_d <- ggplot(ard, aes(risk_difference, reorder(label, risk_difference), fill = direction)) +
    geom_col(width = 0.66) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = pal["muted"]) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(Higher = pal["blue"], Lower = pal["red_dark"])) +
    labs(title = "Absolute difference: partner + child vs neither", x = "Risk difference", y = NULL, fill = NULL)

  p_e <- ggplot(d$f3d, aes(rrr, factor(transition, levels = transition_order), colour = contrast, group = contrast)) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = pal["muted"]) +
    geom_errorbarh(aes(xmin = rrr_lo, xmax = rrr_hi), height = 0.12, linewidth = 0.32) +
    geom_line(linewidth = 0.42, alpha = 0.72) +
    geom_point(size = 1.35) +
    scale_colour_manual(values = sfcr_cols[c("Child only", "Partner only", "Partner + child")]) +
    labs(title = "Partner-child decomposition", x = "RRR", y = NULL, colour = NULL)

  shift <- d$f3c %>% mutate(label = paste(str_remove(origin_state, "Initially "), "|", next_state))
  p_f <- ggplot(shift) +
    geom_segment(aes(x = Neither * 100, xend = `Partner + child` * 100, y = label, yend = label), colour = pal["grey"], linewidth = 0.7) +
    geom_point(aes(Neither * 100, label), colour = "#9CA3AF", size = 1.35) +
    geom_point(aes(`Partner + child` * 100, label), colour = pal["red_dark"], size = 1.55) +
    labs(title = "Clinical probability shift", x = "Probability (%)", y = NULL)

  fig <- (p_a | (p_b / p_c)) / (p_d | p_e | p_f) +
    plot_layout(heights = c(1.3, 1), guides = "collect") +
    plot_annotation(title = "Figure 3. Pooled effects and clinically interpretable transition probabilities", tag_levels = "A") &
    theme(legend.position = "bottom")
  save_pub(fig, file.path(main_dir, "Figure_3_Nature_R_pooled_effects"), 183, 150)

  add_panels(tibble(
    figure = "Figure 3", panel = LETTERS[1:6],
    title = c("Pooled associations", "Independent-origin standardized probabilities", "Disabled-origin standardized probabilities", "Absolute risk difference", "Partner-child decomposition", "Clinical probability shift"),
    meaning = c(
      "Displays transition-specific relative risk ratios for SFCR categories versus neither.",
      "Translates model estimates into standardized next-state probabilities among initially independent participants.",
      "Translates model estimates into standardized next-state probabilities among initially disabled participants.",
      "Shows absolute probability differences for partner plus child versus neither.",
      "Separates child-only, partner-only, and combined partner-child contrasts across transitions.",
      "Shows the probability movement from neither to partner plus child for each origin-next-state pair."
    ),
    source_data = c("nature_Figure3A_pooled_effects.csv", "nature_Figure3B_C_standardized_probabilities.csv", "nature_Figure3B_C_standardized_probabilities.csv", "nature_Figure3D_absolute_risk_difference.csv", "nature_Figure3E_decomposition.csv", "nature_Figure3D_absolute_risk_difference.csv")
  ))
}

fig4 <- function() {
  write_source(d$f4a, "nature_Figure4A_B_meta_summary")
  write_source(d$effects, "nature_Figure4C_country_effects")
  write_source(d$f4bc, "nature_Figure4D_F_country_probabilities")

  meta <- d$f4a %>% mutate(transition = recode(transition, !!!transition_map), transition = factor(transition, levels = transition_order))
  p_a <- ggplot(meta, aes(or, transition)) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = pal["muted"]) +
    geom_errorbarh(aes(xmin = or_lo, xmax = or_hi), height = 0.12, colour = pal["blue_dark"], linewidth = 0.42) +
    geom_point(colour = pal["red_dark"], size = 1.55) +
    geom_text(aes(label = paste0("I虏 ", round(I2), "%")), x = 1.23, size = 2.0, hjust = 0) +
    coord_cartesian(xlim = c(0.72, 1.28)) +
    labs(title = "Two-stage meta-analysis", x = "Pooled OR per 1-unit SFCR score", y = NULL)

  p_b <- ggplot(meta, aes(I2, or, size = tau2 + 0.001, colour = transition, label = transition)) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3, colour = pal["muted"]) +
    geom_point(alpha = 0.9) +
    ggrepel::geom_text_repel(size = 1.8, max.overlaps = 20, min.segment.length = 0) +
    scale_size_continuous(range = c(1.8, 6.0), guide = "none") +
    scale_colour_manual(values = c(pal["gold"], pal["red_dark"], pal["blue_dark"], pal["teal"])) +
    labs(title = "Heterogeneity geometry", x = "I虏 (%)", y = "Pooled OR", colour = NULL)

  eff <- d$effects %>% mutate(transition = recode(transition, !!!transition_map), transition = factor(transition, levels = transition_order))
  p_c <- ggplot(eff, aes(transition, cohort_country, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    scale_fill_gradient2(low = pal["blue_dark"], mid = "#F7F7F7", high = pal["red_dark"], midpoint = 0, name = "Log estimate") +
    labs(title = "Country-specific model effects", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  profile <- d$f4bc %>% select(cohort_country, transition, prob)
  p_d <- ggplot(profile, aes(transition, cohort_country, fill = prob)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    scale_fill_gradient(low = "#F8EDEB", high = pal["red_dark"], labels = percent_format(), name = "Observed probability") +
    labs(title = "Observed post-disability profile", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  p_e <- d$f4bc %>% filter(transition == "Disabled -> Independent") %>% arrange(prob) %>%
    mutate(cohort_country = factor(cohort_country, levels = cohort_country)) %>%
    ggplot(aes(prob, cohort_country)) +
    geom_segment(aes(x = 0, xend = prob, yend = cohort_country), linewidth = 1.25, colour = pal["blue_light"]) +
    geom_point(size = 1.25, colour = pal["blue_dark"]) +
    scale_x_continuous(labels = percent_format()) +
    labs(title = "Recovery ranking", x = "Probability", y = NULL)

  p_f <- d$f4bc %>% filter(transition == "Disabled -> Dead") %>% arrange(prob) %>%
    mutate(cohort_country = factor(cohort_country, levels = cohort_country)) %>%
    ggplot(aes(prob, cohort_country)) +
    geom_segment(aes(x = 0, xend = prob, yend = cohort_country), linewidth = 1.25, colour = pal["red_light"]) +
    geom_point(size = 1.25, colour = pal["red_dark"]) +
    scale_x_continuous(labels = percent_format()) +
    labs(title = "Mortality ranking", x = "Probability", y = NULL)

  fig <- (p_a | p_b) / (p_c | p_d) / (p_e | p_f) +
    plot_layout(heights = c(0.9, 1.0, 1.15), guides = "collect") +
    plot_annotation(title = "Figure 4. Cross-national heterogeneity and post-disability outcome profiles", tag_levels = "A") &
    theme(legend.position = "bottom")
  save_pub(fig, file.path(main_dir, "Figure_4_Nature_R_heterogeneity"), 183, 150)

  add_panels(tibble(
    figure = "Figure 4", panel = LETTERS[1:6],
    title = c("Two-stage meta-analysis", "Heterogeneity geometry", "Country-specific model effects", "Observed post-disability profile", "Recovery ranking", "Mortality ranking"),
    meaning = c(
      "Summarizes pooled two-stage estimates and I虏 for each transition.",
      "Shows whether effect size, between-country heterogeneity, and tau虏 concentrate in specific transitions.",
      "Displays available country-specific coefficient estimates by transition.",
      "Displays observed recovery and mortality probabilities after disability across cohort-country units.",
      "Ranks cohort-country units by observed disabled-to-independent probability.",
      "Ranks cohort-country units by observed disabled-to-dead probability."
    ),
    source_data = c("nature_Figure4A_B_meta_summary.csv", "nature_Figure4A_B_meta_summary.csv", "nature_Figure4C_country_effects.csv", "nature_Figure4D_F_country_probabilities.csv", "nature_Figure4D_F_country_probabilities.csv", "nature_Figure4D_F_country_probabilities.csv")
  ))
}

fig5 <- function() {
  write_source(d$f5a, "nature_Figure5A_B_subgroup_benefit")
  write_source(d$f5b, "nature_Figure5C_landmark")
  write_source(d$f5c, "nature_Figure5D_proxy")
  write_source(d$f5d, "nature_Figure5E_strict_adl")
  write_source(d$st12, "nature_Figure5F_sensitivity_roadmap")

  sub <- d$f5a %>% mutate(cell = paste(sex_clean, age_group, sep = " | "))
  p_a <- sub %>% filter(metric == "lower_disabled") %>%
    heat_plot(wealth_tertile, cell, benefit, "Lower disability risk", "Absolute benefit", high = pal["blue_dark"])
  p_b <- sub %>% filter(metric == "lower_death") %>%
    heat_plot(wealth_tertile, cell, benefit, "Lower mortality risk", "Absolute benefit", high = pal["red_dark"])

  land <- d$f5b %>%
    mutate(sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
           sfcr_cat4 = factor(sfcr_cat4, levels = c("Partner + child", "Partner only", "Child only", "Neither")),
           next_state_label = str_to_title(next_state_label))
  p_c <- ggplot(land, aes(sfcr_cat4, prop, fill = next_state_label)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = state_cols) +
    labs(title = "First-onset landmark outcomes", x = NULL, y = "Proportion", fill = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  proxy <- d$f5c %>%
    mutate(sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
           transition = recode(transition, recovered = "Recovered", dead = "Dead"),
           sfcr_cat4 = factor(sfcr_cat4, levels = c("Partner + child", "Child only", "Neither", "Partner only")))
  p_d <- ggplot(proxy, aes(sfcr_cat4, prob, colour = proxy_group, linetype = transition, group = interaction(proxy_group, transition))) +
    geom_line(linewidth = 0.35) +
    geom_point(size = 1.2) +
    scale_y_continuous(labels = percent_format()) +
    scale_colour_manual(values = c(Self = pal["blue_dark"], Proxy = pal["red_dark"], Unknown = pal["plum"])) +
    labs(title = "Proxy sensitivity", x = NULL, y = "Probability", colour = NULL, linetype = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  strict <- d$f5d %>% pivot_wider(names_from = definition, values_from = prevalence) %>% mutate(cohort = factor(cohort, levels = cohort_order))
  p_e <- ggplot(strict, aes(y = cohort)) +
    geom_segment(aes(x = strict_adl * 100, xend = default_adl * 100, yend = cohort), colour = pal["grey"], linewidth = 0.6) +
    geom_point(aes(x = default_adl * 100), colour = pal["red_dark"], size = 1.35) +
    geom_point(aes(x = strict_adl * 100), colour = pal["blue_dark"], size = 1.35) +
    labs(title = "Outcome-definition sensitivity", x = "Prevalence (%)", y = NULL) +
    annotate("text", x = max(strict$default_adl * 100), y = 1.2, label = "Red: ADL>=1; blue: strict ADL", size = 1.9, colour = pal["muted"])

  table_text <- d$st12 %>%
    mutate(line = paste0(str_to_title(str_replace_all(analysis, "_", " ")), "\n", str_wrap(note, 38), "\nImpact: ", impact)) %>%
    pull(line) %>%
    paste(collapse = "\n\n")
  p_f <- ggplot() +
    annotate("text", x = 0, y = 1, label = table_text, hjust = 0, vjust = 1, size = 2.0, colour = pal["ink"], lineheight = 0.92) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(title = "Sensitivity roadmap") +
    theme_void(base_family = "Arial") +
    theme(plot.title = element_text(size = 7, face = "bold"))

  fig <- (p_a | p_b) / (p_c | p_d) / (p_e | p_f) +
    plot_layout(heights = c(1, 1, 0.9), guides = "collect") +
    plot_annotation(title = "Figure 5. Effect modification, landmark outcomes, and robustness checks", tag_levels = "A") &
    theme(legend.position = "bottom")
  save_pub(fig, file.path(main_dir, "Figure_5_Nature_R_robustness"), 183, 150)

  add_panels(tibble(
    figure = "Figure 5", panel = LETTERS[1:6],
    title = c("Lower disability risk", "Lower mortality risk", "First-onset landmark outcomes", "Proxy sensitivity", "Outcome-definition sensitivity", "Sensitivity roadmap"),
    meaning = c(
      "Shows subgroup absolute benefit for lower disability risk by sex, age, and wealth tertile.",
      "Shows subgroup absolute benefit for lower mortality risk by sex, age, and wealth tertile.",
      "Shows next-wave outcomes after first disability onset by SFCR group.",
      "Shows whether recovered and dead probabilities differ by proxy interview status and SFCR group.",
      "Compares default ADL>=1 disability prevalence with strict ADL definitions by cohort.",
      "Summarizes the robustness analyses available for manuscript interpretation."
    ),
    source_data = c("nature_Figure5A_B_subgroup_benefit.csv", "nature_Figure5A_B_subgroup_benefit.csv", "nature_Figure5C_landmark.csv", "nature_Figure5D_proxy.csv", "nature_Figure5E_strict_adl.csv", "nature_Figure5F_sensitivity_roadmap.csv")
  ))
}

supplement_figures <- function() {
  write_source(d$st6, "nature_SuppFig1_missingness")
  miss <- d$st6 %>% pivot_longer(-c(cohort, wave), names_to = "variable", values_to = "missing") %>% mutate(cohort_wave = paste0(cohort, "_w", wave))
  s1a <- ggplot(miss, aes(variable, cohort_wave, fill = missing)) +
    geom_tile(colour = "white", linewidth = 0.15) +
    scale_fill_gradient(low = "#F7F7F7", high = pal["red_dark"], labels = percent_format(), name = "Missing") +
    labs(title = "Missingness by cohort-wave", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  s1b <- d$st1 %>% mutate(cohort = factor(cohort, levels = cohort_order)) %>%
    ggplot(aes(ids, cohort)) + geom_col(fill = pal["blue"], width = 0.65) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = "Unique IDs by cohort", x = "IDs", y = NULL)
  s1c <- d$f1d %>% filter(include_reason != "Included") %>% mutate(cohort = factor(cohort, levels = cohort_order)) %>%
    ggplot(aes(N, cohort, fill = include_reason)) + geom_col(width = 0.65, colour = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = "Exclusion reasons", x = "Rows", y = NULL, fill = NULL)
  save_pub((s1a / (s1b | s1c)) + plot_annotation(title = "Supplementary Figure S1. Data completeness and inclusion audit", tag_levels = "A"), file.path(supp_dir, "Supplementary_Figure_S1_Nature_R_data_audit"), 183, 135)

  write_source(d$f2d, "nature_SuppFig2_interval_lengths")
  write_source(d$f2c, "nature_SuppFig2_adl_items")
  write_source(d$f4bc, "nature_SuppFig2_country_landscape")
  s2a <- d$f2d %>% mutate(cohort = factor(cohort, levels = cohort_order)) %>%
    ggplot(aes(interval_length, cohort)) + geom_boxplot(fill = pal["blue_light"], width = 0.35, outlier.size = 0.2) +
    labs(title = "Interval length distribution", x = "Years", y = NULL)
  s2b <- heat_plot(d$f2c %>% mutate(cohort = factor(cohort, levels = cohort_order)), `ADL item`, cohort, prevalence, "ADL item prevalence", "Prevalence", high = pal["gold"])
  land <- d$f4bc %>% select(cohort_country, transition, prob) %>% pivot_wider(names_from = transition, values_from = prob)
  s2c <- ggplot(land, aes(`Disabled -> Independent`, `Disabled -> Dead`, label = cohort_country)) +
    geom_point(colour = pal["red_dark"], size = 1.35) +
    ggrepel::geom_text_repel(size = 1.7, max.overlaps = 12, min.segment.length = 0) +
    scale_x_continuous(labels = percent_format()) + scale_y_continuous(labels = percent_format()) +
    labs(title = "Recovery versus mortality", x = "Recovery probability", y = "Death probability")
  save_pub((s2a | s2b) / s2c + plot_annotation(title = "Supplementary Figure S2. Descriptive transition atlas", tag_levels = "A"), file.path(supp_dir, "Supplementary_Figure_S2_Nature_R_descriptive_atlas"), 183, 135)

  write_source(d$main_models, "nature_SuppFig3_full_model_coefficients")
  mm <- d$main_models %>%
    filter(str_detect(term, "sfcr_cat4")) %>%
    mutate(
      transition = case_when(
        origin_state == "independent" & y.level == "disabled" ~ "Independent -> Disabled",
        origin_state == "independent" & y.level == "dead" ~ "Independent -> Dead",
        origin_state == "disabled" & y.level == "dead" ~ "Disabled -> Dead",
        TRUE ~ "Disabled -> Independent"
      ),
      rrr = exp(estimate), rrr_lo = exp(conf.low), rrr_hi = exp(conf.high)
    )
  s3a <- ggplot(mm, aes(rrr, factor(transition, levels = transition_order), colour = term)) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = rrr_lo, xmax = rrr_hi), height = 0.12, linewidth = 0.32, position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5), size = 1.1) +
    guides(colour = "none") +
    labs(title = "Full SFCR model contrasts", x = "RRR", y = NULL)
  s3b <- d$f3b %>% filter(origin_state == "Initially independent") %>%
    heat_plot(next_state, sfcr_cat4, prob, "Independent origin", "Probability", high = pal["blue_dark"])
  s3c <- d$f3b %>% filter(origin_state == "Initially disabled") %>%
    heat_plot(next_state, sfcr_cat4, prob, "Disabled origin", "Probability", high = pal["red_dark"])
  save_pub(s3a | (s3b / s3c) + plot_annotation(title = "Supplementary Figure S3. Main-model appendix", tag_levels = "A"), file.path(supp_dir, "Supplementary_Figure_S3_Nature_R_model_appendix"), 183, 135)

  write_source(d$effects, "nature_SuppFig4_country_effects")
  write_source(d$f4bc, "nature_SuppFig4_country_rankings")
  eff <- d$effects %>% mutate(transition = recode(transition, !!!transition_map), transition = factor(transition, levels = transition_order))
  s4a <- ggplot(eff, aes(transition, cohort_country, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    scale_fill_gradient2(low = pal["blue_dark"], mid = "#F7F7F7", high = pal["red_dark"], midpoint = 0) +
    labs(title = "Country-specific effect matrix", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  s4b <- d$f4bc %>% filter(transition == "Disabled -> Independent") %>% arrange(desc(prob)) %>%
    mutate(cohort_country = factor(cohort_country, levels = rev(cohort_country))) %>%
    ggplot(aes(prob, cohort_country)) + geom_col(fill = pal["blue"], width = 0.65) +
    scale_x_continuous(labels = percent_format()) + labs(title = "Recovery ranking", x = "Probability", y = NULL)
  s4c <- d$f4bc %>% filter(transition == "Disabled -> Dead") %>% arrange(desc(prob)) %>%
    mutate(cohort_country = factor(cohort_country, levels = rev(cohort_country))) %>%
    ggplot(aes(prob, cohort_country)) + geom_col(fill = pal["red_dark"], width = 0.65) +
    scale_x_continuous(labels = percent_format()) + labs(title = "Mortality ranking", x = "Probability", y = NULL)
  save_pub(s4a / (s4b | s4c) + plot_annotation(title = "Supplementary Figure S4. Cross-national heterogeneity appendix", tag_levels = "A"), file.path(supp_dir, "Supplementary_Figure_S4_Nature_R_heterogeneity_appendix"), 183, 140)

  write_source(d$subgroup, "nature_SuppFig5_subgroup_surface")
  write_source(d$f5c, "nature_SuppFig5_proxy")
  write_source(d$f5b, "nature_SuppFig5_landmark")
  write_source(d$f5d, "nature_SuppFig5_strict_adl")
  sg <- d$subgroup %>% mutate(sfcr_cat4 = recode(sfcr_cat4, !!!sfcr_map))
  s5a <- ggplot(sg, aes(disabled_next, dead_next, colour = sfcr_cat4)) +
    geom_point(size = 1.1, alpha = 0.75) +
    scale_colour_manual(values = sfcr_cols) +
    scale_x_continuous(labels = percent_format()) + scale_y_continuous(labels = percent_format()) +
    labs(title = "Subgroup risk surface", x = "Next disabled", y = "Next dead", colour = NULL)
  proxy_s <- d$f5c %>%
    mutate(sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
           transition = recode(transition, recovered = "Recovered", dead = "Dead"),
           sfcr_cat4 = factor(sfcr_cat4, levels = c("Partner + child", "Child only", "Neither", "Partner only")))
  s5b <- ggplot(proxy_s, aes(sfcr_cat4, prob, colour = proxy_group, linetype = transition, group = interaction(proxy_group, transition))) +
    geom_line(linewidth = 0.32) + geom_point(size = 1.1) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Proxy sensitivity", x = NULL, y = "Probability", colour = NULL, linetype = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  land_s <- d$f5b %>%
    mutate(sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
           sfcr_cat4 = factor(sfcr_cat4, levels = c("Partner + child", "Partner only", "Child only", "Neither")),
           next_state_label = str_to_title(next_state_label))
  s5c <- ggplot(land_s, aes(sfcr_cat4, prop, fill = next_state_label)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format()) + scale_fill_manual(values = state_cols) +
    labs(title = "First-onset landmark", x = NULL, y = "Proportion", fill = NULL) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  strict_s <- d$f5d %>% pivot_wider(names_from = definition, values_from = prevalence) %>% mutate(cohort = factor(cohort, levels = cohort_order))
  s5d <- ggplot(strict_s, aes(y = cohort)) +
    geom_segment(aes(x = strict_adl * 100, xend = default_adl * 100, yend = cohort), colour = pal["grey"], linewidth = 0.6) +
    geom_point(aes(x = default_adl * 100), colour = pal["red_dark"], size = 1.25) +
    geom_point(aes(x = strict_adl * 100), colour = pal["blue_dark"], size = 1.25) +
    labs(title = "Strict ADL definition", x = "Prevalence (%)", y = NULL)
  save_pub((s5a | s5b) / (s5c | s5d) + plot_annotation(title = "Supplementary Figure S5. Robustness appendix", tag_levels = "A") & theme(legend.position = "bottom"), file.path(supp_dir, "Supplementary_Figure_S5_Nature_R_robustness_appendix"), 183, 135)

  write_source(d$st8, "nature_SuppFig6_cohort_country_context")
  write_source(d$st10, "nature_SuppFig6_meta_context")
  s6a <- d$st8 %>% arrange(desc(n)) %>% slice_head(n = 18) %>%
    mutate(cohort_country = factor(cohort_country, levels = rev(cohort_country))) %>%
    ggplot(aes(n, cohort_country)) + geom_col(fill = pal["blue_light"], width = 0.65) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = "Largest cohort-country units", x = "Intervals", y = NULL)
  s6b <- d$st10 %>% mutate(transition = recode(transition, !!!transition_map), transition = factor(transition, levels = transition_order)) %>%
    ggplot(aes(I2, transition, fill = transition)) + geom_col(width = 0.65) +
    scale_fill_manual(values = c(pal["gold"], pal["red_dark"], pal["blue_dark"], pal["teal"])) +
    labs(title = "Heterogeneity summary", x = "I虏 (%)", y = NULL, fill = NULL) +
    guides(fill = "none")
  save_pub(s6a / s6b + plot_annotation(title = "Supplementary Figure S6. Context panels", tag_levels = "A"), file.path(supp_dir, "Supplementary_Figure_S6_Nature_R_context"), 183, 120)

  add_panels(tibble(
    figure = paste0("Supplementary Figure S", rep(1:6, times = c(3, 3, 3, 3, 4, 2))),
    panel = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C", "D", "A", "B"),
    title = c(
      "Missingness by cohort-wave", "Unique IDs by cohort", "Exclusion reasons",
      "Interval length distribution", "ADL item prevalence", "Recovery versus mortality",
      "Full SFCR model contrasts", "Independent-origin probabilities", "Disabled-origin probabilities",
      "Country-specific effect matrix", "Recovery ranking", "Mortality ranking",
      "Subgroup risk surface", "Proxy sensitivity", "First-onset landmark", "Strict ADL definition",
      "Largest cohort-country units", "Heterogeneity summary"
    ),
    meaning = c(
      "Shows missingness across harmonized variables and waves.",
      "Shows the respondent base by cohort.",
      "Shows why observations were excluded from interval analysis.",
      "Shows follow-up interval distribution across cohorts.",
      "Shows ADL item-level prevalence across cohorts.",
      "Shows the joint country profile of recovery and mortality after disability.",
      "Shows all SFCR model contrasts used in the primary transition models.",
      "Shows standardized probabilities among initially independent participants.",
      "Shows standardized probabilities among initially disabled participants.",
      "Shows available country-specific effect estimates.",
      "Ranks cohort-country units by recovery probability.",
      "Ranks cohort-country units by mortality probability.",
      "Shows subgroup patterns in next disability and death.",
      "Shows proxy interview sensitivity across SFCR categories.",
      "Shows outcomes after first disability onset.",
      "Compares default and strict ADL definitions.",
      "Shows the largest cohort-country units contributing interval data.",
      "Summarizes I虏 across transitions."
    ),
    source_data = c(
      "nature_SuppFig1_missingness.csv", "nature_Figure1A_C_profile.csv", "nature_Figure1E_exclusions.csv",
      "nature_SuppFig2_interval_lengths.csv", "nature_SuppFig2_adl_items.csv", "nature_SuppFig2_country_landscape.csv",
      "nature_SuppFig3_full_model_coefficients.csv", "nature_Figure3B_C_standardized_probabilities.csv", "nature_Figure3B_C_standardized_probabilities.csv",
      "nature_SuppFig4_country_effects.csv", "nature_SuppFig4_country_rankings.csv", "nature_SuppFig4_country_rankings.csv",
      "nature_SuppFig5_subgroup_surface.csv", "nature_SuppFig5_proxy.csv", "nature_SuppFig5_landmark.csv", "nature_SuppFig5_strict_adl.csv",
      "nature_SuppFig6_cohort_country_context.csv", "nature_SuppFig6_meta_context.csv"
    )
  ))
}

platform_sources()
fig2()
fig3()
fig4()
fig5()
supplement_figures()

write_csv(panel_rows, file.path(out, "panel_meanings.csv"))
writeLines(
  c(
    "# Nature R Visualization Panel Meanings",
    "",
    "This file documents the scientific role and source data for each panel.",
    "",
    apply(panel_rows, 1, function(x) {
      paste0("## ", x[["figure"]], x[["panel"]], ": ", x[["title"]], "\n\n", x[["meaning"]], "\n\nSource data: `source_data/", x[["source_data"]], "`\n")
    })
  ),
  con = file.path(out, "panel_meanings.md"),
  useBytes = TRUE
)

writeLines(
  c(
    "# Nature R Visualization",
    "",
    "Backend: R only, using ggplot2 + patchwork + svglite/cairo_pdf/ragg.",
    "",
    "Figure contract: the main figure sequence preserves the six-country story line: exposure/outcome landscape, pooled effects, cross-national heterogeneity, and robustness.",
    "",
    "Exports: each figure is saved as SVG, PDF, TIFF, and PNG. Source data for each panel are stored in `source_data/`.",
    "",
    "Panel meanings are documented in `panel_meanings.csv` and `panel_meanings.md`."
  ),
  con = file.path(out, "README_Nature_R_visualization.md"),
  useBytes = TRUE
)


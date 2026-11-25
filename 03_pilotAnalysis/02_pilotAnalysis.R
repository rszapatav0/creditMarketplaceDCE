# ============================================================================
# WTP Pilot DCE Analysis
# Single offered alternative vs. a fixed opt-out -> binary logit.
# Computes: model estimates, preliminary WTP, data-quality diagnostics,
# problematic attribute/level flags, and a sample-size rule-of-thumb.
# All results are saved to pilot_results.rds for pilot_report.Rmd to print.
# ============================================================================

pkgs <- c("readxl", "dplyr", "broom", "sandwich", "lmtest", "msm", "ggplot2")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))
rm(list=ls())

# ---- 0. Edit as needed -----------------------------------------------------
data_path <- "./03_pilotAnalysis/DCEdata.xlsx"
ids_sheet <- "main" # pilot main

dce_vars <- c("acopio", "productores", "nProd", "paqueteFlexible",
              "fundamentales", "aclimatar", "whisp", "priceTotal")
binary_attrs <- c("acopio", "paqueteFlexible", "fundamentales", "aclimatar", "whisp")

# ---- 1. Load & type the DCE variables only ---------------------------------
d <- read_excel(data_path, sheet=ids_sheet) %>%
  mutate(
    across(all_of(binary_attrs), ~ ifelse(.x == "true", 1, 0)),
    productores = as.numeric(productores),
    nProd       = as.numeric(nProd),
    priceTotal  = as.numeric(priceTotal),
    accepted    = as.integer(accepted)
  )

# ---- 2. Data-quality diagnostics -------------------------------------------
tasks_per_resp <- d %>% count(user, name = "n_tasks")

diagnostics <- data.frame(
  n_observations      = nrow(d),
  n_respondents       = n_distinct(d$user),
  tasks_per_resp_min  = min(tasks_per_resp$n_tasks),
  tasks_per_resp_mean = round(mean(tasks_per_resp$n_tasks), 1),
  tasks_per_resp_max  = max(tasks_per_resp$n_tasks),
  missing_choice      = sum(is.na(d$accepted)),
  missing_attributes  = sum(!complete.cases(d[, dce_vars])),
  accepted_share      = round(mean(d$accepted, na.rm = TRUE), 3),
  statusquo_share     = round(1 - mean(d$accepted, na.rm = TRUE), 3)
)

# ---- 3. Attribute-level variation & extreme choice patterns ---------------
level_summary <- bind_rows(lapply(dce_vars, function(v) {
  d %>%
    group_by(level = as.character(.data[[v]])) %>%
    summarise(n = n(), accept_rate = round(mean(accepted), 3), .groups = "drop") %>%
    mutate(attribute = v, share_of_data = round(n / nrow(d), 3))
})) %>% select(attribute, level, n, share_of_data, accept_rate)

flags <- level_summary %>%
  mutate(flag = case_when(
    share_of_data < 0.05 ~ "low variation (<5% of obs)",
    accept_rate <= 0.05  ~ "extreme: almost always rejected",
    accept_rate >= 0.95  ~ "extreme: almost always accepted",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(flag))

# ---- 4. Binary logit (opt-out = implicit reference, utility 0) ------------
form <- as.formula(paste("accepted ~", paste(dce_vars, collapse = " + ")))
m <- glm(form, data = d, family = binomial(link = "logit"))

# Cluster-robust SEs by respondent (each user answered many tasks)
vc <- vcovCL(m, cluster = d$user)
ct <- coeftest(m, vcov. = vc)
coef_df <- broom::tidy(ct) %>% rename(estimate = estimate, std.error = std.error)

ll  <- as.numeric(logLik(m))
ll0 <- as.numeric(logLik(glm(accepted ~ 1, data = d, family = binomial)))
model_stats <- data.frame(
  N = nrow(d), logLik = round(ll, 1), logLik_null = round(ll0, 1),
  McFadden_R2 = round(1 - ll / ll0, 3), AIC = round(AIC(m), 1)
)

# ---- 5. Preliminary WTP = -coef_attribute / coef_price ---------------------
price_idx <- which(names(coef(m)) == "priceTotal")
attr_idx  <- setdiff(seq_along(coef(m)), c(1, price_idx))  # drop intercept & price

wtp_table <- do.call(rbind, lapply(attr_idx, function(i) {
  se <- msm::deltamethod(
    as.formula(sprintf("~ -x%d/x%d", i, price_idx)),
    coef(m), vc
  )
  data.frame(
    term = names(coef(m))[i],
    wtp  = -coef(m)[i] / coef(m)[price_idx],
    se   = se
  )
}))
wtp_table$t <- wtp_table$wtp / wtp_table$se
wtp_table$p <- 2 * (1 - pnorm(abs(wtp_table$t)))
rownames(wtp_table) <- NULL

# ---- 6. Sample-size rule of thumb (Johnson & Orme, 2003) -------------------
# n >= 500*c / (t*a);  c = max attribute levels, a = alternatives/task (offer + opt-out = 2)
c_levels <- max(sapply(dce_vars, function(v) n_distinct(d[[v]])))
a_alts   <- 2
t_seq    <- seq(5, 100, by = 5)
sample_size_tbl <- data.frame(
  tasks_per_respondent = t_seq,
  min_recommended_n    = ceiling(500 * c_levels / (t_seq * a_alts))
)

ss_plot <- ggplot(sample_size_tbl, aes(tasks_per_respondent, min_recommended_n)) +
  geom_line() + geom_point() +
  labs(x = "Tasks per respondent", y = "Minimum recommended N (respondents)",
       title = "Rule-of-thumb sample size (Johnson & Orme, 2003)",
       subtitle = sprintf("n >= 500*c/(t*a), c = %d levels, a = %d alternatives", c_levels, a_alts)) +
  theme_minimal()

# ---- 7. Save for the R Markdown report --------------------------------------
results <- list(
  diagnostics = diagnostics, level_summary = level_summary, flags = flags,
  model_stats = model_stats, coef_df = coef_df,
  wtp_table = wtp_table, sample_size_tbl = sample_size_tbl, ss_plot = ss_plot
)
saveRDS(results, paste0("03_pilotAnalysis/", ids_sheet, "Results.rds"))

# Console printout only when run standalone (pilot_report.Rmd sets
# SOURCE_ONLY <- TRUE before sourcing this script, to avoid double-printing)
if (!exists("SOURCE_ONLY")) {
  cat("\n== Diagnostics ==\n");    print(diagnostics)
  cat("\n== Model stats ==\n");    print(model_stats)
  cat("\n== Coefficients ==\n");   print(coef_df)
  cat("\n== WTP ==\n");            print(wtp_table)
  cat("\n== Flags ==\n");          print(flags)
  cat("\n== Sample size ==\n");    print(sample_size_tbl)
}

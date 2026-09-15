# ============================================================================
# WTP DCE pilot data: wide (one row per user) -> long (one row per
# user x alternative), ready for single-alternative DCE regressions
# ============================================================================
#
# INPUTS
#   1) pilot_path : the consolidated pilot export ("Reporte consolidado"),
#                    one row per respondent. The last two relevant columns,
#                    WTPconfirmedAccess and WTPnotInterested, store (as text)
#                    JSON arrays of the alternative IDs the respondent
#                    accepted / rejected, e.g. '["b9","c109","g10"]'.
#   2) ids_path   : the card/alternative catalogue ("check" sheet), one row
#                    per alternative ID, with its attribute levels.
#
# OUTPUT
#   One row per (user, alternative). Alternative attributes are attached by
#   matching the "id" column; the binary DCE outcome variable "accepted" is
#   1 for IDs listed in WTPconfirmedAccess, 0 for IDs listed in
#   WTPnotInterested. Respondent-level covariates are carried on every row
#   belonging to that respondent.
#
# REPLICABILITY
#   The script makes no assumption about how many respondents or alternatives
#   exist. To process a new export, just update `pilot_path` (and, if it
#   ever changes, `ids_path`) below and re-run the whole script -- new
#   respondents/rows are picked up automatically.
# ============================================================================

# ---- 0. Packages -----------------------------------------------------------
required_pkgs <- c("readxl", "dplyr", "tidyr", "purrr", "jsonlite", "stringr", "writexl")
missing_pkgs  <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))
rm(list=ls())

# ---- 1. Inputs you may need to edit ----------------------------------------

pilot_path  <- "./03_pilotAnalysis/reports/DIG-IN_Reporte_Consolidado_2026-09-15_1021.xlsx"  # <- point to the latest export
pilot_sheet <- "Reporte consolidado"

ids_path  <- "./01_DCEdesign/coding/profiles.xlsx"
ids_sheet <- "check"

output_path <- "./03_pilotAnalysis/DCEpilotData.xlsx"

# Pre-registered attribute/level list (used only to sanity-check the ids file)
attributes_levels <- list(
  acopio          = c("true", "false"),
  productores     = c(10000, 50000, 100000),
  nProd           = c(1, 10, 20),
  paqueteFlexible = c("true", "false"),
  fundamentales   = c("true", "false"),
  aclimatar       = c("true", "false"),
  whisp           = c("true", "false"),
  priceTotal      = c(50, 100, 200)
)

# Respondent-level variables needed for the DCE regressions
respondent_vars <- c(
  "walletBalance",
  "sociodemographicQuestionsCompleted",
  "sociodemographicQuestions.ageRange",
  "sociodemographicQuestions.agrExperienceYears",
  "sociodemographicQuestions.experienceYears",
  "sociodemographicQuestions.institutionType",
  "sociodemographicQuestions.institutionOther",
  "sociodemographicQuestions.institutionRole",
  "sociodemographicQuestions.roleOther",
  "sociodemographicQuestions.sex"
)

# ---- 2. Read & clean the alternatives ("cards") table ----------------------

ids_raw <- read_excel(ids_path, sheet = ids_sheet)

# The catalogue is exported with a stray trailing apostrophe on every text
# cell (an Excel "stored as text" artifact, e.g. "b1'", "banco'", "10,000'"),
# and uses the literal string "null" (not a true blank cell) for missing
# priceFundamentales/priceTools. Clean both before converting types.
strip_quote  <- function(x) if (is.character(x)) sub("'$", "", x) else x
null_to_na   <- function(x) { x[x == "null"] <- NA; x }

ids_clean <- ids_raw %>%
  mutate(across(where(is.character), strip_quote)) %>%
  mutate(across(c(priceFundamentales, priceTools), null_to_na)) %>%
  transmute(
    id                 = str_trim(id),
    grupoId            = as.integer(grupoId),
    session            = as.integer(session),
    tipo               = as.character(tipo),                              # banco / coop / grupo card type
    acopio             = tolower(as.character(acopio)),
    productores        = as.numeric(gsub(",", "", as.character(productores))),
    nProd              = as.numeric(nProd),
    paqueteFlexible    = tolower(as.character(paqueteFlexible)),
    fundamentales      = tolower(as.character(fundamentales)),
    aclimatar          = tolower(as.character(aclimatar)),
    whisp              = tolower(as.character(whisp)),
    priceFundamentales = as.numeric(priceFundamentales),
    priceTools         = as.numeric(priceTools),
    priceTotal         = as.numeric(priceTotal)
  )

if (any(duplicated(ids_clean$id))) {
  stop("Duplicate alternative IDs found in the ids file -- please de-duplicate before proceeding.")
}

# Sanity-check every attribute against the pre-registered levels
check_levels <- function(x, allowed, name) {
  bad <- setdiff(unique(stats::na.omit(x)), allowed)
  if (length(bad) > 0) {
    warning(sprintf("Unexpected level(s) in '%s': %s", name, paste(bad, collapse = ", ")))
  }
}
for (v in names(attributes_levels)) {
  check_levels(ids_clean[[v]], as.character(attributes_levels[[v]]), v)
}

# ---- 3. Read the pilot (respondent-level, wide) table -----------------------

pilot_raw <- read_excel(pilot_path, sheet = pilot_sheet)

required_cols <- c("user", "sessionGroup", "WTPconfirmedAccess", "WTPnotInterested", respondent_vars)
missing_cols  <- setdiff(required_cols, names(pilot_raw))
if (length(missing_cols) > 0) {
  stop("The pilot file is missing expected column(s): ", paste(missing_cols, collapse = ", "))
}

# ---- 4. Parse the JSON-array ID-list columns --------------------------------

parse_id_list <- function(x) {
  purrr::map(x, function(txt) {
    if (is.na(txt) || !nzchar(trimws(txt))) return(character(0))
    parsed <- tryCatch(jsonlite::fromJSON(txt), error = function(e) character(0))
    ids <- unlist(parsed)
    if (is.null(ids)) character(0) else as.character(ids)
  })
}

# ---- 5. Explode accepted / rejected ID lists into long format --------------

accepted_long <- pilot_raw %>%
  transmute(user, id = parse_id_list(WTPconfirmedAccess)) %>%
  unnest(id) %>%
  mutate(accepted = 1L)

rejected_long <- pilot_raw %>%
  transmute(user, id = parse_id_list(WTPnotInterested)) %>%
  unnest(id) %>%
  mutate(accepted = 0L)

choice_long <- bind_rows(accepted_long, rejected_long)

# Same alternative listed as both accepted AND rejected for the same user
# would indicate a data problem -- flag it rather than silently duplicating.
dup_flags <- choice_long %>% count(user, id) %>% filter(n > 1)
if (nrow(dup_flags) > 0) {
  warning(sprintf(
    "%d user-alternative pair(s) appear in BOTH WTPconfirmedAccess and WTPnotInterested: %s",
    nrow(dup_flags),
    paste(paste0(dup_flags$user, "/", dup_flags$id), collapse = ", ")
  ))
}

# ---- 6. Attach alternative characteristics ----------------------------------

choice_long <- choice_long %>% left_join(ids_clean, by = "id")

unmatched <- choice_long %>% filter(is.na(grupoId))
if (nrow(unmatched) > 0) {
  warning(sprintf(
    "%d row(s) reference an alternative ID not found in the ids file: %s",
    nrow(unmatched), paste(unique(unmatched$id), collapse = ", ")
  ))
}

# ---- 7. Attach respondent-level covariates ----------------------------------

respondent_info <- pilot_raw %>% select(user, sessionGroup, all_of(respondent_vars))

dce_long <- choice_long %>% left_join(respondent_info, by = "user")

# Every alternative shown to a user belongs to that user's sessionGroup;
# flag it if that is ever not the case (data-integrity check only).
mismatch <- dce_long %>% filter(!is.na(grupoId) & grupoId != sessionGroup)
if (nrow(mismatch) > 0) {
  warning(sprintf(
    "%d row(s) where the alternative's grupoId differs from the respondent's sessionGroup",
    nrow(mismatch)
  ))
}

# ---- 8. Final tidy-up and column order --------------------------------------

dce_long <- dce_long %>%
  mutate(
    choice = factor(accepted, levels = c(0, 1),
                     labels = c("WTPnotInterested", "WTPconfirmedAccess"))
  ) %>%
  select(
    # identifiers
    user, id, sessionGroup, grupoId, tipo, #session, 
    # DCE outcome
    accepted, choice,
    # alternative (card) attributes
    acopio, productores, nProd, paqueteFlexible, fundamentales, aclimatar, whisp,
    priceTotal, #priceFundamentales, priceTools,
    # respondent-level covariates
    all_of(respondent_vars)
  ) %>%
  arrange(user, id)

# ---- 9. Save --------------------------------------------------------------

write_xlsx(dce_long, output_path)

cat(sprintf(
  "Done: %d respondents -> %d user-alternative rows written to '%s'\n",
  n_distinct(dce_long$user), nrow(dce_long), output_path
))

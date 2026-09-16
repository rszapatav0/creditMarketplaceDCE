#' ============================================================
# DISCRETE CHOICE EXPERIMENT - SIMULATION
#' ============================================================

rm(list=ls())
library(DoE.base)
library(mlogit)
library(ggplot2)
library(idefix)
library(MASS)
library(openxlsx)
library(dplyr)
set.seed(21) # Reproducibility


#' ------------------------------------------------------------
# STEP 1: Define Attributes and Levels ---------------------
#' ------------------------------------------------------------

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

# Define which attributes are numeric
numeric_attributes <- c("productores","nProd","priceTotal")

# Quick inspection
str(attributes_levels)
print(paste("Full factorial:", prod(lengths(attributes_levels)), "profiles"))




#' ------------------------------------------------------------
# STEP 2: Generate Full Factorial (Orthogonal) Design --------
#' ------------------------------------------------------------

# Automatically extract nlevels and factor.names from attributes_levels
full_factorial <- fac.design(
  nlevels      = sapply(attributes_levels, length),
  factor.names = attributes_levels,
  replications = 1,
  repeat.only  = FALSE,
  randomize    = FALSE
)

# Convert to clean data frame and fix types
full_factorial_df <- as.data.frame(full_factorial)

# Convert numeric attributes automatically
for (attr in numeric_attributes) {
  full_factorial_df[[attr]] <- as.numeric(as.character(full_factorial_df[[attr]]))
}

# Making changes
full_factorial_df_adapted <- full_factorial_df %>%
  ## Productores individuales
  ### No hay créditos individuales con paquete flexible
  filter(!(nProd==1 & paqueteFlexible=="true")) %>%
  ### No hay productores individuales con 100000 L
  filter(!(nProd==1 & productores==100000)) %>%
  ## Créditos grupales
  filter(!(nProd==20 & productores==10000)) %>%
  ## Créditos con acopio
  ### No hay créditos de acopio con solo 1 productor
  #filter(!(acopio=="true" & nProd==1)) %>%
  ### No hay créditos con solo 10000 L porque eso sería menos de 10 productores
  #filter(!(acopio=="true" & productores==10000)) %>%
  ## Todo lo que se cobra es falso
  filter(!(fundamentales=="false" &	aclimatar=="false" &	whisp=="false")) #&	croppie=="false"
print(paste("Full factorial:", nrow(full_factorial_df_adapted), "profiles"))

# Add profile ID automatically
full_factorial_df_adapted <- cbind(
  profile_id = 1:nrow(full_factorial_df_adapted),
  full_factorial_df_adapted
)

# Inspect
print(full_factorial_df_adapted)

#Checks
## Parametric checks - also driven by attributes_levels
dim(full_factorial_df_adapted) # Should be 18 rows + profile_id column

## Check balance for ALL attributes automatically
lapply(names(attributes_levels), function(attr) table(full_factorial_df_adapted[[attr]]))

# Saving profiles
file_path <- "01_DCEdesign/coding/profiles.xlsx"
wb        <- loadWorkbook(file_path)
writeData(wb, "automated", full_factorial_df_adapted)
saveWorkbook(wb, file_path, overwrite = TRUE)



# Utility functions for the application

calculate_adjusted_ard <- function(tier1) {
  tier1 / 0.9
}

calculate_tier2 <- function(adjusted_ard, coc_bonus, dv_bonus) {
  (adjusted_ard * 0.1) + coc_bonus + dv_bonus
}

format_currency <- function(x) {
  paste0("$", format(x, big.mark = ",", scientific = FALSE))
}

validate_numeric_input <- function(x, min = 0, max = Inf) {
  if(is.na(x) || !is.numeric(x)) return(FALSE)
  x >= min && x <= max
}

# Function to check if all threshold requirements are met
check_thresholds <- function(requirements) {
  all(requirements == "Yes")
}

# Function to calculate weighted score
calculate_weighted_score <- function(ratings, weights) {
  sum(ratings * weights) / sum(weights)
}

# Function to determine ranking tier
determine_tier <- function(score, funding_requested, tier1_amount) {
  if(score >= 80 && funding_requested <= tier1_amount) {
    return("Tier 1")
  } else {
    return("Tier 2")
  }
}

pluralize <- function(s) {
  ends <- "(sh?|x|z|ch)$"
  pluralify <- ifelse(grepl(ends, s, perl = TRUE), "es", "s")
  out <- gsub("ys$", "ies", paste0(s, pluralify))
  return(out)
}


factor_yesno <- function(v) {
  factor(
    v,
    levels = c(1,0),
    labels = c("Yes", "No")
  )
}
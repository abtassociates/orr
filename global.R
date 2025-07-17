library(shiny)
library(bslib)
library(DT)
library(sortable)
library(fontawesome)
library(collapse)
library(data.table)
library(shinyjs)
library(shinydisconnect)
library(stringr)

# Generate fake data
set.seed(123)

# Generate fake HIC data
coc_codes <- paste0(rep(c("NY", "CA", "TX", "FL"), each = 25), "-", sprintf("%03d", 1:100))
PROJECT_TYPES <- c("PSH", "RRH", "TH", "TH+RRH")
target_populations <- c("General", "DV", "Veterans", "Youth")

hic_data <- data.table(
  CoC_Code = sample(coc_codes, 30000, replace = TRUE),
  Project_Name = paste("Project", 1:30000),
  Organization_Name = paste("Org", sample(1:1000, 30000, replace = TRUE)),
  Project_Type = sample(project_types, 30000, replace = TRUE),
  Target_Population = sample(target_populations, 30000, replace = TRUE),
  McKinney_Vento = sample(c("Yes", "No"), 30000, replace = TRUE),
  Individual_Beds = sample(0:100, 30000, replace = TRUE),
  Family_Beds = sample(0:100, 30000, replace = TRUE),
  stringsAsFactors = FALSE
)

# Generate fake GIW data
giw_data <- data.table(
  CoC_Code = sample(coc_codes, 6500, replace = TRUE),
  Grant_Number = paste0("GN-", 1:6500),
  Project_Name = paste("Project", sample(1:30000, 6500)),
  Amount = round(runif(6500, 50000, 500000), 2),
  stringsAsFactors = FALSE
)

# Generate fake ARD data
ard_data <- data.table(
  CoC_Code = unique(coc_codes),
  Total_ARD = round(runif(length(unique(coc_codes)), 1000000, 10000000), 2),
  Tier_1 = NA,
  CoC_Bonus = NA,
  DV_Bonus = NA,
  stringsAsFactors = FALSE
)
ard_data$Tier_1 <- round(ard_data$Total_ARD * 0.94, 2)
ard_data$CoC_Bonus <- round(ard_data$Total_ARD * 0.05, 2)
ard_data$DV_Bonus <- round(ard_data$Total_ARD * 0.05, 2)

# Write data to CSV files
# write.csv(hic_data, "data/hic.csv", row.names = FALSE)
# write.csv(giw_data, "data/giw.csv", row.names = FALSE)
# write.csv(ard_data, "data/ard.csv", row.names = FALSE)
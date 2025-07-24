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

DB_CONFIG <- list(
  host = Sys.getenv("AWS_RDS_HOST"),
  port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
  dbname = Sys.getenv("AWS_RDS_DBNAME"),
  username = Sys.getenv("AWS_RDS_USERNAME"),
  password = Sys.getenv("AWS_RDS_PASSWORD")
)

# Generate fake HIC data
coc_codes <- paste0(rep(c("NY", "CA", "TX", "FL"), each = 25), "-", sprintf("%03d", 1:100))
project_types <- c("PSH", "RRH", "TH", "TH+RRH")
target_populations <- c("General", "DV", "Veterans", "Youth")

hic_data <- data.table(
  coc_instance_id = sample(1:4, 30000, replace = TRUE),
  #CoC_Code = sample(coc_codes, 30000, replace = TRUE),
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

# Coc_instance_id	coc	Coc_status	Date_created	Created_by
# 1	A	In Progress	Today	Alex
# 2	B	In Progress	Today	Marschall
# 3	C	In Progress	Today	Marschall
# 4	C	In Progress	Today	Thomas

cocs <- data.table(
  coc_code = c('A','B','C','D'),
  coc_name = c('Organization A','Organization B','Organization C','Organization D'),
  state = c('FL','NY','CA','TX')
)

users <- data.table(
  username = c('Alex','Marschall','Thomas'),
  firstname = c('Alex','Marschall','Thomas'),
  lastname = c('Silverman','Furman','Brittain'),
  email = c('alex.silverman@abtglobal.com', 'marschall.furman@abtglobal.com', 'thomas.brittain@abtglobal.com'),
  pw = c('AS2134$','MF1234$','TB1234$'),
  date_created = Sys.time() + 0:2,
  created_by = c('Alex','Marschall','Thomas')
)

coc_instances <- data.table(
  coc_instance_id = 1:4,
  coc = c('A','B','C','C'),
  coc_status = rep('In Progress',4),
  date_created = Sys.time() + 0:3, 
  created_by = c('Alex','Marschall','Marschall','Thomas')
)

# Coc_instance_user_id	Coc_instance_id	username	Coc_instance_role	Date_created	Created_by
# 1	1	Alex	Admin	today	Alex
# 2	2	Marschall	Admin	today	Marschall
# 3	2	Alex	Editor	today	Marschall
# 4	3	Marschall	Admin	today	Marschall
# 5	3	Thomas	Editor	today	Marschall
# 6	4	Thomas	Admin	today	Thomas

coc_instance_users <- data.table(
  coc_instance_user_id = 1:6,
  coc_instance_id = c(1,2,2,3,3,4),
  username = c('Alex','Marschall','Alex','Marschall','Thomas','Thomas'),
  coc_instance_role = c('Admin','Admin','Editor','Admin','Editor','Admin'),
  date_created = Sys.time() + 0:5,
  created_by = c('Alex', rep('Marschall',4),'Thomas')
)

# Request_id request_text coc coc_instance_id request_status requesting_user date_of_request

requests <- data.table(
  request_id = 1:2,
  request_text = c("Requesting access to CoC instance 2 for CoC B",
                   "Requesting access to CoC instance 3 for CoC C"),
  coc = c("B","C"),
  coc_instance_id = c(2,3),
  request_status = c("approved","approved"),
  requesting_user = c("Alex","Thomas"),
  date_of_request = c(Sys.time(), Sys.time() + 3)
)


fake_projects <- hic_data |>
  fmutate(
    project_id = 1:fnrow(hic_data)#,
  ) |> 
  fselect(project_id, coc_instance_id, Project_Name, Organization_Name, Project_Type,McKinney_Vento) 




# Write data to CSV files
# write.csv(hic_data, "data/hic.csv", row.names = FALSE)
# write.csv(giw_data, "data/giw.csv", row.names = FALSE)
# write.csv(ard_data, "data/ard.csv", row.names = FALSE)
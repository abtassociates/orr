populate_db <- function(
    add_demo_data = basename(getwd()) != "ORR", 
    USE_SQLITE = Sys.getenv("RSTUDIO") == "1"
) {
library(here)
library(DBI)

files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)
  
library(data.table)
library(glue)
library(collapse)

DB_POOL <- set_up_db_connection()
USE_SQLITE <- USE_SQLITE && Sys.getenv("RSTUDIO") == "1"

HIC_DATA_FILEPATH <- here("database/HIC_RawData2025 - 7.21.25_TEST.csv")
GIW_DATA_FILEPATH <- here("database/GIW.csv")
HUD_ARD_DATA_FILEPATH <- here("database/HUD_ard_report.csv")

# Ideally, this get dynamically populated with data from Cognito
# But not a huge deal since this won't be run after we're live
ADMIN_USERS <- "
  ('alex.silverman@abtglobal.com', 'Alex', 'Silverman', NULL),
  ('marschall.furman@abtglobal.com', 'Marschall', 'Furman', NULL),
  ('Victoria.Lopez@abtglobal.com', 'Victoria', 'Lopez', NULL),
  ('anthony.appau@abtglobal.com', 'Anthony', 'Appau', NULL),
  ('orr_service@abtglobal.com', 'ORR', 'Service Account', NULL),
  ('louise.rothschild@abtglobal.com', 'Louise', 'Rothschild', NULL)
"

drop_table <- function(tbl) {
	message(glue::glue("Dropping {tbl}"))
  if(USE_SQLITE) DBI::dbExecute(DB_POOL, "PRAGMA foreign_keys = OFF;")
  DBI::dbExecute(DB_POOL, glue::glue("DROP TABLE IF EXISTS {tbl} {ifelse(USE_SQLITE, '', 'CASCADE')};"))
	if(USE_SQLITE) DBI::dbExecute(DB_POOL, "PRAGMA foreign_keys = ON;")
}

id_var_attrs <- if(USE_SQLITE) 'INTEGER PRIMARY KEY AUTOINCREMENT' else 'SERIAL PRIMARY KEY'

# create users and All HIC Data ---------------------
############################
# LIST OF USERS
###########################
drop_table("users");
DBI::dbExecute(DB_POOL, " 
CREATE TABLE IF NOT EXISTS users (
    username VARCHAR(100) PRIMARY KEY, -- email?
    firstname VARCHAR(255),
    lastname VARCHAR(255),
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
")

DBI::dbExecute(DB_POOL, glue::glue("
INSERT INTO users (username, firstname, lastname, created_by)
VALUES {ADMIN_USERS};
"))


#######################
# REFERENCES (LOOKUPS/DROPDOWNS)
######################
drop_table("lookups")
DBI::dbExecute(DB_POOL, glue::glue("
-- Create a single, consolidated table for all reference/lookup values
CREATE TABLE IF NOT EXISTS lookups (
    reference_id {id_var_attrs},
    -- Discriminator column to identify the type of reference (e.g., 'project_type', 'coc_status')
    reference_type VARCHAR(100) NOT NULL,
    -- The main display value for the reference item (e.g., 'Rapid Re-Housing', 'In Progress')
    value VARCHAR(255) NOT NULL,
    -- Optional short code or abbreviation (e.g., 'RRH', 'PSH', 'DV')
    value_abbrev VARCHAR(100) NULL,
    -- Optional longer description or secondary value (e.g., the plural 'Individuals')
    value_long VARCHAR(255) NULL,
    -- A flag to indicate if this option requires an 'Other (please specify)' text field in the UI
    other_specify_flag BOOLEAN DEFAULT FALSE,
    -- Standard audit columns
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

# -- Insert all data into the new consolidated table
DBI::dbExecute(DB_POOL, "
INSERT INTO lookups (reference_type, value, created_by)
VALUES
-- from request_statuses
('request_status', 'Sent', 'orr_service@abtglobal.com'),
('request_status', 'Approved', 'orr_service@abtglobal.com'),
('request_status', 'Rejected', 'orr_service@abtglobal.com'),

-- from coc_version_roles
('coc_version_role', 'Owner', 'orr_service@abtglobal.com'),
('coc_version_role', 'Editor', 'orr_service@abtglobal.com'),

-- from coc_statuses
('coc_status', 'Not Started', 'orr_service@abtglobal.com'),
('coc_status', 'In Progress', 'orr_service@abtglobal.com'),
('coc_status', 'Completed', 'orr_service@abtglobal.com'),

-- from funding_actions
('funding_action', 'New', 'orr_service@abtglobal.com'),
('funding_action', 'Renew', 'orr_service@abtglobal.com'),
('funding_action', 'Expand', 'orr_service@abtglobal.com'),
('funding_action', 'Reallocate', 'orr_service@abtglobal.com'),
('funding_action', 'Ignore', 'orr_service@abtglobal.com'),
('funding_action', 'Replace', 'orr_service@abtglobal.com'),

-- from bonus_types
('bonus_type', 'CoC Bonus', 'orr_service@abtglobal.com'),
('bonus_type', 'DV Bonus', 'orr_service@abtglobal.com'),

-- from priorities
('priority', 'High', 'orr_service@abtglobal.com'),
('priority', 'Medium', 'orr_service@abtglobal.com'),
('priority', 'Low', 'orr_service@abtglobal.com'),
('priority', 'Unspecified', 'orr_service@abtglobal.com');
")

DBI::dbExecute(DB_POOL, "
INSERT INTO lookups (reference_type, value, other_specify_flag, created_by)
VALUES
-- from request_rejection_reasons
('request_rejection_reason', 'Not Associated with CoC', FALSE, 'orr_service@abtglobal.com'),
('request_rejection_reason', 'Other', TRUE, 'orr_service@abtglobal.com');
")

DBI::dbExecute(DB_POOL, "
INSERT INTO lookups (reference_type, value, value_long, created_by)
VALUES
-- from project_types
('project_type', 'RRH', 'Rapid Re-Housing', 'orr_service@abtglobal.com'),
('project_type', 'PSH', 'Permanent Supportive Housing', 'orr_service@abtglobal.com'),
('project_type', 'TH', 'Transitional Housing', 'orr_service@abtglobal.com'),
('project_type', 'TH+RRH', 'Transitional Housing + Rapid Re-Housing', 'orr_service@abtglobal.com'),
('project_type', 'HMIS Project', 'HMIS Project', 'orr_service@abtglobal.com'),
('project_type', 'SSO-CE', 'Supportive Services Only - Coordinated Entry', 'orr_service@abtglobal.com'),
('project_type', 'SSO', 'Supportive Services Only', 'orr_service@abtglobal.com'),
('project_type', 'DEM', 'Demonstration Project', 'orr_service@abtglobal.com'),
('project_type', 'OPH', 'Other Permanent Housing', 'orr_service@abtglobal.com'),
('project_type', 'SH', 'Safe Haven', 'orr_service@abtglobal.com'),
('project_type', 'ES', 'Emergency Shelter', 'orr_service@abtglobal.com'),

-- from target_populations
('target_population', 'General', 'General', 'orr_service@abtglobal.com'),
('target_population', 'DV', 'Domestic Violence', 'orr_service@abtglobal.com'),
('target_population', 'CH', 'Chronically Homeless', 'orr_service@abtglobal.com'),
('target_population', 'Vet', 'Veteran', 'orr_service@abtglobal.com'),
('target_population', 'Yth', 'Youth', 'orr_service@abtglobal.com'),
('target_population', 'HIV', 'Human Immunodeficiency Virus', 'orr_service@abtglobal.com'),
('target_population', 'NA', 'Not Applicable', 'orr_service@abtglobal.com');
")

DBI::dbExecute(DB_POOL, "
INSERT INTO lookups (reference_type, value_abbrev, value, value_long, created_by)
VALUES
-- from population_groups
('population_group', 'Ind', 'Individual', 'Individuals', 'orr_service@abtglobal.com'),
('population_group', 'Fam', 'Family', 'Families', 'orr_service@abtglobal.com');
")

#####################
# HUD PROVIDED DATA
###################
drop_table("all_hic_data")
DBI::dbExecute(DB_POOL, "
CREATE TABLE IF NOT EXISTS all_hic_data (
	row_num INTEGER, -- unique within a CoC
	hudnum VARCHAR(6), -- CoC Code
    coc_name TEXT,
    organization_name VARCHAR,
    project_name VARCHAR,
    -- hic_date DATE,
    project_type SMALLINT REFERENCES lookups(reference_id), -- Changed to reference lookups
    geocode VARCHAR(10),
    target_population SMALLINT REFERENCES lookups(reference_id), -- Changed to reference lookups
    -- bed_type INTEGER,
    -- hmis_participating VARCHAR(1),
    -- inventory_type VARCHAR(1),
    mckinneyventoesg BOOLEAN,
    mckinneyventoesges BOOLEAN,
    mckinneyventoesgrrh BOOLEAN,
    mckinneyventoesgcov BOOLEAN,
    mckinneyventoesgrrhcov BOOLEAN,
    mckinneyventococ BOOLEAN,
    mckinneyventococsh BOOLEAN,
    mckinneyventococth BOOLEAN,
    mckinneyventococpsh BOOLEAN,
    mckinneyventococrrh BOOLEAN,
    mckinneyventococsro BOOLEAN,
    mckinneyventococthrrh BOOLEAN,
    mckinneyventospc BOOLEAN,
    mckinneyventos8 BOOLEAN,
    mckinneyventoshp BOOLEAN,
    mckinneyventoyhdp BOOLEAN,
    mckinneyventoyhdprenewals BOOLEAN,
    beds_hh_w_children INTEGER,
    --units_hh_w_children INTEGER,
    veteran_beds_hh_w_children INTEGER,
    youth_beds_hh_w_children INTEGER,
    ch_beds_hh_w_children INTEGER,
    beds_hh_wo_children INTEGER,
    veteran_beds_hh_wo_children INTEGER,
    youth_beds_hh_wo_children INTEGER,
    ch_beds_hh_wo_children INTEGER,
    beds_hh_w_only_children INTEGER,
    ch_beds_hh_w_only_children INTEGER,

	-- A row num must be unique within a coc
    CONSTRAINT uq_row_num_hudnum UNIQUE (row_num, hudnum)
  );               
")

# import HIC data ----------------
hic_data <- fread(HIC_DATA_FILEPATH)

# Rename columns to match SQL table
setnames(hic_data, old = c(
  "Row #",
  "HudNum",
  "CoC",
  "Organization Name",
  "Project Name",
  "Project Type",
  "Geocode",
  "Target Population",
  "mcKinneyVentoEsgEs",
  "mcKinneyVentoEsgRrh",
  "mcKinneyVentoEsgCov",
  "mcKinneyVentoEsgRUSH",
  "mcKinneyVentoCocSh",
  "mcKinneyVentoCocTh",
  "mcKinneyVentoCocPsh",
  "mcKinneyVentoCocRrh",
  "mcKinneyVentoCocSro",
  "mcKinneyVentoCocThRrh",
  "mcKinneyVentoSpC",
  "mcKinneyVentoS8",
  "mcKinneyVentoShp",
  "mcKinneyVentoYhdp",
  "mcKinneyVentoYhdpRenewals",
  "Beds HH w/ Children",
  "Veteran Beds HH w/ Children",
  "Youth Beds HH w/ Children",
  "CH Beds HH w/ Children",
  "Beds HH w/o Children",
  "Veteran Beds HH w/o Children",
  "Youth Beds HH w/o Children",
  "CH Beds HH w/o Children",
  "Beds HH w/ only Children",
  "CH Beds HH w only Children"
), new = c(
  "row_num",
  "hudnum",
  "coc_name",
  "organization_name",
  "project_name",
  "project_type",
  "geocode",
  "target_population",
  "mckinneyventoesges",
  "mckinneyventoesgrrh",
  "mckinneyventoesgcov",
  "mckinneyventoesgrrhcov",
  "mckinneyventococsh",
  "mckinneyventococth",
  "mckinneyventococpsh",
  "mckinneyventococrrh",
  "mckinneyventococsro",
  "mckinneyventococthrrh",
  "mckinneyventospc",
  "mckinneyventos8",
  "mckinneyventoshp",
  "mckinneyventoyhdp",
  "mckinneyventoyhdprenewals",
  "beds_hh_w_children",
  "veteran_beds_hh_w_children",
  "youth_beds_hh_w_children",
  "ch_beds_hh_w_children",
  "beds_hh_wo_children",
  "veteran_beds_hh_wo_children",
  "youth_beds_hh_wo_children",
  "ch_beds_hh_wo_children",
  "beds_hh_w_only_children",
  "ch_beds_hh_w_only_children"
))

# Drop columns that aren't in your SQL table (if they exist)
cols_to_drop <- c("mcKinneyVentoUnshelt", "mcKinneyVentoRural")
hic_data[, (cols_to_drop) := NULL]

# Add missing columns that are in SQL but not in CSV (with default values)
hic_data[, mckinneyventoesg := FALSE]
hic_data[, mckinneyventococ := FALSE]

# Fetch lookup tables from database
project_type_lookup <- DBI::dbGetQuery(DB_POOL, 
  "SELECT reference_id, value FROM lookups WHERE reference_type = 'project_type'")

target_population_lookup <- DBI::dbGetQuery(DB_POOL, 
  "SELECT reference_id, value FROM lookups WHERE reference_type = 'target_population'")

# Create named vectors for mapping
project_type_map <- setNames(project_type_lookup$reference_id, project_type_lookup$value)
target_population_map <- setNames(target_population_lookup$reference_id, target_population_lookup$value)

# Convert NAs, empty string to actual valid value "NA"
hic_data[, target_population := ifelse(is.na(target_population) | target_population == "", "NA", target_population)]

# Convert character codes to reference IDs
hic_data[, project_type := project_type_map[project_type]]
hic_data[, target_population := target_population_map[target_population]]

DBI::dbAppendTable(DB_POOL, "all_hic_data", hic_data)

# populate HIC, States, CoCs, and GIWs tables ---------------------
DBI::dbExecute(DB_POOL, "ALTER TABLE all_hic_data ADD COLUMN hic_data_id INTEGER")
DBI::dbExecute(DB_POOL, "ALTER TABLE all_hic_data ADD COLUMN date_created TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE all_hic_data ADD COLUMN created_by VARCHAR(100) REFERENCES users(username)")
DBI::dbExecute(DB_POOL, "ALTER TABLE all_hic_data ADD COLUMN date_updated TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE all_hic_data ADD COLUMN updated_by VARCHAR(100) NULL REFERENCES users(username)")

DBI::dbExecute(DB_POOL, "
UPDATE all_hic_data
SET 
  created_by = 'orr_service@abtglobal.com', 
  updated_by = 'orr_service@abtglobal.com', 
  date_updated = CURRENT_TIMESTAMP,
  date_created = CURRENT_TIMESTAMP;
")

drop_table("states")
DBI::dbExecute(DB_POOL, "
CREATE TABLE IF NOT EXISTS states (
    state_code VARCHAR(2) PRIMARY KEY,
    state_name VARCHAR(100),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
")

DBI::dbExecute(DB_POOL, "
INSERT INTO states (state_code, state_name, created_by)
SELECT DISTINCT 
    SUBSTR(hudnum, 1, 2) as state_code,
    CASE SUBSTR(hudnum, 1, 2)
        WHEN 'AL' THEN 'Alabama'
        WHEN 'AK' THEN 'Alaska'
        WHEN 'AZ' THEN 'Arizona'
        WHEN 'AR' THEN 'Arkansas'
        WHEN 'CA' THEN 'California'
        WHEN 'CO' THEN 'Colorado'
        WHEN 'CT' THEN 'Connecticut'
        WHEN 'DE' THEN 'Delaware'
        WHEN 'FL' THEN 'Florida'
        WHEN 'GA' THEN 'Georgia'
        WHEN 'HI' THEN 'Hawaii'
        WHEN 'ID' THEN 'Idaho'
        WHEN 'IL' THEN 'Illinois'
        WHEN 'IN' THEN 'Indiana'
        WHEN 'IA' THEN 'Iowa'
        WHEN 'KS' THEN 'Kansas'
        WHEN 'KY' THEN 'Kentucky'
        WHEN 'LA' THEN 'Louisiana'
        WHEN 'ME' THEN 'Maine'
        WHEN 'MD' THEN 'Maryland'
        WHEN 'MA' THEN 'Massachusetts'
        WHEN 'MI' THEN 'Michigan'
        WHEN 'MN' THEN 'Minnesota'
        WHEN 'MS' THEN 'Mississippi'
        WHEN 'MO' THEN 'Missouri'
        WHEN 'MT' THEN 'Montana'
        WHEN 'NE' THEN 'Nebraska'
        WHEN 'NV' THEN 'Nevada'
        WHEN 'NH' THEN 'New Hampshire'
        WHEN 'NJ' THEN 'New Jersey'
        WHEN 'NM' THEN 'New Mexico'
        WHEN 'NY' THEN 'New York'
        WHEN 'NC' THEN 'North Carolina'
        WHEN 'ND' THEN 'North Dakota'
        WHEN 'OH' THEN 'Ohio'
        WHEN 'OK' THEN 'Oklahoma'
        WHEN 'OR' THEN 'Oregon'
        WHEN 'PA' THEN 'Pennsylvania'
        WHEN 'RI' THEN 'Rhode Island'
        WHEN 'SC' THEN 'South Carolina'
        WHEN 'SD' THEN 'South Dakota'
        WHEN 'TN' THEN 'Tennessee'
        WHEN 'TX' THEN 'Texas'
        WHEN 'UT' THEN 'Utah'
        WHEN 'VT' THEN 'Vermont'
        WHEN 'VA' THEN 'Virginia'
        WHEN 'WA' THEN 'Washington'
        WHEN 'WV' THEN 'West Virginia'
        WHEN 'WI' THEN 'Wisconsin'
        WHEN 'WY' THEN 'Wyoming'
        WHEN 'DC' THEN 'District of Columbia'
        WHEN 'AS' THEN 'American Samoa'
        WHEN 'GU' THEN 'Guam'
        WHEN 'MP' THEN 'Northern Mariana Islands'
        WHEN 'PR' THEN 'Puerto Rico'
        WHEN 'VI' THEN 'U.S. Virgin Islands'
        ELSE 'Unknown'
    END as state_name,
    'orr_service@abtglobal.com' -- Replace with appropriate username
FROM all_hic_data;
")

drop_table("cocs")
DBI::dbExecute(DB_POOL, "
CREATE TABLE IF NOT EXISTS cocs (
    coc_code VARCHAR(6) PRIMARY KEY,
    coc_name TEXT,
    state VARCHAR(2) REFERENCES states(state_code),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
")

DBI::dbExecute(DB_POOL, "
INSERT INTO cocs (coc_code, coc_name, state, created_by)
SELECT DISTINCT 
    hudnum as coc_code,
	coc_name as coc_name,
    SUBSTR(hudnum, 1, 2) as state,
    'orr_service@abtglobal.com'
FROM all_hic_data a1;
")

drop_table("giw")
DBI::dbExecute(DB_POOL, "
CREATE TABLE IF NOT EXISTS giw (
    grant_number VARCHAR(15) PRIMARY KEY,
    coc VARCHAR(6) REFERENCES cocs(coc_code),
    applicant_name VARCHAR,
    project_name VARCHAR,
    expiration_year INTEGER,
    project_component VARCHAR,
    restriction_dv_or_ydhp VARCHAR,
    dv_ard_estimated DECIMAL,
    yhdp_ard_estimated DECIMAL,
    cocs_ard_estimated DECIMAL,
    total_units INTEGER,
    total_ara DECIMAL
);               
")

# import GIW ---------------------
# Read the data
giw_data <- fread(GIW_DATA_FILEPATH, encoding = "Latin-1")

# Rename columns to match SQL table
setnames(giw_data, old = c(
  "Grant Number",
  "CoC",
  "Applicant Name",
  "Project Name",
  "Expiration Year",
  "Project Component",
  "Restriction (DV or YHDP)",
  "DV ARD (Estimated)",
  "YHDP ARD (Estimated)",
  "CoCs ARD (Estimated)",
  "Total Units",
  "Total ARA"
), new = c(
  "grant_number",
  "coc",
  "applicant_name",
  "project_name",
  "expiration_year",
  "project_component",
  "restriction_dv_or_ydhp",
  "dv_ard_estimated",
  "yhdp_ard_estimated",
  "cocs_ard_estimated",
  "total_units",
  "total_ara"
))

# only keep the ones where the CoC is in the HIC data
giw_data <- giw_data |> fsubset(coc %in% funique(hic_data$hudnum))
# Append to database
DBI::dbAppendTable(DB_POOL, "giw", giw_data)


# Update GIW ---------------------
DBI::dbExecute(DB_POOL, "ALTER TABLE giw ADD COLUMN date_created TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE giw ADD COLUMN created_by VARCHAR(100) REFERENCES users(username)")
DBI::dbExecute(DB_POOL, "ALTER TABLE giw ADD COLUMN date_updated TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE giw ADD COLUMN updated_by VARCHAR(100) NULL REFERENCES users(username)")

DBI::dbExecute(DB_POOL, "
UPDATE giw
SET 
  created_by = 'orr_service@abtglobal.com', 
  updated_by = 'orr_service@abtglobal.com', 
  date_updated = CURRENT_TIMESTAMP,
  date_created = CURRENT_TIMESTAMP;
")

# Create HUD Report ---------------------
drop_table("hud_ard_report")
DBI::dbExecute(DB_POOL, "
CREATE TABLE IF NOT EXISTS hud_ard_report (
	coc VARCHAR(6) REFERENCES cocs(coc_code),
    coc_number_and_name TEXT,
    pprn INTEGER,
    estimated INTEGER,
    tier_1 INTEGER,
    coc_bonus INTEGER NULL,
    dv_bonus INTEGER,
    coc_planning INTEGER
);
")

# Import HUD ARD Report ---------------------
# Read the data
hud_ard_data <- fread(HUD_ARD_DATA_FILEPATH, encoding="Latin-1")

# Rename columns to match SQL table
setnames(hud_ard_data, old = c(
  "CoCName",
  "CoC Number and Name",
  "PPRN",
  "Estimated ARD",
  "Tier 1",
  "CoC Bonus",
  "DV Bonus",
  "CoC Planning"
), new = c(
  "coc",
  "coc_number_and_name",
  "pprn",
  "estimated",
  "tier_1",
  "coc_bonus",
  "dv_bonus",
  "coc_planning"
))

# only keep the ones where the CoC is in the HIC data
hud_ard_data <- hud_ard_data |> fsubset(coc %in% funique(hic_data$hudnum))
# Append to database
DBI::dbAppendTable(DB_POOL, "hud_ard_report", hud_ard_data)

# Create rest of table ---------------------
DBI::dbExecute(DB_POOL, "ALTER TABLE hud_ard_report ADD COLUMN date_created TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE hud_ard_report ADD COLUMN created_by VARCHAR(100) REFERENCES users(username)")
DBI::dbExecute(DB_POOL, "ALTER TABLE hud_ard_report ADD COLUMN date_updated TIMESTAMP")
DBI::dbExecute(DB_POOL, "ALTER TABLE hud_ard_report ADD COLUMN updated_by VARCHAR(100) REFERENCES users(username)")

DBI::dbExecute(DB_POOL, "
UPDATE hud_ard_report
SET 
    created_by = 'orr_service@abtglobal.com',
    updated_by = 'orr_service@abtglobal.com',  
    date_updated = CURRENT_TIMESTAMP,
    date_created = CURRENT_TIMESTAMP;
")

#######################
# USER-COC MANAGEMENT
######################
drop_table("coc_versions")
drop_table("coc_version_requests")
drop_table("coc_version_users")

DBI::dbExecute(DB_POOL, glue::glue("
--- CoC versions (CoCs can have versions, as new users may decide to create their own)
--- Also a single user could create multiple versions of the same CoC, to modify everything from inventory all the way to ranking
CREATE TABLE IF NOT EXISTS coc_versions (
    coc_version_id {id_var_attrs},
	coc_version_name VARCHAR(255),
    coc VARCHAR(6) REFERENCES cocs(coc_code),
    coc_status SMALLINT REFERENCES lookups(reference_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
--- CoC version Requests (one row per request per CoC version)
-- users who wish to access a particular existing version makes a 
-- request in the system that the version Admin can approve/reject
CREATE TABLE IF NOT EXISTS coc_version_requests (
	coc_request_id {id_var_attrs},
	coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id),
	request_status SMALLINT REFERENCES lookups(reference_id), -- Changed to reference lookups
	reason_for_rejection SMALLINT REFERENCES lookups(reference_id), -- Changed to reference lookups
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
-- CoC version Users
--- This is a many-to-many relationship between users and CoC versions
--- when a new user registers for a CoC that's already been created (i.e. for which an CoC version already exists)
--- they have the option to request access to that version or to create their own version
--- if a user creates a new CoC version for the same CoC, the MVP will copy everything over from the original CoC version
--- Advanced would allow them to select what to carry over
CREATE TABLE IF NOT EXISTS coc_version_users (
    coc_version_user_id {id_var_attrs},
    coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id),
    username VARCHAR(100) REFERENCES users(username),
    coc_version_role SMALLINT REFERENCES lookups(reference_id), -- Changed to reference lookups
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),
    
    -- A user cannot be associated with the same CoC version more than once
    CONSTRAINT uq_coc_version_users UNIQUE (coc_version_id, username)
);
"))

########################
#	TOOL-BASED DATA (HARDCODED)
########################
drop_table("factor_groups")
drop_table("factor_subgroups")
drop_table("thresholds")
drop_table("rating_factors")

###### RATING #########
DBI::dbExecute(DB_POOL, glue::glue("
--- Factor Groups (heading in CUSTOMIZE RATING CRITERIA page, e.g. 'Performance Measures')
CREATE TABLE IF NOT EXISTS factor_groups (
    factor_group_id {id_var_attrs},
    factor_group VARCHAR(100),
    funding_action SMALLINT REFERENCES lookups(reference_id),
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

message("about to do CTEs for factor_groups")
DBI::dbExecute(DB_POOL, "
-- Use CTEs to look up funding_action IDs
WITH
    l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
    l_renew AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'Renew')
INSERT INTO factor_groups (factor_group, funding_action, created_by)
VALUES
('Performance Measures', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Serve High Needs Populations', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Project Effectiveness', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
-- ('Equity Factors', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Other and Local Criteria', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Experience', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
('Design of Housing & Supportive Services', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
('Timeliness', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
('Financial', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
('Project Effectiveness', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
-- ('Equity Factors', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
('Other and Local Criteria', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com');
")

DBI::dbExecute(DB_POOL, glue::glue("
--- Factor Sub Groups (subheading in CUSTOMIZE RATING CRITERIA page, e.g. 'Length of Stay')
CREATE TABLE IF NOT EXISTS factor_subgroups (
    factor_subgroup_id {id_var_attrs},
    factor_subgroup VARCHAR(100),
    factor_group SMALLINT REFERENCES factor_groups(factor_group_id),
    funding_action SMALLINT REFERENCES lookups(reference_id), -- 1. New, 2. Renew
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

message("about to do CTEs for factor_subgroups")
DBI::dbExecute(DB_POOL, "
-- Use CTEs to look up factor_group and funding_action IDs
WITH
    l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
    l_renew AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'Renew'),
    fg_perf_meas_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Performance Measures' AND funding_action = (SELECT reference_id FROM l_renew)),
    fg_high_needs_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Serve High Needs Populations' AND funding_action = (SELECT reference_id FROM l_renew))
    -- fg_equity_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Equity Factors' AND funding_action = (SELECT reference_id FROM l_renew)),
    -- fg_equity_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Equity Factors' AND funding_action = (SELECT reference_id FROM l_new))
INSERT INTO factor_subgroups (factor_subgroup, factor_group, funding_action, created_by)
VALUES
('Length of Stay', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Exits to Permanent Housing', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Returns to Homelessness (if data is available for project)', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('New or Increased Income and Earned Income', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Coordinated Assessment Score', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('Project Focuses on Chronically Homeless People', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
('APR Data on ≥ 50% Disability/Zero Income/Unsheltered', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com');
-- ('Agency Leadership, Governance, and Policies', (SELECT factor_group_id FROM fg_equity_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
-- ('Program Participant Outcomes', (SELECT factor_group_id FROM fg_equity_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
-- ('Agency Leadership, Governance, and Policies', (SELECT factor_group_id FROM fg_equity_new), (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
-- ('Program Participant Outcomes', (SELECT factor_group_id FROM fg_equity_new), (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com');
")

message("about to do thresholds")
DBI::dbExecute(DB_POOL, glue::glue("
-- Thresholds (Reference table)
--- unique list of thresholds
CREATE TABLE IF NOT EXISTS thresholds (
    threshold_id {id_var_attrs},
    type VARCHAR(3), -- 'CoC' or 'HUD'
    threshold_text TEXT,
    coc_version_id SMALLINT NULL REFERENCES coc_versions(coc_version_id),  -- only filled out for custom thresholds
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),
    
  
CONSTRAINT custom_threshold_unique_key UNIQUE (coc_version_id, threshold_text)
);
"))

message("about to do populate thresholds")
DBI::dbExecute(DB_POOL, "
INSERT INTO thresholds (type, threshold_text, created_by)
VALUES ('CoC', 'Coordinated Entry Participation', 'orr_service@abtglobal.com'),
('CoC', 'Housing First and/or Low Barrier Implementation', 'orr_service@abtglobal.com'),
('CoC', 'Documented, secured minimum match', 'orr_service@abtglobal.com'),
('CoC', 'Project has reasonable costs per permanent housing exit, as defined locally', 'orr_service@abtglobal.com'),
('CoC', 'Project is financially feasible', 'orr_service@abtglobal.com'),
('CoC', 'Applicant is active CoC participant', 'orr_service@abtglobal.com'),
('CoC', 'Application is complete and data are consistent', 'orr_service@abtglobal.com'),
('CoC', 'Data quality at or above 90%', 'orr_service@abtglobal.com'),
('CoC', 'Bed/unit utilization rate at or above 90%', 'orr_service@abtglobal.com'),
('CoC', 'Acceptable organizational audit/financial review', 'orr_service@abtglobal.com'),
('CoC', 'PIT participation', 'orr_service@abtglobal.com'),
('CoC', 'Healthcare MOU', 'orr_service@abtglobal.com'),
('HUD', '1. Applicant has Active SAM registration with current information, and maintains an active SAM registration annually.', 'orr_service@abtglobal.com'),
('HUD', '2. Applicant has Valid UEI (Unique Entity Identifier) Number.', 'orr_service@abtglobal.com'),
('HUD', '3. CoC Program Eligibility – Project applicants and potential subrecipients meet the eligibility requirements of the CoC Program as described in the Act and the Rule and provide evidence of eligibility required in the application (e.g., nonprofit documentation).', 'orr_service@abtglobal.com'),
('HUD', '4. Financial and Management Capacity:
- Project applicants and subrecipients demonstrate the financial and management capacity and experience to carry out the project as detailed in the project application
- Applicants must demonstrate the capacity to administer federal funds.', 'orr_service@abtglobal.com'),
('HUD', '5. Certifications – Project applicants submit the required certifications specified in the NOFO.', 'orr_service@abtglobal.com'),
('HUD', '6. Population Served – The population to be served meets program eligibility requirements as described in the Act, the Rule, and the NOFO.', 'orr_service@abtglobal.com'),
('HUD', '7. HMIS Participation –
- Project applicants (except those only receiving CoC planning or UFA Costs) agree to participate in a local HMIS system.
- Victim service providers must not disclose any personally identifying client info in HMIS.
- Victim service providers must use a comparable database meeting HMIS requirements.', 'orr_service@abtglobal.com'),
('HUD', '8. Applicant has no Outstanding Delinquent Federal Debts –
It is HUD policy that applicants with delinquent federal debt are not eligible unless:
   a) A negotiated repayment schedule is established and not delinquent, or
   b) Other arrangements satisfactory to HUD are made before the award of funds.', 'orr_service@abtglobal.com'),
('HUD', '9. Applicant has no Debarments and/or Suspensions –
In accordance with 2 CFR 2424, no federal funds may be awarded to debarred or suspended applicants.', 'orr_service@abtglobal.com'),
('HUD', '10. Pre-selection Review of Performance –
If your organization has delinquent federal debt or is excluded from doing business with the Federal government, HUD may:
   a) Deny funding or consider suspension/termination for cause;
   b) Require removal of key individuals from project roles;
   c) Change payment or reporting terms.

HUD reviews OMB-designated sources such as:
- Federal Awardee Performance and Integrity Information System (FAPIIS)
- The \"Do Not Pay\" website.', 'orr_service@abtglobal.com'),
('HUD', '11. Sufficiency of Financial Management System –
HUD will not award funds to applicants lacking a compliant financial management system per 2 CFR 200.302.
HUD may conduct surveys for:
- New applicants without federal award history
- Applicants flagged as high risk due to past performance or financial findings.', 'orr_service@abtglobal.com'),
('HUD', '12. False Statements –
A false statement in an application may result in:
- Denial or termination of award
- Criminal, civil, and/or administrative sanctions
- Fines, penalties, and imprisonment
Applicants confirm all statements are truthful.', 'orr_service@abtglobal.com'),
('HUD', '13. Mandatory Disclosure Requirement –
Recipients or applicants must disclose, in writing, to HUD:
- Any violations of Federal criminal law involving fraud, bribery, or gratuity affecting the award
- Disclosures must occur within 10 days of learning of the violation
Recipients are also required to:
- Report proceedings via SAM, per Appendix XII to 2 CFR part 200
- Comply with 2 CFR part 180, 31 U.S.C. 3321, and 31 U.S.C. 2313
Failure to disclose may lead to suspension, debarment, or other remedies in §200.338.', 'orr_service@abtglobal.com'),
('HUD', '14. Prohibition Against Lobbying Activities –
Applicants must comply with:
- The Byrd Amendment (31 U.S.C. 1352)
- 24 CFR part 87

Applicants must:
- Submit a signed Certification Regarding Lobbying
- Disclose non-federal lobbying efforts via SFLLL (Standard Form for Lobbying)

Federally recognized Indian tribes and TDHEs (created via tribal sovereignty) are exempt from the Byrd Amendment.
State-recognized tribes and TDHEs under state law must comply.', 'orr_service@abtglobal.com'),
('HUD', '15. Equal Participation of Faith-Based Organizations –
All projects must comply with 24 CFR 5.109, updated by HUD (81 FR 19355) on April 4, 2016, to reflect E.O. 13559:
\"Fundamental Principles and Policymaking Criteria for Partnerships with Faith-Based and Other Neighborhood Organizations\"
Applies to all HUD programs and activities unless otherwise exempted by program statutes or regulations.', 'orr_service@abtglobal.com'),
('HUD', '16. Resolution of Civil Rights Matters –
Applicants with unresolved civil rights matters as of the submission deadline:
- Will be deemed ineligible
- Will not be reviewed, rated, ranked, or funded', 'orr_service@abtglobal.com');
")

message("about to create rating_Factors")
DBI::dbExecute(DB_POOL, glue::glue("
CREATE TABLE IF NOT EXISTS rating_factors (
    rating_factor_id {id_var_attrs},
    rating_factor_text TEXT,
    rating_factor_text_short TEXT,
    piping_text VARCHAR(200), -- to be concatenated with goal for piping into rating scores tab
    funding_action SMALLINT REFERENCES lookups(reference_id),
    project_type SMALLINT NULL REFERENCES lookups(reference_id),
    target_population SMALLINT NULL REFERENCES lookups(reference_id),
    factor_group SMALLINT REFERENCES factor_groups(factor_group_id), 
    factor_subgroup SMALLINT REFERENCES factor_subgroups(factor_subgroup_id),
    goal VARCHAR(10) NULL, -- text of the goal, e.g. '30 days' or '90%' or 'Yes',
    max_point_value NUMERIC(4, 1),
    coc_version_id SMALLINT NULL REFERENCES coc_versions(coc_version_id), -- only filled out for custom factors
	  date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

DBI::dbExecute(DB_POOL, "
CREATE UNIQUE INDEX IF NOT EXISTS rating_factors_unique_idx 
ON rating_factors (
  coc_version_id,
  COALESCE(project_type, -1),
  COALESCE(target_population, -1),
  rating_factor_text
);
")

message("about to populate rating_Factors")
DBI::dbExecute(DB_POOL, "
-- Use CTEs for all lookup IDs and factor group/subgroup IDs
WITH
    l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
    l_renew AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'Renew'),

    l_rrh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'RRH'),
    l_psh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'PSH'),
    l_th AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'TH'),
    l_th_rrh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'TH+RRH'),

    l_dv AS (SELECT reference_id FROM lookups WHERE reference_type = 'target_population' AND value = 'DV'),
    l_general AS (SELECT reference_id FROM lookups WHERE reference_type = 'target_population' AND value = 'General'), -- Assuming this is used for the second population in pairs

    fg_perf_meas_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Performance Measures' AND funding_action = (SELECT reference_id FROM l_renew)),
    fg_high_needs_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Serve High Needs Populations' AND funding_action = (SELECT reference_id FROM l_renew)),
    fg_proj_effect_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Project Effectiveness' AND funding_action = (SELECT reference_id FROM l_renew)),
    -- fg_equity_factors_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Equity Factors' AND funding_action = (SELECT reference_id FROM l_renew)),
    fg_other_local_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Other and Local Criteria' AND funding_action = (SELECT reference_id FROM l_renew)),
    fg_experience_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Experience' AND funding_action = (SELECT reference_id FROM l_new)),
    fg_design_housing_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Design of Housing & Supportive Services' AND funding_action = (SELECT reference_id FROM l_new)),
    fg_timeliness_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Timeliness' AND funding_action = (SELECT reference_id FROM l_new)),
    fg_financial_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Financial' AND funding_action = (SELECT reference_id FROM l_new)),
    fg_proj_effect_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Project Effectiveness' AND funding_action = (SELECT reference_id FROM l_new)),
    -- fg_equity_factors_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Equity Factors' AND funding_action = (SELECT reference_id FROM l_new)),
    fg_other_local_new AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Other and Local Criteria' AND funding_action = (SELECT reference_id FROM l_new)),

    fsg_los_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Length of Stay' AND factor_group = (SELECT factor_group_id FROM fg_perf_meas_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_exits_ph_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Exits to Permanent Housing' AND factor_group = (SELECT factor_group_id FROM fg_perf_meas_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_returns_homeless_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Returns to Homelessness (if data is available for project)' AND factor_group = (SELECT factor_group_id FROM fg_perf_meas_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_new_inc_income_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'New or Increased Income and Earned Income' AND factor_group = (SELECT factor_group_id FROM fg_perf_meas_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_coord_assess_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Coordinated Assessment Score' AND factor_group = (SELECT factor_group_id FROM fg_high_needs_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_proj_chron_homeless_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Project Focuses on Chronically Homeless People' AND factor_group = (SELECT factor_group_id FROM fg_high_needs_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    fsg_apr_data_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'APR Data on ≥ 50% Disability/Zero Income/Unsheltered' AND factor_group = (SELECT factor_group_id FROM fg_high_needs_renew) AND funding_action = (SELECT reference_id FROM l_renew))
    -- fsg_agency_leadership_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Agency Leadership, Governance, and Policies' AND factor_group = (SELECT factor_group_id FROM fg_equity_factors_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    -- fsg_prog_part_outcomes_renew AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Program Participant Outcomes' AND factor_group = (SELECT factor_group_id FROM fg_equity_factors_renew) AND funding_action = (SELECT reference_id FROM l_renew)),
    -- fsg_agency_leadership_new AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Agency Leadership, Governance, and Policies' AND factor_group = (SELECT factor_group_id FROM fg_equity_factors_new) AND funding_action = (SELECT reference_id FROM l_new)),
    -- fsg_prog_part_outcomes_new AS (SELECT factor_subgroup_id FROM factor_subgroups WHERE factor_subgroup = 'Program Participant Outcomes' AND factor_group = (SELECT factor_group_id FROM fg_equity_factors_new) AND funding_action = (SELECT reference_id FROM l_new))

INSERT INTO rating_factors 
(rating_factor_text, rating_factor_text_short, piping_text, funding_action, project_type, target_population, factor_group, factor_subgroup, goal, max_point_value, created_by) 
VALUES
-- RENEWAL RATING FACTORS
-- Performance Measures (factor_group = fg_perf_meas_renew)
-- Subgroup: Length of Stay (fsg_los_renew)
('On average, participants spend XX days from project entry to residential move-in', 'Rapid Re-Housing', 'On average, participants are placed in housing <<goal>> after referral to RRH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '15 days', 30, 'orr_service@abtglobal.com'),
('On average, participants spend XX days from project entry to residential move-in', 'Rapid Re-Housing', 'On average, participants are placed in housing <<goal>> after referral to RRH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '30 days', 30, 'orr_service@abtglobal.com'),
('On average, participants spend XX days from project entry to residential move-in', 'Permanent Supportive-Housing', 'On average, participants are placed in housing <<goal>> after referral to PSH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '15 days', 30, 'orr_service@abtglobal.com'),
('On average, participants spend XX days from project entry to residential move-in', 'Permanent Supportive-Housing', 'On average, participants are placed in housing <<goal>> after referral to PSH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '15 days', 30, 'orr_service@abtglobal.com'),
('On average, participants stay in project XX days', 'Transitional Housing', 'On average, participants stay in project <<goal>>', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '180 days', 20, 'orr_service@abtglobal.com'),
('On average, participants stay in project XX days', 'Transitional Housing', 'On average, participants stay in project <<goal>>', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '180 days', 20, 'orr_service@abtglobal.com'),
('On average, participants stay in project XX days', 'TH+RRH - Transitional Housing Component', 'On average, participants stay in project <<goal>>', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '180 days', 10, 'orr_service@abtglobal.com'),
('On average, participants stay in project XX days', 'TH+RRH - Transitional Housing Component', 'On average, participants stay in project <<goal>>', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '180 days', 10, 'orr_service@abtglobal.com'),
('On average, participants spend XX days from project entry to residential move-in', 'TH+RRH - Rapid Re-Housing Component', 'On average, participants are placed in housing <<goal>> after referral to RRH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '15 days', 10, 'orr_service@abtglobal.com'),
('On average, participants spend XX days from project entry to residential move-in', 'TH+RRH - Rapid Re-Housing Component', 'On average, participants are placed in housing <<goal>> after referral to RRH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_los_renew), '15 days', 10, 'orr_service@abtglobal.com'),

-- Subgroup: Exits to Permanent Housing (fsg_exits_ph_renew)
('Minimum percent move to permanent housing', 'Rapid Re-Housing', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent move to permanent housing', 'Rapid Re-Housing', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '80 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent remain in or move to permanent housing', 'Permanent Supportive-Housing', '<<goal>> remain in or move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent remain in or move to permanent housing', 'Permanent Supportive-Housing', '<<goal>> remain in or move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent move to permanent housing', 'Transitional Housing', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent move to permanent housing', 'Transitional Housing', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent move to permanent housing', 'TH+RRH - Transitional Housing Component', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),
('Minimum percent move to permanent housing', 'TH+RRH - Transitional Housing Component', '<<goal>> move to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_exits_ph_renew), '90 %', 25, 'orr_service@abtglobal.com'),

-- Subgroup: Returns to Homelessness (if data is available for project) (fsg_returns_homeless_renew)
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '10 %', 15, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '20 %', 10, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '10 %', 15, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '20 %', 10, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '10 %', 15, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '20 %', 10, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '10 %', 15, 'orr_service@abtglobal.com'),
('Maximum percent of participants return to homelessness within 12 months of exit to permanent housing', 'Within 12 months of exit to permanent housing', '≤ <<goal>> of participants return to homelessness within 12 months of exit to PH', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_returns_homeless_renew), '20 %', 10, 'orr_service@abtglobal.com'),

-- Subgroup: New or Increased Income and Earned Income (fsg_new_inc_income_renew)
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project stayers', 'Earned income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '8 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>> of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project stayers', 'Non-employment income for project stayers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '10 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased earned income for project leavers', 'Earned income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '15 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),
('Minimum percent of participants with new or increased non-employment income for project leavers', 'Non-employment income for project leavers', '<<goal>>+ of participants with new or increased income', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT factor_subgroup_id FROM fsg_new_inc_income_renew), '25 %', 2.5, 'orr_service@abtglobal.com'),

-- Serve High Needs Populations (factor_group = fg_high_needs_renew)
-- Subgroup: Coordinated Assessment score (fsg_coord_assess_renew)
('Assessment score for XX% of participants indicates RRH or more intensive intervention', 'Rapid Re-Housing', 'Assessment score for <<goal>> of participants indicates RRH or more intensive intervention', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('Assessment score for XX% of participants indicates RRH or more intensive intervention', 'Rapid Re-Housing', 'Assessment score for <<goal>> of participants indicates RRH or more intensive intervention', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('Assessment score for participants indicates PSH with XX% at highest end of PSH range', 'Permanent Supportive-Housing', 'Assessment score for participants indicates PSH with <<goal>> at highest end of PSH range', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('Assessment score for participants indicates PSH with XX% at highest end of PSH range', 'Permanent Supportive-Housing', 'Assessment score for participants indicates PSH with <<goal>> at highest end of PSH range', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participant meet CoC''s TH targeting criteria', 'Transitional Housing', '<<goal>> of participants meet CoC’s TH targeting criteria', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participant meet CoC''s TH targeting criteria', 'Transitional Housing', '<<goal>> of participants meet CoC’s TH targeting criteria', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('Assessment score for XX% of participants indicates RRH or more intensive intervention', 'TH+RRH - Transitional Housing Component', 'Assessment score for <<goal>> of participants indicates RRH or more intensive intervention', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('Assessment score for XX% of participants indicates RRH or more intensive intervention', 'TH+RRH - Transitional Housing Component', 'Assessment score for <<goal>> of participants indicates RRH or more intensive intervention', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_coord_assess_renew), '95 %', 20, 'orr_service@abtglobal.com'),

-- Subgroup: Project focuses on chronically homeless people (fsg_proj_chron_homeless_renew)
('XX% of participants are chronically homeless', 'Rapid Re-Housing', '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', 'Rapid Re-Housing', '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', 'Permanent Supportive-Housing', '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', 'Permanent Supportive-Housing', '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', NULL, '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', NULL, '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', NULL, '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),
('XX% of participants are chronically homeless', NULL, '≥ <<goal>> of participants are chronically homeless', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_proj_chron_homeless_renew), '95 %', 20, 'orr_service@abtglobal.com'),

-- Subgroup: APR data on ≥ 50% disability/zero income/unsheltered (fsg_apr_data_renew)
('Minimum percent of participants with zero income at entry', 'Rapid Re-Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'Rapid Re-Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Rapid Re-Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Rapid Re-Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Rapid Re-Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Rapid Re-Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'Permanent Supportive-Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '80 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'Permanent Supportive-Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '80 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Permanent Supportive-Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '75 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Permanent Supportive-Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '75 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Permanent Supportive-Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '75 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Permanent Supportive-Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '75 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'Transitional Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'Transitional Housing', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Transitional Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'Transitional Housing', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Transitional Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'Transitional Housing', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with zero income at entry', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants with zero income at entry', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants with more than one disability', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants with more than one disability type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),
('Minimum percent of participants entering project from place not meant for human habitation', 'TH+RRH - Transitional Housing Component', '≥ <<goal>> of participants entering project from place not meant for human habitation', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT factor_subgroup_id FROM fsg_apr_data_renew), '50 %', 10, 'orr_service@abtglobal.com'),

-- Project Effectiveness (factor_group = fg_proj_effect_renew, factor_subgroup = NULL)
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 20, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 20, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 20, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 20, 'orr_service@abtglobal.com'),
('Costs are within local average cost per positive housing exit for project type', 'Project has reasonable costs', 'Costs are within local average cost per positive housing exit for project type', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries to project from CE referral (or alternative system for DV projects)', 'Coordinated Entry Participation', '≥ <<goal>> of entries to project from CE referrals', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, '95 %', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Housing First and/or Low Barrier Implementation - CoC assessment of fidelity to Housing First from CoC monitoring or review of project policies and procedures', 'Housing First and/or Low Barrier Implementation', 'Commits to applying Housing First model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),

-- Equity Factors, Governance, and Policies (factor_group = fg_equity_factors_renew)
-- Subgroup: Agency Leadership, Governance, and Policies (fsg_agency_leadership_renew)
-- ('Recipient has under-represented individuals (BIPOC, LGBTQ+, etc) in managerial and leadership positions', 'Recipient Management & Leadership Positions', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_agency_leadership_renew), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('Recipient''s board of directors includes representation from more than one person with lived experience of homelessness', 'Recipient Board of Directors', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_agency_leadership_renew), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('Recipient has relational process for receiving and incorporating feedback from persons with lived experience of homelessness', 'Process for receiving & incorporating feedback', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_agency_leadership_renew), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('Recipient has reviewed internal policies and procedures with an equity lens and has a plan for developing and implementing equitable policies that do not impose undue barriers', 'Internal Policies and Procedures', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_agency_leadership_renew), 'Yes', 10, 'orr_service@abtglobal.com'),

-- Program Participant Outcomes (factor_group = fg_equity_factors_renew, subgroup = fsg_prog_part_outcomes_renew)
-- ('Recipient has reviewed program participant outcomes with an equity lens, including the disaggregation of data by race, ethnicity, gender identity, age, and/or other underserved populations', 'Outcomes with an equity lens', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_renew), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('Recipient has identified programmatic changes needed to make program participant outcomes more equitable and developed a plan to make those changes', 'Program changes for equitable outcomes', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_renew), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('Recipient is working with HMIS lead to develop a schedule for reviewing and/or other underserved populations', 'HMIS data review with equity lens', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_renew), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_renew), 'Yes', 10, 'orr_service@abtglobal.com'),

-- Other and Local Criteria (factor_group = fg_other_local_renew, factor_subgroup = NULL)
('Applicant Narrative that CoC Scores', 'Applicant Narrative', 'Project is operating in conformance to CoC standards', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_other_local_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('CoC Monitoring Score', 'CoC Monitoring Score', 'Project is operating in conformance to CoC standards', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_other_local_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),

-- NEW RATING FACTORS
-- Experience (factor_group = fg_experience_new)
('A. Describe the experience of the applicant and sub-recipients (if any) in working with the proposed population and in providing housing similar to that proposed in the application.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('A. Describe the experience of the applicant and sub-recipients (if any) in working with the proposed population and in providing housing similar to that proposed in the application.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('B. Describe experience with utilizing a Housing First approach. This must include:
- Eligibility criteria
- Process for accepting new clients
- Process and criteria for exiting clients
- Demonstration of no preconditions to entry, allowing entry regardless of current or past substance abuse, income, criminal records (with exceptions of restrictions imposed by federal, state, or local law or ordinance), marital status, familial status, self-disclosed or perceived sexual orientation, gender identity or gender expression.
- Demonstration that the project has a process to address situations that may jeopardize housing or project assistance to ensure that project participation is terminated in only the most severe cases.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),

('B. Describe experience with utilizing a Housing First approach. This must include:
- Eligibility criteria
- Process for accepting new clients
- Process and criteria for exiting clients
- Demonstration of no preconditions to entry, allowing entry regardless of current or past substance abuse, income, criminal records (with exceptions of restrictions imposed by federal, state, or local law or ordinance), marital status, familial status, self-disclosed or perceived sexual orientation, gender identity or gender expression.
- Demonstration that the project has a process to address situations that may jeopardize housing or project assistance to ensure that project participation is terminated in only the most severe cases.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('C. Describe experience in effectively utilizing federal funds including HUD grants and other public funding, including satisfactory drawdowns and performance for existing grants as evidenced by timely reimbursement of subrecipients (if applicable), regular drawdowns, timely resolution of monitoring findings, and timely submission of required reporting on existing grants.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('C. Describe experience in effectively utilizing federal funds including HUD grants and other public funding, including satisfactory drawdowns and performance for existing grants as evidenced by timely reimbursement of subrecipients (if applicable), regular drawdowns, timely resolution of monitoring findings, and timely submission of required reporting on existing grants.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),

-- Design of Housing & Supportive Services (factor_group = fg_design_housing_new)
('A. Extent to which the applicant 1) Demonstrates understanding of the needs of the clients to be served. 2) Demonstrates that type, scale, and location of the housing fit the needs of the clients to be served. 3) Demonstrates that type and scale of the all supportive services, regardless of funding source, meets the needs of clients to be served. 4) Demonstrates how clients will be assisted in obtaining mainstream benefits. 5) Establishes performances measures for housing and income that are objective, measurable, trackable and meet or exceed any established HUD or CoC benchmarks.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('A. Extent to which the applicant 1) Demonstrates understanding of the needs of the clients to be served. 2) Demonstrates that type, scale, and location of the housing fit the needs of the clients to be served. 3) Demonstrates that type and scale of the all supportive services, regardless of funding source, meets the needs of clients to be served. 4) Demonstrates how clients will be assisted in obtaining mainstream benefits. 5) Establishes performances measures for housing and income that are objective, measurable, trackable and meet or exceed any established HUD or CoC benchmarks.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('B. Describe the plan to assist clients to rapidly secure and maintain permanent housing that is safe, affordable, accessible, and acceptable to their needs.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B. Describe the plan to assist clients to rapidly secure and maintain permanent housing that is safe, affordable, accessible, and acceptable to their needs.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('C. Describe how clients will be assisted to increase employment and/or income and to maximize their ability to live independently.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('C. Describe how clients will be assisted to increase employment and/or income and to maximize their ability to live independently.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('D. Project leverages housing resources with housing subsidies or units not funded through the CoC or ESG programs.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('D. Project leverages housing resources with housing subsidies or units not funded through the CoC or ESG programs.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('E. Project leverages health resources, including a partnership commitment with a healthcare organization.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('E. Project leverages health resources, including a partnership commitment with a healthcare organization.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_design_housing_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),

-- Timeliness (factor_group = fg_timeliness_new)
('A. Describe plan for rapid implementation of the program, documenting how the project will be ready to begin housing the first program participant. Provide a detailed schedule of proposed activities for 60 days, 120 days, and 180 days after grant award.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_timeliness_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('A. Describe plan for rapid implementation of the program, documenting how the project will be ready to begin housing the first program participant. Provide a detailed schedule of proposed activities for 60 days, 120 days, and 180 days after grant award.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_timeliness_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),

-- Financial (factor_group = fg_financial_new)
('A. Project is cost-effective when projected cost per person served is compared to CoC average within project type.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('A. Project is cost-effective when projected cost per person served is compared to CoC average within project type.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B1. Organization''s most recent audit: Found no exceptions to standard practices', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B1. Organization''s most recent audit: Found no exceptions to standard practices', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B2. Organization''s most recent audit: Identified agency as ''low risk''', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B2. Organization''s most recent audit: Identified agency as ''low risk''', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B3. Organization''s most recent audit: Indicates no findings', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('B3. Organization''s most recent audit: Indicates no findings', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('C. Documented match amount meets HUD requirements.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('C. Documented match amount meets HUD requirements.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 5, 'orr_service@abtglobal.com'),
('D. Budgeted costs are reasonable, allocable, and allowable.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 20, 'orr_service@abtglobal.com'),
('D. Budgeted costs are reasonable, allocable, and allowable.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_financial_new), NULL, NULL, 20, 'orr_service@abtglobal.com'),

-- Project Effectiveness (factor_group = fg_proj_effect_new)
('Coordinated Entry Participation- Minimum percent of entries projected to come from CE referrals', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_new), NULL, '95 %', 5, 'orr_service@abtglobal.com'),
('Coordinated Entry Participation- Minimum percent of entries projected to come from CE referrals', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_new), NULL, '95 %', 5, 'orr_service@abtglobal.com');

-- Equity Factors (factor_group = fg_equity_factors_new)
-- Subgroup: Agency Leadership, Governance, and Policies (fsg_agency_leadership_new)
-- ('New project has under-represented individuals (BIPOC, LGBTQ+, etc) in managerial and leadership positions', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_agency_leadership_new), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('New project''s organizational board of directors includes representation from more than one person with lived experience (per 578.75(g))', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_agency_leadership_new), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('New project has relational process for receiving and incorporating feedback from persons with lived experience or a plan to create one', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_agency_leadership_new), 'Yes', 10, 'orr_service@abtglobal.com'),
-- ('New project has reviewed internal policies and procedures with an equity lens and has a plan for developing and implementing equitable policies that do not impose undue barriers that exacerbate disparities and outcomes', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_agency_leadership_new), 'Yes', 10, 'orr_service@abtglobal.com'),

-- Program Participant Outcomes (factor_group = fg_equity_factors_new, subgroup = fsg_prog_part_outcomes_new)
-- ('New project describes their plan for reviewing program participant outcomes with an equity lens, including the disaggregation of data by race, ethnicity, gender identity, and/or age. If already implementing a plan, describe findings from outcomes review', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_new), NULL, 10, 'orr_service@abtglobal.com'),
-- ('New project describes plan to review whether programmatic changes are needed to make program participant outcomes more equitable and developed a plan to make those changes. If already implementing plan, describe findings from review', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_new), NULL, 10, 'orr_service@abtglobal.com'),
-- ('New project describes plan to work with HMIS lead to develop a schedule for reviewing HMIS data with disaggregation by race, ethnicity, gender identity, and/or age. If already implementing plan, describe findings from review', NULL, (SELECT reference_id FROM l_new), NULL, NULL, (SELECT factor_group_id FROM fg_equity_factors_new), (SELECT factor_subgroup_id FROM fsg_prog_part_outcomes_new), NULL, 10, 'orr_service@abtglobal.com');
")

###### FUNDING PRIORITIES #########
drop_table("coc_nofo_opportunities")
DBI::dbExecute(DB_POOL, glue::glue("
--- This is the set of checkboxes in the middle of the Funding Priorities tab
CREATE TABLE IF NOT EXISTS coc_nofo_opportunities (
    coc_nofo_opportunity_id {id_var_attrs},
    bonus_type SMALLINT REFERENCES lookups(reference_id),
    funding_action SMALLINT REFERENCES lookups(reference_id),
    project_type SMALLINT REFERENCES lookups(reference_id),
    target_population SMALLINT REFERENCES lookups(reference_id),
    population_group SMALLINT REFERENCES lookups(reference_id),
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

message("now doing CTEs")

DBI::dbExecute(DB_POOL, "
-- Use CTEs for all lookup IDs
WITH
    l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
    l_coc_bonus AS (SELECT reference_id FROM lookups WHERE reference_type = 'bonus_type' AND value = 'CoC Bonus'),
    l_dv_bonus AS (SELECT reference_id FROM lookups WHERE reference_type = 'bonus_type' AND value = 'DV Bonus'),
    l_psh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'PSH'),
    l_rrh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'RRH'),
    l_th_rrh AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'TH+RRH'),
    l_hmis AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'HMIS Project'),
    l_sso_ce AS (SELECT reference_id FROM lookups WHERE reference_type = 'project_type' AND value = 'SSO-CE'),
    l_vet AS (SELECT reference_id FROM lookups WHERE reference_type = 'target_population' AND value = 'Vet'), -- used for dedicated PLUS/CH
    l_ch AS (SELECT reference_id FROM lookups WHERE reference_type = 'target_population' AND value = 'CH'), -- The original comment suggests CH, but the number mapping was Vet. Correcting to CH.
    l_ind AS (SELECT reference_id FROM lookups WHERE reference_type = 'population_group' AND value_abbrev = 'Ind'),
    l_fam AS (SELECT reference_id FROM lookups WHERE reference_type = 'population_group' AND value_abbrev = 'Fam')
INSERT INTO coc_nofo_opportunities (bonus_type, funding_action, project_type, target_population, population_group, created_by)
VALUES 
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_ch), (SELECT reference_id FROM l_ind), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_ch), (SELECT reference_id FROM l_fam), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_rrh), NULL, (SELECT reference_id FROM l_ind), 'orr_service@abtglobal.com'), 
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_rrh), NULL, (SELECT reference_id FROM l_fam), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_th_rrh), NULL, (SELECT reference_id FROM l_fam), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_th_rrh), NULL, (SELECT reference_id FROM l_ind), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_hmis), NULL, NULL, 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_coc_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_sso_ce), NULL, NULL, 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_dv_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_rrh), NULL, (SELECT reference_id FROM l_ind), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_dv_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_rrh), NULL, (SELECT reference_id FROM l_fam), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_dv_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_th_rrh), NULL, (SELECT reference_id FROM l_ind), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_dv_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_th_rrh), NULL, (SELECT reference_id FROM l_fam), 'orr_service@abtglobal.com'),
((SELECT reference_id FROM l_dv_bonus), (SELECT reference_id FROM l_new), (SELECT reference_id FROM l_sso_ce), NULL, NULL, 'orr_service@abtglobal.com');
")

#####################
# USER-ENTERED DATA
#####################
#-- Start from the most dependent tables
drop_table("projects")
drop_table("coc_funding_priorities")
drop_table("selected_coc_nofo_opportunities")
drop_table("selected_rating_factors")
drop_table("selected_thresholds")
drop_table("ranking")
drop_table("rating_scores")
drop_table("threshold_entries")
drop_table("project_evaluations")

###### INVENTORY #########
DBI::dbExecute(DB_POOL, glue::glue("
--- Projects (User can import HIC data or select their CoC from the HIC Data we have at that time)
-- Ideally, there would be one source of truth in the HIC data, but the timing and SecOps doesn't allow that
CREATE TABLE IF NOT EXISTS projects (
    project_id {id_var_attrs},
    coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id),
    organization_name VARCHAR,
    project_name VARCHAR,
    project_type SMALLINT REFERENCES lookups(reference_id),
    target_population SMALLINT REFERENCES lookups(reference_id),
    mckinneyvento BOOLEAN,
    mckinneyventoyhdp BOOLEAN,
    dv_renewal BOOLEAN,
    grant_number VARCHAR(15) NULL,
    coc_amount_awarded_last_year NUMERIC(11, 2) NULL,
    coc_amount_expended_last_year NUMERIC(11, 2) NULL,
    coc_funding_requested NUMERIC(11, 2),
    funding_action SMALLINT REFERENCES lookups(reference_id),
    geocode VARCHAR(10),
    all_fam_beds INTEGER,
    dv_fam_beds INTEGER,
    ch_fam_beds INTEGER,
    vet_fam_beds INTEGER,
    par_youth_beds INTEGER,
	beds_hh_wo_children INTEGER NULL,
	beds_hh_w_only_children INTEGER NULL,
    all_ind_beds INTEGER,
    dv_ind_beds INTEGER,
	ch_beds_hh_wo_children INTEGER NULL,
	ch_beds_hh_w_only_children INTEGER NULL,
    total_ch_ind_beds INTEGER,
    vet_ind_beds INTEGER,
    single_youth_beds INTEGER,
    is_dedicated_ch_fam BOOLEAN,
    is_dedicated_ch_ind BOOLEAN,
    is_dedicated_dv BOOLEAN,
    amount_other_public_funding NUMERIC(11, 2),
    amount_private_funding NUMERIC(11, 2),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

##### FUNDING PRIORITIES ########
DBI::dbExecute(DB_POOL, glue::glue("
--- Funding Priorities by Project Type and Population
---- This is the table of population and project types at the bottom of the Funding Priorities tab
--- when a new CoC version is created, the system should generate a set of these
--- each row corresponds to a ProjectType + TargetPopulation combo. It may not have any priorities
CREATE TABLE IF NOT EXISTS coc_funding_priorities (
    coc_funding_priority_id {id_var_attrs},
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id),
    project_type SMALLINT REFERENCES lookups(reference_id),
    target_population SMALLINT REFERENCES lookups(reference_id),
	population_group SMALLINT REFERENCES lookups(reference_id),
    beds INTEGER NULL,
    funding NUMERIC(11, 2) NULL,
    priority SMALLINT NULL REFERENCES lookups(reference_id),
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),

CONSTRAINT coc_funding_priorities_unique_key UNIQUE (coc_version_id, project_type, target_population, population_group)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
--- User-Selected NOFO Opportunities
CREATE TABLE IF NOT EXISTS selected_coc_nofo_opportunities (
    selected_coc_nofo_opportunity_id {id_var_attrs},
    coc_nofo_opportunity_id SMALLINT REFERENCES coc_nofo_opportunities(coc_nofo_opportunity_id),
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),
    
    CONSTRAINT coc_nofo_opportunities_unique_key UNIQUE (coc_version_id, coc_nofo_opportunity_id)
);
"))

#### CUSTOMIZED RATING CRITERIA ####
DBI::dbExecute(DB_POOL, glue::glue("
--- User-Selected Rating Factors
CREATE TABLE IF NOT EXISTS selected_rating_factors (
    selected_rating_factor_id {id_var_attrs},
    rating_factor_id SMALLINT REFERENCES rating_factors(rating_factor_id),
    goal VARCHAR(100) NULL, -- text of the goal, e.g. '30 days' or '90%' or 'Yes'
    max_point_value NUMERIC(4, 1),
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),

	-- a CoC Profile cannot have more than one of a given selected rating factor
	CONSTRAINT uq_selected_rating_factors_profile UNIQUE (coc_version_id, rating_factor_id)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
--- User-Selected Threshold Factors
CREATE TABLE IF NOT EXISTS selected_thresholds (
    selected_threshold_id {id_var_attrs},
    threshold_id SMALLINT REFERENCES thresholds(threshold_id),
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),

	-- a CoC Profile cannot have more than one of a given selected rating factor
    CONSTRAINT uq_selected_thresholds_profile UNIQUE (coc_version_id, threshold_id)
);
"))

#### RATING ####
DBI::dbExecute(DB_POOL, glue::glue("
--- Rating_Scores
CREATE TABLE IF NOT EXISTS rating_scores (
    rating_score_id {id_var_attrs},
    project_id INTEGER REFERENCES projects(project_id),
    selected_rating_factor_id SMALLINT NULL REFERENCES selected_rating_factors(selected_rating_factor_id), --can be null if they rate outside the app
    rating_score INTEGER,
    performance VARCHAR(5) NULL,
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),
    
  CONSTRAINT uq_rating_scores_profile UNIQUE (project_id, selected_rating_factor_id)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
--- Threshold_Entries
CREATE TABLE IF NOT EXISTS threshold_entries (
    threshold_entry_id {id_var_attrs},
    project_id INTEGER REFERENCES projects(project_id),
    threshold_id SMALLINT NULL REFERENCES thresholds(threshold_id),
    met_threshold BOOLEAN,
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username),
    
  
  CONSTRAINT uq_threshold_entries_profile UNIQUE (project_id, threshold_id)
);
"))

DBI::dbExecute(DB_POOL, glue::glue("
--- Project_Evaluations
CREATE TABLE IF NOT EXISTS project_evaluations (
    project_evaluation_id {id_var_attrs},
    project_id INTEGER NOT NULL UNIQUE REFERENCES projects(project_id) ON DELETE CASCADE,
    method VARCHAR(7) NULL CHECK (method IN ('in_app', 'outside')),
    met_hud_thresholds BOOLEAN NULL,
    met_coc_thresholds BOOLEAN NULL,

    weighted_score SMALLINT CHECK (weighted_score >= 0 AND weighted_score <= 100),

    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

#### RANKING ####
DBI::dbExecute(DB_POOL, glue::glue("
--- Ranking 
CREATE TABLE IF NOT EXISTS ranking (
    rank_id {id_var_attrs},
    project_id INTEGER REFERENCES projects(project_id),
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id),
    rank SMALLINT,
    coc_funding_recommendation NUMERIC(11, 2),
	date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username),
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username)
);
"))

#################
# CREATE INDEXES
###############
# -- Add an index on the reference_type for efficient lookups (e.g., getting all project types)
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_cocs_state ON cocs(state);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_giw_coc ON giw(coc);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_hud_ard_report_coc ON hud_ard_report(coc);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_all_hic_data_hudnum ON all_hic_data(hudnum);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_all_hic_data_project_type ON all_hic_data(project_type);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_versions_coc ON coc_versions(coc);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_versions_coc_status ON coc_versions(coc_status);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_versions_created_by ON coc_versions(created_by);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_version_requests_version_id ON coc_version_requests(coc_version_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_version_requests_status ON coc_version_requests(request_status);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_version_users_username ON coc_version_users(username);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_version_users_role ON coc_version_users(coc_version_role);")

# -- Definition / 'Hardcoded' Tables
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_factor_groups_funding_action ON factor_groups(funding_action);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_factor_subgroups_factor_group ON factor_subgroups(factor_group);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_factor_subgroups_funding_action ON factor_subgroups(funding_action);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_factors_funding_action ON rating_factors(funding_action);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_factors_project_type ON rating_factors(project_type);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_factors_target_population ON rating_factors(target_population);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_factors_factor_group ON rating_factors(factor_group);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_factors_factor_subgroup ON rating_factors(factor_subgroup);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_bonus_type ON coc_nofo_opportunities(bonus_type);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_funding_action ON coc_nofo_opportunities(funding_action);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_project_type ON coc_nofo_opportunities(project_type);")

# -- User-Entered Data Tables
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_projects_coc_version_id ON projects(coc_version_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_projects_project_type ON projects(project_type);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_projects_target_population ON projects(target_population);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_projects_funding_action ON projects(funding_action);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_funding_priorities_coc_version_id ON coc_funding_priorities(coc_version_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_coc_funding_priorities_project_type ON coc_funding_priorities(project_type);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_selected_coc_nofo_opportunities_coc_version_id ON selected_coc_nofo_opportunities(coc_version_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_scores_project_id ON rating_scores(project_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_scores_selected_rating_factor_id ON rating_scores(selected_rating_factor_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_threshold_entries_project_id ON threshold_entries(project_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_threshold_entries_threshold_id ON threshold_entries(threshold_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_ranking_project_id ON ranking(project_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_ranking_coc_version_id ON ranking(coc_version_id);")

#-- Composite Indexes for High-Frequency Lookups
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_rating_scores_project_factor ON rating_scores(project_id, selected_rating_factor_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_threshold_entries_project_threshold ON threshold_entries(project_id, threshold_id);")
DBI::dbExecute(DB_POOL, "CREATE INDEX IF NOT EXISTS idx_references_type ON lookups (reference_type);")

message("Done populating the db!")

if(Sys.getenv("RSTUDIO") == "1" || add_demo_data)
  source(here("sandbox/generate_test_data_for_demo.R"), local=TRUE)
}

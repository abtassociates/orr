-- ==============================================================================
-- 1. DROP EXISTING TABLES
-- ==============================================================================
DROP TABLE IF EXISTS users __CASCADE__;
DROP TABLE IF EXISTS lookups __CASCADE__;
DROP TABLE IF EXISTS all_hic_data __CASCADE__;
DROP TABLE IF EXISTS states __CASCADE__;
DROP TABLE IF EXISTS cocs __CASCADE__;
DROP TABLE IF EXISTS giw __CASCADE__;
DROP TABLE IF EXISTS hud_ard_report __CASCADE__;

-- ==============================================================================
-- 2. CREATE TABLES & INSERT STATIC DATA
-- ==============================================================================

-- USERS
CREATE TABLE users (
    username VARCHAR(100) PRIMARY KEY,
    firstname VARCHAR(255),
    lastname VARCHAR(255),
    role VARCHAR(20) NULL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

INSERT INTO users (username, firstname, lastname, created_by, role) VALUES 
  ('alex.silverman@abtglobal.com', 'Alex', 'Silverman', NULL, 'admin'),
  ('marschall.furman@abtglobal.com', 'Marschall', 'Furman', NULL, 'admin'),
  ('Victoria.Lopez@abtglobal.com', 'Victoria', 'Lopez', NULL, 'tester'),
  ('anthony.appau@abtglobal.com', 'Anthony', 'Appau', NULL, 'admin'),
  ('orr_service@abtglobal.com', 'ORR', 'Service Account', NULL, NULL),
  ('louise.rothschild@abtglobal.com', 'Louise', 'Rothschild', NULL, 'tester'),
  ('kally.canfield@abtglobal.com', 'Kally', 'Canfield', NULL, 'tester'),
  ('Randy.McCoy@abtglobal.com', 'Randy', 'McCoy', NULL, 'tester');

-- LOOKUPS
CREATE TABLE lookups (
    reference_id __PK_TYPE__,
    reference_type VARCHAR(100) NOT NULL,
    value VARCHAR(255) NOT NULL,
    value_abbrev VARCHAR(100) NULL,
    value_long VARCHAR(255) NULL,
    other_specify_flag BOOLEAN DEFAULT FALSE,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

INSERT INTO lookups (reference_type, value, created_by) VALUES
    ('request_status', 'Sent', 'orr_service@abtglobal.com'),
    ('request_status', 'Approved', 'orr_service@abtglobal.com'),
    ('request_status', 'Rejected', 'orr_service@abtglobal.com'),
    ('coc_version_role', 'Owner', 'orr_service@abtglobal.com'),
    ('coc_version_role', 'Editor', 'orr_service@abtglobal.com'),
    ('coc_status', 'Not Started', 'orr_service@abtglobal.com'),
    ('coc_status', 'Rating In Progress', 'orr_service@abtglobal.com'),
    ('coc_status', 'Rating Complete', 'orr_service@abtglobal.com'),
    ('coc_status', 'Complete', 'orr_service@abtglobal.com'),
    ('funding_action', 'New', 'orr_service@abtglobal.com'),
    ('funding_action', 'Renew', 'orr_service@abtglobal.com'),
    ('funding_action', 'Expand', 'orr_service@abtglobal.com'),
    ('funding_action', 'Reallocate', 'orr_service@abtglobal.com'),
    ('funding_action', 'Ignore', 'orr_service@abtglobal.com'),
    ('funding_action', 'Replace', 'orr_service@abtglobal.com'),
    ('bonus_type', 'CoC Bonus', 'orr_service@abtglobal.com'),
    ('bonus_type', 'DV Bonus', 'orr_service@abtglobal.com'),
    ('priority', 'Unspecified', 'orr_service@abtglobal.com'),
    ('priority', 'Low', 'orr_service@abtglobal.com'),
    ('priority', 'Medium', 'orr_service@abtglobal.com'),
    ('priority', 'High', 'orr_service@abtglobal.com');
    
INSERT INTO lookups (reference_type, value, other_specify_flag, created_by) VALUES
    ('request_rejection_reason', 'Not Associated with CoC', FALSE, 'orr_service@abtglobal.com'),
    ('request_rejection_reason', 'Other', TRUE, 'orr_service@abtglobal.com');

INSERT INTO lookups (reference_type, value, value_long, created_by) VALUES
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
    ('target_population', 'General', 'General', 'orr_service@abtglobal.com'),
    ('target_population', 'DV', 'Domestic Violence', 'orr_service@abtglobal.com'),
    ('target_population', 'CH', 'Chronically Homeless', 'orr_service@abtglobal.com'),
    ('target_population', 'Veteran', 'Veteran', 'orr_service@abtglobal.com'),
    ('target_population', 'Youth', 'Youth', 'orr_service@abtglobal.com'),
    ('target_population', 'HIV', 'Human Immunodeficiency Virus', 'orr_service@abtglobal.com'),
    ('target_population', 'NA', 'Not Applicable', 'orr_service@abtglobal.com');

INSERT INTO lookups (reference_type, value_abbrev, value, value_long, created_by) VALUES
    ('population_group', 'Ind', 'Individual', 'Individuals', 'orr_service@abtglobal.com'),
    ('population_group', 'Fam', 'Family', 'Families', 'orr_service@abtglobal.com');


INSERT INTO lookups (reference_type, value_abbrev, value, value_long, created_by) VALUES
    ('tier', 'Tier 1', 'Tier 1', 'Tier 1', 'orr_service@abtglobal.com'),
    ('tier', 'Tier 2', 'Tier 2', 'Tier 2', 'orr_service@abtglobal.com'),
    ('tier', 'Tier 3', 'Projects Exceeding ARD Adj', 'Projects Exceeding ARD Adj', 'orr_service@abtglobal.com'),
    ('tier', 'Tier 4', 'Excluded', 'Projects Not Selected for Funding', 'orr_service@abtglobal.com');

-- RAW DATA TABLES (Populated via CSV in R later)
CREATE TABLE all_hic_data (
    hic_data_id __PK_TYPE__,
    row_num INTEGER, 
    hudnum VARCHAR(6), 
    coc_name TEXT,
    organization_name VARCHAR,
    project_name VARCHAR,
    project_type SMALLINT REFERENCES lookups(reference_id), 
    geocode VARCHAR(10),
    target_population SMALLINT REFERENCES lookups(reference_id), 
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
    -- mckinneyventoyhdp BOOLEAN,
    -- mckinneyventoyhdprenewals BOOLEAN,
    mckinneyventounshelt BOOLEAN,
    mckinneyventorural BOOLEAN,
    beds_hh_w_children INTEGER,
    veteran_beds_hh_w_children INTEGER,
    youth_beds_hh_w_children INTEGER,
    ch_beds_hh_w_children INTEGER,
    beds_hh_wo_children INTEGER,
    veteran_beds_hh_wo_children INTEGER,
    youth_beds_hh_wo_children INTEGER,
    ch_beds_hh_wo_children INTEGER,
    beds_hh_w_only_children INTEGER,
    ch_beds_hh_w_only_children INTEGER,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_row_num_hudnum UNIQUE (row_num, hudnum)
);

CREATE TABLE states (
    state_code VARCHAR(2) PRIMARY KEY,
    state_name VARCHAR(100),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE cocs (
    coc_code VARCHAR(6) PRIMARY KEY,
    coc_name TEXT,
    state VARCHAR(2) REFERENCES states(state_code),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE giw (
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
    total_ara DECIMAL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE hud_ard_report (
    coc VARCHAR(6) REFERENCES cocs(coc_code) ON DELETE CASCADE,
    coc_number_and_name TEXT,
    pprn INTEGER,
    estimated NUMERIC(12, 2),
    tier_1 NUMERIC(12, 2),
    coc_bonus NUMERIC(12, 2) NULL,
    dv_bonus NUMERIC(12, 2),
    coc_planning INTEGER,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

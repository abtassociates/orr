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
DROP TABLE IF EXISTS coc_versions __CASCADE__;
DROP TABLE IF EXISTS coc_version_requests __CASCADE__;
DROP TABLE IF EXISTS coc_version_users __CASCADE__;
DROP TABLE IF EXISTS user_settings __CASCADE__;
DROP TABLE IF EXISTS factor_groups __CASCADE__;
DROP TABLE IF EXISTS factor_subgroups __CASCADE__;
DROP TABLE IF EXISTS thresholds __CASCADE__;
DROP TABLE IF EXISTS rating_factors __CASCADE__;
DROP TABLE IF EXISTS coc_nofo_opportunities __CASCADE__;
DROP TABLE IF EXISTS projects __CASCADE__;
DROP TABLE IF EXISTS coc_funding_priorities __CASCADE__;
DROP TABLE IF EXISTS selected_coc_nofo_opportunities __CASCADE__;
DROP TABLE IF EXISTS selected_rating_factors __CASCADE__;
DROP TABLE IF EXISTS selected_thresholds __CASCADE__;
DROP TABLE IF EXISTS rating_scores __CASCADE__;
DROP TABLE IF EXISTS threshold_entries __CASCADE__;
DROP TABLE IF EXISTS project_evaluations __CASCADE__;
DROP TABLE IF EXISTS ranking __CASCADE__;
DROP TABLE IF EXISTS user_presence __CASCADE__;

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
    ('coc_status', 'In Progress', 'orr_service@abtglobal.com'),
    ('coc_status', 'Completed', 'orr_service@abtglobal.com'),
    ('funding_action', 'New', 'orr_service@abtglobal.com'),
    ('funding_action', 'Renew', 'orr_service@abtglobal.com'),
    ('funding_action', 'Expand', 'orr_service@abtglobal.com'),
    ('funding_action', 'Reallocate', 'orr_service@abtglobal.com'),
    ('funding_action', 'Ignore', 'orr_service@abtglobal.com'),
    ('funding_action', 'Replace', 'orr_service@abtglobal.com'),
    ('bonus_type', 'CoC Bonus', 'orr_service@abtglobal.com'),
    ('bonus_type', 'DV Bonus', 'orr_service@abtglobal.com'),
    ('priority', 'High', 'orr_service@abtglobal.com'),
    ('priority', 'Medium', 'orr_service@abtglobal.com'),
    ('priority', 'Low', 'orr_service@abtglobal.com'),
    ('priority', 'Unspecified', 'orr_service@abtglobal.com');

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
    mckinneyventoyhdp BOOLEAN,
    mckinneyventoyhdprenewals BOOLEAN,
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
    estimated INTEGER,
    tier_1 INTEGER,
    coc_bonus INTEGER NULL,
    dv_bonus INTEGER,
    coc_planning INTEGER,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

-- APP DATA TABLES
CREATE TABLE coc_versions (
    coc_version_id __PK_TYPE__,
    coc_version_name VARCHAR(255),
    coc VARCHAR(6) REFERENCES cocs(coc_code) ON DELETE CASCADE,
    coc_status SMALLINT REFERENCES lookups(reference_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    dv_ard INTEGER,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE coc_version_requests (
    coc_request_id __PK_TYPE__,
    coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    request_status SMALLINT REFERENCES lookups(reference_id), 
    reason_for_rejection SMALLINT REFERENCES lookups(reference_id), 
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE coc_version_users (
    coc_version_user_id __PK_TYPE__,
    coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    username VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    coc_version_role SMALLINT REFERENCES lookups(reference_id), 
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_coc_version_users UNIQUE (coc_version_id, username)
);

CREATE TABLE user_settings (
    user_setting_id __PK_TYPE__,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    coc_user VARCHAR(255) REFERENCES users(username) ON DELETE CASCADE,
    setting_name VARCHAR(255),
    setting_value VARCHAR,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT user_settings_unique_key UNIQUE (coc_version_id, coc_user, setting_name)
);

-- FACTOR TABLES
CREATE TABLE factor_groups (
    factor_group_id __PK_TYPE__,
    factor_group VARCHAR(100),
    funding_action SMALLINT REFERENCES lookups(reference_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

WITH l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
     l_renew AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'Renew')
INSERT INTO factor_groups (factor_group, funding_action, created_by) VALUES
    ('Performance Measures', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Serve High Needs Populations', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Project Effectiveness', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Other and Local Criteria', (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Experience', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
    ('Design of Housing & Supportive Services', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
    ('Timeliness', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
    ('Financial', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
    ('Project Effectiveness', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com'),
    ('Other and Local Criteria', (SELECT reference_id FROM l_new), 'orr_service@abtglobal.com');

CREATE TABLE factor_subgroups (
    factor_subgroup_id __PK_TYPE__,
    factor_subgroup VARCHAR(100),
    factor_group SMALLINT REFERENCES factor_groups(factor_group_id) ON DELETE CASCADE,
    funding_action SMALLINT REFERENCES lookups(reference_id), 
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

WITH l_new AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'New'),
     l_renew AS (SELECT reference_id FROM lookups WHERE reference_type = 'funding_action' AND value = 'Renew'),
     fg_perf_meas_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Performance Measures' AND funding_action = (SELECT reference_id FROM l_renew)),
     fg_high_needs_renew AS (SELECT factor_group_id FROM factor_groups WHERE factor_group = 'Serve High Needs Populations' AND funding_action = (SELECT reference_id FROM l_renew))
INSERT INTO factor_subgroups (factor_subgroup, factor_group, funding_action, created_by) VALUES
    ('Length of Stay', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Exits to Permanent Housing', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Returns to Homelessness (if data is available for project)', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('New or Increased Income and Earned Income', (SELECT factor_group_id FROM fg_perf_meas_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Coordinated Assessment Score', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('Project Focuses on Chronically Homeless People', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com'),
    ('APR Data on ≥ 50% Disability/Zero Income/Unsheltered', (SELECT factor_group_id FROM fg_high_needs_renew), (SELECT reference_id FROM l_renew), 'orr_service@abtglobal.com');

CREATE TABLE thresholds (
    threshold_id __PK_TYPE__,
    type VARCHAR(3), 
    threshold_text TEXT,
    coc_version_id SMALLINT NULL REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE, 
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT custom_threshold_unique_key UNIQUE (coc_version_id, threshold_text)
);

INSERT INTO thresholds (type, threshold_text, created_by) VALUES 
    ('CoC', 'Coordinated Entry Participation', 'orr_service@abtglobal.com'),
    ('CoC', 'Housing First and/or Low Barrier Implementation', 'orr_service@abtglobal.com'),
    ('CoC', 'Documented, secured minimum match', 'orr_service@abtglobal.com'),
    ('HUD', '1. Applicant has Active SAM registration with current information.', 'orr_service@abtglobal.com'); 
    -- Note: Shortened here for brevity, add the rest of your thresholds from the original file!

CREATE TABLE rating_factors (
    rating_factor_id __PK_TYPE__,
    rating_factor_text TEXT,
    rating_factor_text_short TEXT,
    piping_text VARCHAR(200), 
    funding_action SMALLINT REFERENCES lookups(reference_id),
    project_type SMALLINT NULL REFERENCES lookups(reference_id),
    target_population SMALLINT NULL REFERENCES lookups(reference_id),
    factor_group SMALLINT REFERENCES factor_groups(factor_group_id) ON DELETE CASCADE, 
    factor_subgroup SMALLINT REFERENCES factor_subgroups(factor_subgroup_id) ON DELETE CASCADE,
    goal VARCHAR(10) NULL, 
    max_point_value NUMERIC(4, 1),
    coc_version_id SMALLINT NULL REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE UNIQUE INDEX IF NOT EXISTS rating_factors_unique_idx ON rating_factors (
  coc_version_id, COALESCE(project_type, -1), COALESCE(target_population, -1), rating_factor_text
);

-- (Move your massive WITH l_new AS... INSERT INTO rating_factors here)

CREATE TABLE coc_nofo_opportunities (
    coc_nofo_opportunity_id __PK_TYPE__,
    bonus_type SMALLINT REFERENCES lookups(reference_id),
    funding_action SMALLINT REFERENCES lookups(reference_id),
    project_type SMALLINT REFERENCES lookups(reference_id),
    target_population SMALLINT REFERENCES lookups(reference_id),
    population_group SMALLINT REFERENCES lookups(reference_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

-- (Move your WITH l_new AS... INSERT INTO coc_nofo_opportunities here)

-- PROJECTS & CORE TABLES
CREATE TABLE projects (
    project_id __PK_TYPE__,
    coc_version_id SMALLINT REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
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
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE coc_funding_priorities (
    coc_funding_priority_id __PK_TYPE__,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    project_type SMALLINT REFERENCES lookups(reference_id),
    target_population SMALLINT REFERENCES lookups(reference_id),
    population_group SMALLINT REFERENCES lookups(reference_id),
    beds INTEGER NULL,
    funding NUMERIC(11, 2) NULL,
    priority SMALLINT NULL REFERENCES lookups(reference_id),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT coc_funding_priorities_unique_key UNIQUE (coc_version_id, project_type, target_population, population_group)
);

CREATE TABLE selected_coc_nofo_opportunities (
    selected_coc_nofo_opportunity_id __PK_TYPE__,
    coc_nofo_opportunity_id SMALLINT REFERENCES coc_nofo_opportunities(coc_nofo_opportunity_id) ON DELETE CASCADE,
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT coc_nofo_opportunities_unique_key UNIQUE (coc_version_id, coc_nofo_opportunity_id)
);

CREATE TABLE selected_rating_factors (
    selected_rating_factor_id __PK_TYPE__,
    rating_factor_id SMALLINT REFERENCES rating_factors(rating_factor_id) ON DELETE CASCADE,
    goal VARCHAR(100) NULL, 
    max_point_value NUMERIC(4, 1),
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_selected_rating_factors_profile UNIQUE (coc_version_id, rating_factor_id)
);

CREATE TABLE selected_thresholds (
    selected_threshold_id __PK_TYPE__,
    threshold_id SMALLINT REFERENCES thresholds(threshold_id) ON DELETE CASCADE,
    selected BOOLEAN DEFAULT FALSE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_selected_thresholds_profile UNIQUE (coc_version_id, threshold_id)
);

CREATE TABLE rating_scores (
    rating_score_id __PK_TYPE__,
    project_id INTEGER REFERENCES projects(project_id) ON DELETE CASCADE,
    selected_rating_factor_id SMALLINT NULL REFERENCES selected_rating_factors(selected_rating_factor_id) ON DELETE CASCADE,
    rating_score NUMERIC(4, 1) NULL,
    performance VARCHAR(100) NULL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_rating_scores_profile UNIQUE (project_id, selected_rating_factor_id)
);

CREATE TABLE threshold_entries (
    threshold_entry_id __PK_TYPE__,
    project_id INTEGER REFERENCES projects(project_id) ON DELETE CASCADE,
    threshold_id SMALLINT NULL REFERENCES thresholds(threshold_id) ON DELETE CASCADE,
    met_threshold BOOLEAN,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT uq_threshold_entries_profile UNIQUE (project_id, threshold_id)
);

CREATE TABLE project_evaluations (
    project_evaluation_id __PK_TYPE__,
    project_id INTEGER NOT NULL UNIQUE REFERENCES projects(project_id) ON DELETE CASCADE,
    method VARCHAR(7) NULL CHECK (method IN ('in_app', 'outside')),
    met_hud_thresholds BOOLEAN NULL,
    met_coc_thresholds BOOLEAN NULL,
    weighted_score SMALLINT CHECK (weighted_score >= 0 AND weighted_score <= 100),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE ranking (
    rank_id __PK_TYPE__,
    project_id INTEGER REFERENCES projects(project_id) ON DELETE CASCADE,
    coc_version_id INTEGER REFERENCES coc_versions(coc_version_id) ON DELETE CASCADE,
    rank SMALLINT,
    coc_funding_recommendation NUMERIC(11, 2),
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(100) REFERENCES users(username) ON DELETE CASCADE,
    date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(100) NULL REFERENCES users(username) ON DELETE CASCADE,
    version_id INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE user_presence (
    session_id VARCHAR(100),
    context VARCHAR(100),
    user_id VARCHAR(100),
    record_id VARCHAR(100),
    field VARCHAR(100),
    last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (session_id, context)
);

-- ==============================================================================
-- 3. CREATE INDEXES
-- ==============================================================================
CREATE INDEX IF NOT EXISTS idx_cocs_state ON cocs(state);
CREATE INDEX IF NOT EXISTS idx_giw_coc ON giw(coc);
CREATE INDEX IF NOT EXISTS idx_hud_ard_report_coc ON hud_ard_report(coc);
CREATE INDEX IF NOT EXISTS idx_all_hic_data_hudnum ON all_hic_data(hudnum);
CREATE INDEX IF NOT EXISTS idx_all_hic_data_project_type ON all_hic_data(project_type);
CREATE INDEX IF NOT EXISTS idx_coc_versions_coc ON coc_versions(coc);
CREATE INDEX IF NOT EXISTS idx_coc_versions_coc_status ON coc_versions(coc_status);
CREATE INDEX IF NOT EXISTS idx_coc_versions_created_by ON coc_versions(created_by);
CREATE INDEX IF NOT EXISTS idx_coc_version_requests_version_id ON coc_version_requests(coc_version_id);
CREATE INDEX IF NOT EXISTS idx_coc_version_requests_status ON coc_version_requests(request_status);
CREATE INDEX IF NOT EXISTS idx_coc_version_users_username ON coc_version_users(username);
CREATE INDEX IF NOT EXISTS idx_coc_version_users_role ON coc_version_users(coc_version_role);
CREATE INDEX IF NOT EXISTS idx_factor_groups_funding_action ON factor_groups(funding_action);
CREATE INDEX IF NOT EXISTS idx_factor_subgroups_factor_group ON factor_subgroups(factor_group);
CREATE INDEX IF NOT EXISTS idx_factor_subgroups_funding_action ON factor_subgroups(funding_action);
CREATE INDEX IF NOT EXISTS idx_rating_factors_funding_action ON rating_factors(funding_action);
CREATE INDEX IF NOT EXISTS idx_rating_factors_project_type ON rating_factors(project_type);
CREATE INDEX IF NOT EXISTS idx_rating_factors_target_population ON rating_factors(target_population);
CREATE INDEX IF NOT EXISTS idx_rating_factors_factor_group ON rating_factors(factor_group);
CREATE INDEX IF NOT EXISTS idx_rating_factors_factor_subgroup ON rating_factors(factor_subgroup);
CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_bonus_type ON coc_nofo_opportunities(bonus_type);
CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_funding_action ON coc_nofo_opportunities(funding_action);
CREATE INDEX IF NOT EXISTS idx_coc_nofo_opportunities_project_type ON coc_nofo_opportunities(project_type);
CREATE INDEX IF NOT EXISTS idx_projects_coc_version_id ON projects(coc_version_id);
CREATE INDEX IF NOT EXISTS idx_projects_project_type ON projects(project_type);
CREATE INDEX IF NOT EXISTS idx_projects_target_population ON projects(target_population);
CREATE INDEX IF NOT EXISTS idx_projects_funding_action ON projects(funding_action);
CREATE INDEX IF NOT EXISTS idx_coc_funding_priorities_coc_version_id ON coc_funding_priorities(coc_version_id);
CREATE INDEX IF NOT EXISTS idx_coc_funding_priorities_project_type ON coc_funding_priorities(project_type);
CREATE INDEX IF NOT EXISTS idx_selected_coc_nofo_opportunities_coc_version_id ON selected_coc_nofo_opportunities(coc_version_id);
CREATE INDEX IF NOT EXISTS idx_rating_scores_project_id ON rating_scores(project_id);
CREATE INDEX IF NOT EXISTS idx_rating_scores_selected_rating_factor_id ON rating_scores(selected_rating_factor_id);
CREATE INDEX IF NOT EXISTS idx_threshold_entries_project_id ON threshold_entries(project_id);
CREATE INDEX IF NOT EXISTS idx_threshold_entries_threshold_id ON threshold_entries(threshold_id);
CREATE INDEX IF NOT EXISTS idx_ranking_project_id ON ranking(project_id);
CREATE INDEX IF NOT EXISTS idx_ranking_coc_version_id ON ranking(coc_version_id);
CREATE INDEX IF NOT EXISTS idx_rating_scores_project_factor ON rating_scores(project_id, selected_rating_factor_id);
CREATE INDEX IF NOT EXISTS idx_threshold_entries_project_threshold ON threshold_entries(project_id, threshold_id);
CREATE INDEX IF NOT EXISTS idx_references_type ON lookups (reference_type);
CREATE INDEX IF NOT EXISTS idx_user_presence_lookup ON user_presence (record_id, context, field, last_seen);

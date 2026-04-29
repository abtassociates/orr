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

INSERT INTO thresholds (type, threshold_text, created_by)
VALUES 
('CoC', 'Coordinated Entry Participation', 'orr_service@abtglobal.com'),
('CoC', 'Low Barrier Implementation', 'orr_service@abtglobal.com'),
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
('HUD', '4. Financial and Management Capacity:<ul><li>Project applicants and subrecipients demonstrate the financial and management capacity and experience to carry out the project as detailed in the project application</li><li>Applicants must demonstrate the capacity to administer federal funds.</li></ul>', 'orr_service@abtglobal.com'),
('HUD', '5. Certifications – Project applicants submit the required certifications specified in the NOFO.', 'orr_service@abtglobal.com'),
('HUD', '6. Population Served – The population to be served meets program eligibility requirements as described in the Act, the Rule, and the NOFO.', 'orr_service@abtglobal.com'),
('HUD', '7. HMIS Participation –<ul><li>Project applicants (except those only receiving CoC planning or UFA Costs) agree to participate in a local HMIS system.</li><li>Victim service providers must not disclose any personally identifying client info in HMIS.</li><li>Victim service providers must use a comparable database meeting HMIS requirements.</li></ul>', 'orr_service@abtglobal.com'),
('HUD', '8. Applicant has no Outstanding Delinquent Federal Debts – It is HUD policy that applicants with delinquent federal debt are not eligible unless:<ol type="a"><li>A negotiated repayment schedule is established and not delinquent, or</li><li>Other arrangements satisfactory to HUD are made before the award of funds.</li></ol>', 'orr_service@abtglobal.com'),
('HUD', '9. Applicant has no Debarments and/or Suspensions – In accordance with 2 CFR 2424, no federal funds may be awarded to debarred or suspended applicants.', 'orr_service@abtglobal.com'),
('HUD', '10. Pre-selection Review of Performance – If your organization has delinquent federal debt or is excluded from doing business with the Federal government, HUD may:<ol type="a"><li>Deny funding or consider suspension/termination for cause</li><li>Require removal of key individuals from project roles</li><li>Change payment or reporting terms.</li></ol>HUD reviews OMB-designated sources such as:<ul><li>Federal Awardee Performance and Integrity Information System (FAPIIS)</li><li>The "Do Not Pay" website.</li></ul>', 'orr_service@abtglobal.com'),
('HUD', '11. Sufficiency of Financial Management System – HUD will not award funds to applicants lacking a compliant financial management system per 2 CFR 200.302.<br>HUD may conduct surveys for:<ul><li>New applicants without federal award history</li><li>Applicants flagged as high risk due to past performance or financial findings.</li></ul>', 'orr_service@abtglobal.com'),
('HUD', '12. False Statements – A false statement in an application may result in:<ul><li>Denial or termination of award</li><li>Criminal, civil, and/or administrative sanctions</li><li>Fines, penalties, and imprisonment</li></ul>Applicants confirm all statements are truthful.', 'orr_service@abtglobal.com'),
('HUD', '13. Mandatory Disclosure Requirement – Recipients or applicants must disclose, in writing, to HUD:<ul><li>Any violations of Federal criminal law involving fraud, bribery, or gratuity affecting the award</li><li>Disclosures must occur within 10 days of learning of the violation</li></ul>Recipients are also required to:<ul><li>Report proceedings via SAM, per Appendix XII to 2 CFR part 200</li><li>Comply with 2 CFR part 180, 31 U.S.C. 3321, and 31 U.S.C. 2313</li></ul>Failure to disclose may lead to suspension, debarment, or other remedies in §200.338.', 'orr_service@abtglobal.com'),
('HUD', '14. Prohibition Against Lobbying Activities – Applicants must comply with:<ul><li>The Byrd Amendment (31 U.S.C. 1352)</li><li>24 CFR part 87</li></ul>Applicants must:<ul><li>Submit a signed Certification Regarding Lobbying</li><li>Disclose non-federal lobbying efforts via SFLLL (Standard Form for Lobbying)</li></ul>Federally recognized Indian tribes and TDHEs (created via tribal sovereignty) are exempt from the Byrd Amendment.<br>State-recognized tribes and TDHEs under state law must comply.', 'orr_service@abtglobal.com'),
('HUD', '15. Equal Participation of Faith-Based Organizations – All projects must comply with 24 CFR 5.109, updated by HUD (81 FR 19355) on April 4, 2016, to reflect E.O. 13559:<br>"Fundamental Principles and Policymaking Criteria for Partnerships with Faith-Based and Other Neighborhood Organizations"<br>Applies to all HUD programs and activities unless otherwise exempted by program statutes or regulations.', 'orr_service@abtglobal.com'),
('HUD', '16. Resolution of Civil Rights Matters – Applicants with unresolved civil rights matters as of the submission deadline:<ul><li>Will be deemed ineligible</li><li>Will not be reviewed, rated, ranked, or funded</li></ul>', 'orr_service@abtglobal.com');

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
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_psh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('Low Barrier Implementation - CoC assessment of fidelity to minimizing barriers to housing from CoC monitoring or review of project policies and procedures', 'Low Barrier Implementation', 'Commits to a low or no barrier model', (SELECT reference_id FROM l_renew), (SELECT reference_id FROM l_th_rrh), (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_proj_effect_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),

-- Other and Local Criteria (factor_group = fg_other_local_renew, factor_subgroup = NULL)
('Applicant Narrative that CoC Scores', 'Applicant Narrative', 'Project is operating in conformance to CoC standards', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_other_local_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),
('CoC Monitoring Score', 'CoC Monitoring Score', 'Project is operating in conformance to CoC standards', (SELECT reference_id FROM l_renew), NULL, NULL, (SELECT factor_group_id FROM fg_other_local_renew), NULL, 'Yes', 10, 'orr_service@abtglobal.com'),

-- NEW RATING FACTORS
-- Experience (factor_group = fg_experience_new)
('A. Describe the experience of the applicant and sub-recipients (if any) in working with the proposed population and in providing housing similar to that proposed in the application.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('A. Describe the experience of the applicant and sub-recipients (if any) in working with the proposed population and in providing housing similar to that proposed in the application.', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 15, 'orr_service@abtglobal.com'),
('B. Describe experience with utilizing a Housing First approach. This must include:<ul><li>Eligibility criteria</li><li>Process for accepting new clients</li><li>Process and criteria for exiting clients</li><li>Demonstration of no preconditions to entry, allowing entry regardless of current or past substance abuse, income, criminal records (with exceptions of restrictions imposed by federal, state, or local law or ordinance), marital status, familial status, self-disclosed or perceived sexual orientation, gender identity or gender expression.</li><li>Demonstration that the project has a process to address situations that may jeopardize housing or project assistance to ensure that project participation is terminated in only the most severe cases.</li></ul>', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_dv), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
('B. Describe experience with utilizing a Housing First approach. This must include:<ul><li>Eligibility criteria</li><li>Process for accepting new clients</li><li>Process and criteria for exiting clients</li><li>Demonstration of no preconditions to entry, allowing entry regardless of current or past substance abuse, income, criminal records (with exceptions of restrictions imposed by federal, state, or local law or ordinance), marital status, familial status, self-disclosed or perceived sexual orientation, gender identity or gender expression.</li><li>Demonstration that the project has a process to address situations that may jeopardize housing or project assistance to ensure that project participation is terminated in only the most severe cases.</li></ul>', NULL, NULL, (SELECT reference_id FROM l_new), NULL, (SELECT reference_id FROM l_general), (SELECT factor_group_id FROM fg_experience_new), NULL, NULL, 10, 'orr_service@abtglobal.com'),
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

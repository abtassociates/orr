# CoC Project Online Rating and Ranking Tool (ORR)

ORR is an R Shiny application that helps Continuums of Care (CoCs) evaluate and rank projects as part of their annual Continuum of Care Program competition process.

The application provides a customizable framework for reviewing project performance, applying rating factors and thresholds, establishing funding priorities, and developing project rankings. It supports CoC-specific evaluation criteria and produces project rating report cards.

ORR is an optional decision-support tool. Its use does not guarantee additional points in the annual competition, compliance with every Notice of Funding Opportunity (NOFO) requirement, or a funding award. CoCs should consult the applicable NOFO when designing their evaluation and ranking processes.

## Features

* **Project inventory:** Review and manage projects associated with a CoC.
* **Funding priorities:** Configure and maintain priorities and funding ceilings used to inform the ranking process.
* **Project rating:** Evaluate eligible projects using rating factors, thresholds, and scoring criteria that can be customized to the CoC.
* **Project ranking:** Develop project rankings informed by evaluation results and funding priorities.
* **CoC-specific configuration:** Maintain evaluation settings and customized rating factors and thresholds.
* **Rating report cards:** Generate PDF reports summarizing project performance, scores, and maximum available points.
* **User accounts and sessions:** Authenticate users and maintain user-specific application settings and session presence.

## Technology stack

| Component                  | Technology                                      |
| -------------------------- | ----------------------------------------------- |
| Application                | R Shiny                                         |
| User interface             | `bslib`, Bootstrap 5, custom CSS and JavaScript |
| Interactive tables         | `DT`                                            |
| Data manipulation          | `data.table`, `collapse`                        |
| Database interface         | `DBI`, `pool`                                   |
| Production database        | PostgreSQL via `RPostgres`                      |
| Local development database | SQLite via `RSQLite`                            |
| Authentication             | Amazon Cognito                                  |
| PDF reports                | `gt`, `pagedown`                                |
| Background processing      | `mirai`, `promises`                             |
| Dependency management      | `renv`                                          |

The repository includes a dependency lockfile, `renv.lock`, specifying R 4.5.1 and the package versions used by the project.

## Application navigation

The main application sections are:

| Section            | Purpose                                                           |
| ------------------ | ----------------------------------------------------------------- |
| About              | Application overview, authentication entry points, and disclaimer |
| Dashboard          | CoC-specific landing page and overview                            |
| Inventory          | Project inventory and project information                         |
| Funding Priorities | Funding priorities and ceilings                                   |
| Rating             | Project evaluations and scoring                                   |
| Ranking            | Project ranking workflow                                          |
| Account            | User-specific settings and account functionality                  |

Some sections become available only after authentication or after the relevant CoC and project data have been selected or created.

## Repository structure

The application uses Shiny's conventional `ui.R`, `server.R`, and `global.R` entry points, with supporting code organized into directories.

| Path                   | Responsibility                                                                                                             |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `ui.R`                 | Main page layout, navigation, shared UI elements, and application styling and scripts                                      |
| `server.R`             | Session initialization, authentication flow, navigation, and module registration                                           |
| `global.R`             | Package loading, global options, theme configuration, helper loading, database initialization, and background-worker setup |
| `R/global_data_prep.R` | Global lookup data, project-type rules, and constants used throughout the application                                      |
| `R/utils/`             | Shared utilities, including database connection management and report generation                                           |
| `R/app_db_funcs/`      | Application database helper functions                                                                                      |
| `www/`                 | Browser assets, including custom CSS and JavaScript                                                                        |
| `database/`            | Database setup and population resources                                                                                    |
| `sandbox/`             | Local development database and other development artifacts                                                                 |
| `renv.lock`            | Locked R package dependencies                                                                                              |
| `.Rprofile`            | Activates the project's `renv` environment                                                                                 |

Application sections are loaded dynamically after authentication. This keeps the main server entry point focused on shared session state and navigation while delegating section-specific behavior to separate UI and server modules.

## Getting started

### Prerequisites

Before running ORR locally, you will need:

* R 4.5.1, matching the repository's dependency lockfile.
* RStudio or another R development environment.
* Git.
* The R package dependencies specified by `renv.lock`.
* Access to a provisioned development database, or the ability to initialize the local SQLite database.
* Appropriate configuration for authentication and any external services required by the workflow you intend to test.

### 1. Clone the repository

```bash
git clone https://github.com/abtassociates/orr.git
cd orr
git switch dev
```

### 2. Restore the R environment

Open the project in RStudio, or start an R session from the repository root.

```r
install.packages("renv")
renv::restore()
```

This restores the package versions recorded in `renv.lock`. Some packages may require system libraries or additional operating-system dependencies.

### 3. Configure the database

ORR supports SQLite for local development and PostgreSQL for deployed environments.

The database connection logic is in `R/utils/db_connect.R`.

In local RStudio development, the application defaults to SQLite. The local database is expected at:

```text
sandbox/dev_db.sqlite
```

A new or empty SQLite file is not sufficient: the database must contain the required schema and reference data. Use the repository's database setup and population resources to initialize it.

PostgreSQL configuration is supplied through environment variables rather than hard-coded connection credentials. The connection code uses the following settings:

| Environment variable | Purpose                             |
| -------------------- | ----------------------------------- |
| `AWS_RDS_HOST`       | PostgreSQL host                     |
| `AWS_RDS_PORT`       | PostgreSQL port; defaults to `5432` |
| `AWS_RDS_USERNAME`   | Database username                   |
| `AWS_RDS_PASSWORD`   | Database password                   |
| `AWS_RDS_DBNAME`     | Production database name            |
| `AWS_RDS_DBNAME_DEV` | Development database name           |

Local development against PostgreSQL may additionally require an SSH tunnel and the corresponding SSH configuration.

Keep credentials and private configuration outside version control. Do not commit database passwords, private keys, or environment-specific secrets.

### 4. Configure authentication and application settings

The application integrates with Amazon Cognito for sign-in and account creation. Authentication redirects, session handling, and browser-side helpers are configured through the application code and its deployment environment.

A working database connection alone does not guarantee that authentication will work. Configure the required Cognito settings and other deployment-specific values before testing the full login flow.

### 5. Run the application

From the repository root, launch the app in R:

```r
shiny::runApp()
```

In a local RStudio session, the application detects the development environment and uses SQLite by default. Confirm that the local database has been initialized before launching.

## Development notes

### Database access

Database connection and query helpers are centralized in the shared utility and application database-function directories. The application uses a connection pool so database connections can be reused across requests and sessions.

Prefer the existing database helper functions when adding or modifying queries. Use parameterized queries for user-supplied values, and preserve the database behavior expected by both SQLite and PostgreSQL.

SQLite is intended to facilitate local development; it should not be assumed to reproduce every PostgreSQL behavior.

### Shiny modules

The main server initializes shared user and CoC state, handles authentication, and registers the application sections after login. When extending a section, follow the existing module pattern and keep section-specific behavior in its own module rather than expanding the top-level server unnecessarily.

Changes to shared reactive state, database helpers, or global initialization can affect multiple sections and should be tested accordingly.

### Background report generation

PDF report cards are built using `gt` and rendered to PDF with `pagedown`. The application configures `mirai` workers for report-generation tasks.

When modifying report generation, consider both the HTML report content and the PDF-rendering environment. The rendering process relies on a working Chrome-compatible browser installation and its associated system dependencies.

### Application configuration

Global configuration is initialized in `global.R`. This includes package loading, theme and brand configuration, logging, utility sourcing, database initialization, and background-worker setup.

When adding configuration, prefer explicit environment variables or centralized configuration over embedding environment-specific values in application logic.

## Troubleshooting

**The application cannot connect to the database**

* Confirm that the local SQLite database has been initialized or that PostgreSQL is reachable.
* Verify the configured database name and required environment variables.
* For local PostgreSQL development, confirm that the SSH tunnel and credentials are valid.

**Login or redirects fail**

* Verify the Cognito configuration and callback URLs for the current environment.
* Check that the authentication settings match the URL where the application is running.
* Inspect the application logs for authentication and session errors.

**PDF generation fails**

* Confirm that the browser required by `pagedown` is installed and accessible.
* Check the application logs for browser-launch, timeout, or rendering errors.

**A section is missing after login**

* Confirm that the section is registered in the application navigation configuration.
* Check the module's UI and server functions and inspect the logs for initialization errors.

## Contributing

Contributions should preserve the existing application architecture and database conventions.

When submitting a change:

1. Describe the user-facing behavior or defect being addressed.
2. Keep changes focused and avoid introducing unnecessary duplication in shared utilities.
3. Test the affected application sections and database operations.
4. Verify behavior against the intended development database.
5. Document new configuration requirements and any changes to database initialization.

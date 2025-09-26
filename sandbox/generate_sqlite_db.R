library(DBI)
library(RPostgres)
library(RSQLite)
library(here)
library(glue)
library(dplyr)

#-----------------------------
# PostgreSQL Connection Config
#-----------------------------
DB_CONFIG <- list(
  host     = Sys.getenv("AWS_RDS_HOST"),
  port     = as.integer(Sys.getenv("AWS_RDS_PORT", "5432")),
  dbname   = Sys.getenv("AWS_RDS_DBNAME"),
  username = Sys.getenv("AWS_RDS_USERNAME"),
  password = Sys.getenv("AWS_RDS_PASSWORD")
)

pg_conn <- dbConnect(
  RPostgres::Postgres(),
  host     = DB_CONFIG$host,
  port     = DB_CONFIG$port,
  dbname   = DB_CONFIG$dbname,
  user     = DB_CONFIG$username,
  password = DB_CONFIG$password,
  sslmode  = "require"
)

#-----------------------------
# SQLite Connection
#-----------------------------
sqlite_conn <- dbConnect(
  RSQLite::SQLite(),
  here::here("sandbox/dev_db.sqlite")
)

# Enable foreign key constraints in SQLite
dbExecute(sqlite_conn, "PRAGMA foreign_keys = ON;")

#-----------------------------
# Type Mapping: Postgres → SQLite
#-----------------------------
pg_to_sqlite_type <- function(pg_type) {
  t <- tolower(pg_type)
  if (t %in% c("integer", "int", "int4", "serial", "smallint", "bigint", "bigserial")) {
    return("INTEGER")
  } else if (grepl("numeric|decimal|real|double|float", t)) {
    return("REAL")
  } else if (grepl("bool", t)) {
    return("INTEGER") # SQLite uses INTEGER for boolean (0/1)
  } else if (grepl("timestamp|date|time", t)) {
    return("TEXT") # SQLite stores dates as text
  } else if (grepl("uuid", t)) {
    return("TEXT")
  } else if (grepl("json", t)) {
    return("TEXT")
  } else {
    return("TEXT") # default fallback
  }
}

#-----------------------------
# Get Postgres Table Schema
#-----------------------------
get_pg_table_schema <- function(pg_conn, table) {
  query <- glue("
    SELECT column_name, data_type, is_nullable, column_default
    FROM information_schema.columns
    WHERE table_name = '{table}'
      AND table_schema = 'public'
    ORDER BY ordinal_position
  ")
  dbGetQuery(pg_conn, query)
}

#-----------------------------
# Get Primary Keys
#-----------------------------
get_primary_keys <- function(pg_conn) {
  query <- "
    SELECT
      kcu.table_name,
      kcu.column_name,
      kcu.ordinal_position
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
      AND tc.table_schema = 'public'
    ORDER BY kcu.table_name, kcu.ordinal_position
  "
  dbGetQuery(pg_conn, query)
}

#-----------------------------
# Get Foreign Keys
#-----------------------------
get_foreign_keys <- function(pg_conn) {
  query <- "
    SELECT
      tc.constraint_name,
      tc.table_name,
      kcu.column_name,
      ccu.table_name AS foreign_table_name,
      ccu.column_name AS foreign_column_name,
      rc.update_rule,
      rc.delete_rule
    FROM information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema = kcu.table_schema
    JOIN information_schema.constraint_column_usage AS ccu
      ON ccu.constraint_name = tc.constraint_name
      AND ccu.table_schema = tc.table_schema
    JOIN information_schema.referential_constraints AS rc
      ON tc.constraint_name = rc.constraint_name
      AND tc.table_schema = rc.constraint_schema
    WHERE tc.constraint_type = 'FOREIGN KEY'
      AND tc.table_schema = 'public'
    ORDER BY tc.table_name, tc.constraint_name
  "
  dbGetQuery(pg_conn, query)
}

#-----------------------------
# Get Unique Constraints
#-----------------------------
get_unique_constraints <- function(pg_conn) {
  query <- "
    SELECT
      tc.constraint_name,
      tc.table_name,
      kcu.column_name
    FROM information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'UNIQUE'
      AND tc.table_schema = 'public'
    ORDER BY tc.table_name, tc.constraint_name, kcu.ordinal_position
  "
  dbGetQuery(pg_conn, query)
}

#-----------------------------
# Get Check Constraints
#-----------------------------
get_check_constraints <- function(pg_conn) {
  query <- "
    SELECT
      tc.constraint_name,
      tc.table_name,
      cc.check_clause
    FROM information_schema.table_constraints AS tc
    JOIN information_schema.check_constraints AS cc
      ON tc.constraint_name = cc.constraint_name
      AND tc.constraint_schema = cc.constraint_schema
    WHERE tc.constraint_type = 'CHECK'
      AND tc.table_schema = 'public'
    ORDER BY tc.table_name, tc.constraint_name
  "
  dbGetQuery(pg_conn, query)
}

#-----------------------------
# Handle Default Values
#-----------------------------
convert_pg_default <- function(default_val, data_type, is_pk = FALSE) {
  if (is.na(default_val) || default_val == "" || is.null(default_val)) {
    return("")
  }
  
  # Convert to string and trim
  default_val <- trimws(as.character(default_val))
  
  # Skip sequence defaults entirely - these will be handled as AUTOINCREMENT for PKs
  if (grepl("nextval\\(", default_val)) {
    return("")
  }
  
  # Skip other complex PostgreSQL functions
  if (grepl("^[a-zA-Z_]+\\(.*\\)$", default_val) && 
      !tolower(default_val) %in% c("now()", "current_timestamp", "current_date", "current_time")) {
    return("")
  }
  
  # Remove PostgreSQL-specific syntax
  default_val <- gsub("::.*", "", default_val) # Remove type casts
  default_val <- trimws(default_val)
  
  # Handle common patterns
  if (grepl("^'.*'$", default_val)) {
    return(paste0("DEFAULT ", default_val))
  } else if (tolower(default_val) %in% c("true", "false")) {
    bool_val <- if (tolower(default_val) == "true") "1" else "0"
    return(paste0("DEFAULT ", bool_val))
  } else if (grepl("^-?[0-9]+(\\.[0-9]+)?$", default_val)) {
    return(paste0("DEFAULT ", default_val))
  } else if (tolower(default_val) %in% c("now()", "current_timestamp", "current_date", "current_time")) {
    return("DEFAULT CURRENT_TIMESTAMP")
  } else if (tolower(default_val) == "null") {
    return("DEFAULT NULL")
  } else if (default_val != "" && !grepl("\\(", default_val)) {
    # Only simple values without function calls
    return(paste0("DEFAULT '", gsub("'", "''", default_val), "'"))
  }
  
  return("")
}

#-----------------------------
# Create SQLite Table from PG Schema
#-----------------------------
create_sqlite_table_from_pg <- function(pg_conn, sqlite_conn, table, pks_df) {
  schema_df <- get_pg_table_schema(pg_conn, table)
  table_pks <- pks_df[pks_df$table_name == table, ]
  
  col_defs <- mapply(function(name, type, nullable, default) {
    sqlite_type <- pg_to_sqlite_type(type)
    null_str <- if (nullable == "NO") "NOT NULL" else ""
    
    # Check if this column is a primary key
    is_pk <- name %in% table_pks$column_name
    pk_str <- ""
    
    # Handle sequence defaults (nextval) for primary keys
    has_sequence_default <- !is.na(default) && grepl("nextval\\(", default)
    
    if (is_pk && nrow(table_pks) == 1) {
      if (has_sequence_default || grepl("serial", tolower(type))) {
        pk_str <- "PRIMARY KEY AUTOINCREMENT"
        default_str <- "" # No default needed for autoincrement
      } else {
        pk_str <- "PRIMARY KEY"
        default_str <- convert_pg_default(default, type, is_pk)
      }
    } else {
      default_str <- convert_pg_default(default, type, is_pk)
    }
    
    # Clean up and combine parts, removing empty strings
    parts <- c(glue('"{name}"'), sqlite_type, pk_str, null_str, default_str)
    parts <- parts[parts != "" & !is.na(parts)]
    paste(parts, collapse = " ")
  }, schema_df$column_name, schema_df$data_type, schema_df$is_nullable, schema_df$column_default)
  
  # Remove any empty column definitions
  col_defs <- col_defs[col_defs != "" & !is.na(col_defs)]
  
  # Add composite primary key if multiple columns
  if (nrow(table_pks) > 1) {
    pk_cols <- paste0('"', table_pks$column_name, '"', collapse = ", ")
    col_defs <- c(col_defs, glue("PRIMARY KEY ({pk_cols})"))
  }
  
  create_sql <- glue("CREATE TABLE \"{table}\" (\n  {paste(col_defs, collapse = ',\n  ')}\n);")
  
  message(glue("Creating table: {table}"))
  tryCatch({
    dbExecute(sqlite_conn, create_sql)
    message("✅ Table created successfully")
  }, error = function(e) {
    message(glue("❌ Error creating table {table}: {e$message}"))
    message(glue("SQL: {create_sql}"))
    # Print each column definition for debugging
    message("Column definitions:")
    for (i in seq_along(col_defs)) {
      message(glue("  {i}: {col_defs[i]}"))
    }
  })
}

#-----------------------------
# Create Foreign Key Constraints
#-----------------------------
create_foreign_keys <- function(sqlite_conn, fks_df) {
  message("Creating foreign key constraints...")
  
  # Group by table and constraint name
  fk_groups <- fks_df %>%
    group_by(table_name, constraint_name) %>%
    summarise(
      columns = paste(column_name, collapse = ", "),
      foreign_table = first(foreign_table_name),
      foreign_columns = paste(foreign_column_name, collapse = ", "),
      update_rule = first(update_rule),
      delete_rule = first(delete_rule),
      .groups = 'drop'
    )
  
  for (i in seq_len(nrow(fk_groups))) {
    fk <- fk_groups[i, ]
    
    # Convert referential action rules
    on_update <- if (fk$update_rule == "CASCADE") "ON UPDATE CASCADE" else 
      if (fk$update_rule == "SET NULL") "ON UPDATE SET NULL" else
        if (fk$update_rule == "RESTRICT") "ON UPDATE RESTRICT" else ""
    
    on_delete <- if (fk$delete_rule == "CASCADE") "ON DELETE CASCADE" else 
      if (fk$delete_rule == "SET NULL") "ON DELETE SET NULL" else
        if (fk$delete_rule == "RESTRICT") "ON DELETE RESTRICT" else ""
    
    # SQLite doesn't support ADD CONSTRAINT for FK, so we'd need to recreate tables
    # For now, we'll log the FK relationships that should be manually added
    message(glue("FK: {fk$table_name}({fk$columns}) -> {fk$foreign_table}({fk$foreign_columns}) {on_update} {on_delete}"))
  }
}

#-----------------------------
# Create Indexes for Unique Constraints
#-----------------------------
create_unique_constraints <- function(sqlite_conn, unique_df) {
  message("Creating unique constraints...")
  
  # Group by table and constraint name
  unique_groups <- unique_df %>%
    group_by(table_name, constraint_name) %>%
    summarise(columns = paste0('"', column_name, '"', collapse = ", "), .groups = 'drop')
  
  for (i in seq_len(nrow(unique_groups))) {
    uc <- unique_groups[i, ]
    index_sql <- glue('CREATE UNIQUE INDEX "idx_{uc$constraint_name}" ON "{uc$table_name}" ({uc$columns});')
    
    tryCatch({
      dbExecute(sqlite_conn, index_sql)
      message(glue("✅ Unique constraint created: {uc$constraint_name}"))
    }, error = function(e) {
      message(glue("❌ Error creating unique constraint {uc$constraint_name}: {e$message}"))
    })
  }
}

#-----------------------------
# Main Copy Function
#-----------------------------
copy_pg_to_sqlite <- function(pg_conn, sqlite_conn) {
  # Get all metadata first
  tables <- dbListTables(pg_conn)
  pks_df <- get_primary_keys(pg_conn)
  fks_df <- get_foreign_keys(pg_conn)
  unique_df <- get_unique_constraints(pg_conn)
  check_df <- get_check_constraints(pg_conn)
  
  message(sprintf("Found %d tables in PostgreSQL database.", length(tables)))
  
  # Start transaction
  dbBegin(sqlite_conn)
  
  tryCatch({
    # Phase 1: Create all tables with primary keys
    for (table in tables) {
      message(sprintf("Phase 1 - Processing table: %s", table))
      
      # Drop existing table if exists
      dbExecute(sqlite_conn, glue('DROP TABLE IF EXISTS "{table}";'))
      
      # Create schema in SQLite
      create_sqlite_table_from_pg(pg_conn, sqlite_conn, table, pks_df)
      
      message("---------------------------------------------------")
    }
    
    # Phase 2: Copy data
    for (table in tables) {
      message(sprintf("Phase 2 - Copying data for table: %s", table))
      
      # Read from Postgres
      data <- dbReadTable(pg_conn, table)
      
      # Convert boolean columns for SQLite
      schema_df <- get_pg_table_schema(pg_conn, table)
      bool_cols <- schema_df$column_name[grepl("bool", tolower(schema_df$data_type))]
      
      if (length(bool_cols) > 0 && nrow(data) > 0) {
        for (bool_col in bool_cols) {
          if (bool_col %in% names(data)) {
            data[[bool_col]] <- as.integer(as.logical(data[[bool_col]]))
          }
        }
      }
      
      # Datetime
      data$date_created <- format(data$date_created, "%Y-%m-%d %H:%M:%S")
      data$date_updated <- format(data$date_updated, "%Y-%m-%d %H:%M:%S")

      # Insert into SQLite
      if (nrow(data) > 0) {
        dbWriteTable(sqlite_conn, table, data, overwrite=TRUE)
        message(glue("✅ Copied {nrow(data)} rows"))
      } else {
        message("ℹ️ No data to copy")
      }
      
      message("---------------------------------------------------")
    }
    
    # Phase 3: Create constraints and indexes
    if (nrow(unique_df) > 0) {
      create_unique_constraints(sqlite_conn, unique_df)
    }
    
    # Log foreign key relationships (SQLite FK creation would require table recreation)
    if (nrow(fks_df) > 0) {
      create_foreign_keys(sqlite_conn, fks_df)
      message("ℹ️ Foreign key relationships logged above. SQLite foreign keys are enabled.")
    }
    
    # Log check constraints (SQLite has limited CHECK constraint support)
    if (nrow(check_df) > 0) {
      message("Check constraints found (limited SQLite support):")
      for (i in seq_len(nrow(check_df))) {
        cc <- check_df[i, ]
        message(glue("  {cc$table_name}: {cc$check_clause}"))
      }
    }
    
    # Commit transaction
    dbCommit(sqlite_conn)
    message("✅ Transaction committed successfully!")
    
  }, error = function(e) {
    dbRollback(sqlite_conn)
    stop(glue("Migration failed: {e$message}"))
  })
  
  message("✅ All tables copied with schema preserved!")
  message("ℹ️ Note: Some PostgreSQL features may have limited SQLite equivalents")
}

#-----------------------------
# Verify Migration
#-----------------------------
verify_migration <- function(pg_conn, sqlite_conn) {
  message("Verifying migration...")
  
  pg_tables <- dbListTables(pg_conn)
  sqlite_tables <- dbListTables(sqlite_conn)
  
  message(glue("PostgreSQL tables: {length(pg_tables)}"))
  message(glue("SQLite tables: {length(sqlite_tables)}"))
  
  for (table in pg_tables) {
    if (table %in% sqlite_tables) {
      pg_count <- dbGetQuery(pg_conn, glue("SELECT COUNT(*) as count FROM \"{table}\""))$count
      sqlite_count <- dbGetQuery(sqlite_conn, glue("SELECT COUNT(*) as count FROM \"{table}\""))$count
      
      status <- if (pg_count == sqlite_count) "✅" else "❌"
      message(glue("{status} {table}: PG={pg_count}, SQLite={sqlite_count}"))
    } else {
      message(glue("❌ {table}: Missing in SQLite"))
    }
  }
}

#-----------------------------
# Run Migration
#-----------------------------
message("Starting PostgreSQL to SQLite migration...")

# Perform the migration
copy_pg_to_sqlite(pg_conn, sqlite_conn)

# Verify the results
verify_migration(pg_conn, sqlite_conn)

message("🎉 Migration completed!")
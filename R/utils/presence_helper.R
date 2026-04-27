clean_presence_table <- function() {
  # Delete anything where the heartbeat stopped more than 30 mins ago
  threshold <- format(Sys.time() - (30 * 60), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  db_execute(
    "DELETE FROM user_presence WHERE last_seen < $1",
    params = threshold
  )
}
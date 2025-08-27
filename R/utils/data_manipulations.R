
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

get_labelled_lookups <- function(l, lookup_col = "value") {
  lookup_info <- lookups[reference_type == l]
  setNames(lookup_info$reference_id, lookup_info[[lookup_col]])
}
get_lookup_label <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- lookups[reference_type == ref_type]
  if(is.character(v)) {
    filtered_lookups[reference_id == v, get(lookup_col)]
  } else {
    filtered_lookups[.(v), get(lookup_col), on = "reference_id"]
  } 
}

get_lookup_refid <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- lookups[reference_type == ref_type]
  if(is.character(v)) {
    r <- filtered_lookups[get(lookup_col) == v, "reference_id"]
  } else {
    r <- filtered_lookups[.(v), get(lookup_col), on = "reference_id"]
  } 
  as.integer(r)
}

convert_to_factor <- function(data, v, textToNum = FALSE, label_col = "value") {
  lookup_info <- lookups[reference_type == v, .(reference_id, value, value_abbrev, value_long)]
  
  factor(
    if(!textToNum) data[[v]] else lookup_info$reference_id[match(data[[v]], lookup_info[[label_col]])],
    levels = lookup_info$reference_id,
    labels = lookup_info[[label_col]]
  )
}
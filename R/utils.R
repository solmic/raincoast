#R/utils.R
drop_all <- function(x) setdiff(x, "All")

update_choices <- function(session, id, vec, selected = "All") {
  updateSelectInput(
    session, id,
    choices  = c("All", sort(unique(na.omit(vec)))),
    selected = selected
  )
}

paired_map <- function(levels) {
  levels <- as.character(levels)
  n <- length(levels)
  cols <- if (n <= 12) {
    pal <- RColorBrewer::brewer.pal(max(3, n), "Paired")
    pal[seq_len(n)]
  } else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n)
  }
  setNames(cols, levels)
}

pal_for_levels <- function(levels, palette = "Batlow") {
  cols <- colorspace::sequential_hcl(length(levels), palette = palette)
  setNames(cols, levels)
}

filter_multi <- function(dat, col, selected) {
  keep <- drop_all(selected)
  if (length(keep) > 0) dat <- dat[dat[[col]] %in% keep, ]
  dat
}

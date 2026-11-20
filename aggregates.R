# Categories of one classification that are the sum of categories of the other,
# by their codes: those of COFOG (GF...) are made of Hungarian ones (F...), and
# vice versa
aggregates <- list(
  GF01 = c("F01", "F15"),
  GF0101 = c("F01.a", "F01.b", "F01.c"),
  GF0104 = c("F01.d", "F01.e"),
  GF04 = c("F09", "F10", "F11", "F12", "F13"),
  F01 = c("GF0101", "GF0102", "GF0103", "GF0104", "GF0105", "GF0106"),
  F01.f = c("GF0102", "GF0103", "GF0105", "GF0106"),
  F03.b = c("GF0301", "GF0305", "GF0306"),
  F04.d = c("GF0905", "GF0906", "GF0907", "GF0908")
)

# The rows to add to `data` for the aggregates made of the codes in its column
# `from` ("huncode" or "cofog"): for each country (geo), year (year) and
# type (type), the sum of the parts, coded only in the other column
aggregate_rows <- function(data, from) {
  to <- setdiff(c("huncode", "cofog"), from)
  codes <- names(aggregates)[startsWith(names(aggregates), "GF") == (from == "huncode")]
  rbindlist(lapply(codes, \(code) {
    parts <- aggregates[[code]]
    # A misspelt code would otherwise just be left out of the sum
    absent <- setdiff(parts, data[[from]])
    if (length(absent) > 0) stop(code, ": no data for ", toString(absent))
    rows <- data[get(from) %chin% parts, .(value = sum(value)), by = .(geo, year, type)]
    rows[, (c(to, from, "level")) := list(
      code, NA_character_, if (grepl("^G?F[0-9]{2}$", code)) "main_group" else "subgroup"
    )]
  }))
}

# The sum of `codes`, with each run of at least three consecutive codes written
# as a range (e.g. GF0101–GF0106). Codes run on their final letter (F01.a,
# F01.b) or their last two digits (F09, F10; GF0101, GF0102)
sum_label <- function(codes) {
  codes <- sort(codes)
  stem <- sub("([0-9]{2}|[a-z])$", "", codes)
  suffix <- substring(codes, nchar(stem) + 1)
  pos <- match(suffix, letters)
  pos[is.na(pos)] <- as.integer(suffix[is.na(pos)])
  run <- cumsum(c(TRUE, stem[-1] != stem[-length(stem)] | !(diff(pos) %in% 1)))
  runs <- vapply(split(codes, run), \(r) {
    if (length(r) >= 3) paste0(r[1], "\u2013", r[length(r)]) else paste(r, collapse = " + ")
  }, "")
  paste(runs, collapse = " + ")
}
aggregate_of <- vapply(aggregates, sum_label, "")
# A category's code in one classification or, if it's an aggregate, the sum it's
# built from, else a dash
code_label <- \(code, other) fcoalesce(code, unname(aggregate_of[other]), "-")
# The aggregates whose code starts with `prefix` (GF: those of COFOG, F: the
# Hungarian ones), written out for the text, e.g. "GF01 = F01 + F15, ..."
written_out <- \(prefix) {
  codes <- names(aggregates)[startsWith(names(aggregates), prefix)]
  paste(codes, "=", vapply(aggregates[codes], paste, "", collapse = " + "), collapse = ", ")
}
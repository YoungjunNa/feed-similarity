feed_error <- function(message) {
  stop(structure(list(message = message, call = NULL),
                 class = c("feed_input_error", "error", "condition")))
}

feed_files <- c(feedipedia = "feedipedia_all.xlsx",
                feedtable = "feedtable-all.xlsx")

feed_demo_files <- c(feedipedia = "demo_database_1.xlsx",
                     feedtable = "demo_database_2.xlsx")

read_feed_data <- function(directory = "data") {
  setNames(lapply(names(feed_files), function(db) {
    path <- file.path(directory, feed_files[[db]])
    kind <- "original"
    if (!file.exists(path)) {
      # Fall back to the bundled synthetic demonstration dataset.
      path <- file.path(directory, feed_demo_files[[db]])
      kind <- "demo"
    }
    if (!file.exists(path)) {
      feed_error(paste0("Missing source file: place ", feed_files[[db]],
                        " or ", feed_demo_files[[db]], " in ", directory, "/"))
    }
    x <- as.data.frame(readxl::read_excel(path, .name_repair = "check_unique"))
    if (!"feed" %in% names(x) || !is.character(x$feed) ||
        anyNA(x$feed) || any(!nzchar(x$feed))) {
      feed_error(paste("Invalid feed names in", path))
    }
    hash <- unname(tools::md5sum(path))
    x$feed_id <- sprintf("%s-%s-s1-r%05d", db, substr(hash, 1, 12),
                         seq_len(nrow(x)) + 1L)
    attr(x, "source_md5") <- hash
    attr(x, "database") <- db
    attr(x, "dataset_kind") <- kind
    x
  }), names(feed_files))
}

nutrient_names <- function(x) setdiff(names(x), c("feed", "feed_id"))

audit_nutrients <- function(x) {
  do.call(rbind, lapply(nutrient_names(x), function(nm) {
    v <- x[[nm]]
    numeric <- is.numeric(v)
    finite <- if (numeric) is.finite(v) else rep(FALSE, length(v))
    n <- sum(finite)
    s <- if (n >= 2L) stats::sd(v[finite]) else NA_real_
    reason <- if (all(is.na(v))) "all_missing" else if (!numeric) {
      "non_numeric_do_not_coerce"
    } else if (n < 2L) "fewer_than_two_finite_values" else if (s == 0) {
      "zero_variance"
    } else "eligible"
    data.frame(nutrient = nm, type = class(v)[1],
               n_rows = length(v), n_missing = sum(is.na(v)),
               n_nonfinite_not_na = if (numeric) sum(!is.finite(v) & !is.na(v)) else 0L,
               n_finite = n, n_zero = if (numeric) sum(v == 0, na.rm = TRUE) else NA_integer_,
               mean = if (n) mean(v[finite]) else NA_real_, sd = s,
               min = if (n) min(v[finite]) else NA_real_,
               max = if (n) max(v[finite]) else NA_real_,
               status = reason, stringsAsFactors = FALSE)
  }))
}

default_nutrients <- function(db) {
  switch(db,
         feedipedia = c("crude_protein_percent_dm", "crude_fibre_percent_dm",
                       "ether_extract_percent_dm", "ash_percent_dm",
                       "gross_energy_mj_kg_dm"),
         feedtable = c("crude_protein_percent", "crude_fibre_percent",
                      "crude_fat_percent", "ash_percent",
                      "gross_energy_mj_mj_kg"),
         feed_error("Unknown database."))
}

validate_feed_ids <- function(x) {
  if (!all(c("feed", "feed_id") %in% names(x)) || !nrow(x) ||
      anyNA(x$feed_id) || any(!nzchar(x$feed_id)) || anyDuplicated(x$feed_id)) {
    feed_error("Feed records require nonempty, unique feed_id values.")
  }
}

fit_feed_scale <- function(x, nutrients, pool_ids = x$feed_id) {
  validate_feed_ids(x)
  if (!length(nutrients) || anyNA(nutrients) || anyDuplicated(nutrients) ||
      !all(nutrients %in% nutrient_names(x))) {
    feed_error("Select at least one distinct, known nutrient.")
  }
  if (!length(pool_ids) || anyNA(pool_ids) || anyDuplicated(pool_ids) ||
      !all(pool_ids %in% x$feed_id)) feed_error("Invalid standardization pool IDs.")
  if (!all(vapply(x[nutrients], is.numeric, logical(1)))) {
    feed_error("Selected nutrients must be numeric; logical values are not concentrations.")
  }
  pool <- x[match(pool_ids, x$feed_id), nutrients, drop = FALSE]
  params <- do.call(rbind, lapply(nutrients, function(nm) {
    v <- pool[[nm]][is.finite(pool[[nm]])]
    data.frame(nutrient = nm, n = length(v),
               mean = if (length(v)) mean(v) else NA_real_,
               sd = if (length(v) >= 2L) stats::sd(v) else NA_real_)
  }))
  bad <- params$n < 2L | !is.finite(params$sd) | params$sd <= 0
  if (any(bad)) {
    feed_error(paste("Cannot standardize insufficient or constant nutrients:",
                     paste(params$nutrient[bad], collapse = ", ")))
  }
  params
}

feed_distance <- function(x, reference_id, nutrients, pool_ids = x$feed_id,
                          candidate_ids = x$feed_id, standardize = TRUE,
                          weights = NULL, exclude_self = TRUE) {
  params <- fit_feed_scale(x, nutrients, pool_ids)
  if (length(reference_id) != 1L || is.na(reference_id) ||
      !reference_id %in% x$feed_id) feed_error("Select exactly one valid reference ID.")
  if (anyNA(candidate_ids) || anyDuplicated(candidate_ids) ||
      !all(candidate_ids %in% x$feed_id)) feed_error("Invalid candidate IDs.")
  ref_row <- match(reference_id, x$feed_id)
  reference <- as.numeric(x[ref_row, nutrients, drop = TRUE])
  if (any(!is.finite(reference))) {
    feed_error(paste("Reference lacks finite values for:",
                     paste(nutrients[!is.finite(reference)], collapse = ", ")))
  }
  multiplier <- rep(1, length(nutrients))
  if (!is.null(weights)) {
    if (length(weights) != length(nutrients) || any(!is.finite(weights)) ||
        any(weights <= 0)) feed_error("Weights must be finite and strictly positive.")
    multiplier <- weights / sum(weights)
  }
  ids <- if (exclude_self) setdiff(candidate_ids, reference_id) else candidate_ids
  candidates <- x[match(ids, x$feed_id), , drop = FALSE]
  values <- as.matrix(candidates[nutrients])
  observed <- rowSums(is.finite(values))
  complete <- observed == length(nutrients)
  excluded <- data.frame(feed_id = candidates$feed_id[!complete],
                         feed = candidates$feed[!complete],
                         observed = observed[!complete],
                         required = rep(length(nutrients), sum(!complete)),
                         reason = rep("missing_or_nonfinite_selected_nutrient",
                                      sum(!complete)))
  valid <- candidates[complete, , drop = FALSE]
  delta <- sweep(values[complete, , drop = FALSE], 2L, reference, "-")
  if (standardize) delta <- sweep(delta, 2L, params$sd, "/")
  contributions <- sweep(delta^2, 2L, multiplier, "*")
  squared <- rowSums(contributions)
  ranked <- data.frame(feed_id = valid$feed_id, feed = valid$feed,
                       distance = sqrt(squared), squared_distance = squared,
                       rank = as.integer(rank(squared, ties.method = "min")),
                       valid[nutrients], check.names = FALSE)
  ranked <- ranked[order(ranked$distance, ranked$feed_id), , drop = FALSE]
  rownames(ranked) <- NULL
  long <- data.frame(
    feed_id = rep(valid$feed_id, each = length(nutrients)),
    nutrient = rep(nutrients, times = nrow(valid)),
    raw_difference = as.vector(t(sweep(values[complete, , drop = FALSE],
                                      2L, reference, "-"))),
    squared_contribution = as.vector(t(contributions)),
    share = as.vector(t(contributions / ifelse(squared > 0, squared, NA_real_))))
  list(ranked = ranked, excluded = excluded,
       reference = x[ref_row, c("feed_id", "feed", nutrients), drop = FALSE],
       parameters = params, contributions = long, pool_ids = pool_ids,
       nutrients = nutrients, standardize = standardize, weights = weights)
}

rank_agreement <- function(a, b, k) {
  if (length(k) != 1L || !is.finite(k) || k < 1L || k != as.integer(k)) {
    feed_error("k must be a positive integer.")
  }
  a <- a[order(a$distance, a$feed_id), , drop = FALSE]
  b <- b[order(b$distance, b$feed_id), , drop = FALSE]
  common <- intersect(a$feed_id, b$feed_id)
  ac <- a[match(common, a$feed_id), , drop = FALSE]
  bc <- b[match(common, b$feed_id), , drop = FALSE]
  ar <- rank(ac$distance, ties.method = "average")
  br <- rank(bc$distance, ties.method = "average")
  rho <- if (length(common) >= 3L && stats::sd(ar) > 0 && stats::sd(br) > 0) {
    stats::cor(ar, br)
  } else NA_real_
  top_a <- head(a$feed_id, k)
  top_b <- head(b$feed_id, k)
  intersection <- length(intersect(top_a, top_b))
  union <- length(union(top_a, top_b))
  denominator <- min(k, nrow(a), nrow(b))
  common_a <- ac[order(ac$distance, ac$feed_id), , drop = FALSE]
  common_b <- bc[order(bc$distance, bc$feed_id), , drop = FALSE]
  common_denominator <- min(k, length(common))
  data.frame(k = k, n_a = nrow(a), n_b = nrow(b), n_common = length(common),
             top_a_n = length(top_a), top_b_n = length(top_b),
             top_overlap_n = intersection, overlap_denominator = denominator,
             top_overlap = if (denominator) intersection / denominator else NA_real_,
             top_jaccard = if (union) intersection / union else NA_real_,
             common_top_overlap = if (common_denominator) {
               length(intersect(head(common_a$feed_id, k), head(common_b$feed_id, k))) /
                 common_denominator
             } else NA_real_, spearman_common = rho)
}

# Brute-force oracle tests for partition_tibble()
# Run from /workspace via: Rscript scratch/test-partition.R
pkgload::load_all(quiet = TRUE)

# ---- Brute-force oracle ------------------------------------------------
# Enumerate every combination of admissible cuts, keep min max-shard
# bytes; ties -> lexicographically smallest cut vector.
lex_less <- function(a, b) {
  d <- which(a != b)
  length(d) > 0L && a[[d[[1L]]]] < b[[d[[1L]]]]
}

brute2 <- function(bytes, valid, k) {
  n <- length(bytes)
  pos <- which(valid)
  if (k == 1L) {
    return(list(cuts = integer(), optimal = sum(bytes)))
  }
  if (length(pos) < k - 1L) return(NULL)
  prefix <- c(0, cumsum(as.numeric(bytes)))
  best <- NULL
  # utils::combn() misinterprets a scalar m as 1:m, build the matrix
  # explicitly and enumerate its columns.
  combos <- if (length(pos) == 1L) {
    list(pos)
  } else {
    m <- utils::combn(pos, k - 1L, simplify = TRUE)
    lapply(seq_len(ncol(m)), function(j) m[, j])
  }
  for (cc in combos) {
    bounds <- c(0L, cc, n)
    t <- max(diff(prefix[bounds + 1L]))
    if (
      is.null(best) || t < best$optimal ||
        (t == best$optimal && lex_less(cc, best$cuts))
    ) {
      best <- list(cuts = cc, optimal = t)
    }
  }
  best
}

set.seed(42)
fails <- 0L
for (iter in 1:400) {
  n <- sample(2:10, 1L)
  bytes <- sample(1:9, n, replace = TRUE)
  keys_n <- sample(1:max(2L, n), 1L)
  key <- sample(letters[seq_len(keys_n)], n, replace = TRUE)
  if (runif(1L) < 0.3) key[sample(seq_len(n), sample(seq_len(n), 1L))] <- NA_character_

  df <- tibble(key = key, bytes = bytes, x = seq_len(n),
               f = factor(key), l = as.Date("2020-01-01") + seq_len(n))

  for (k in 1:n) {
    # oracle on admissible cuts (NA treated as a regular key)
    valid <- logical(n - 1L)
    if (n > 1L) {
      for (i in seq_len(n - 1L)) {
        left <- key[seq_len(i)]
        right <- key[i + seq_len(n - i)]
        # match() distinguishes NAs, intersect() would drop them.
        valid[[i]] <- all(is.na(match(left, right)))
      }
    }
    oracle <- brute2(bytes, valid, k)
    got <- tryCatch(
      partition_tibble(df, k = k, .bytes = bytes, .key = "key"),
      error = function(e) e
    )

    if (is.null(oracle)) {
      if (!inherits(got, "error")) {
        cat("FAIL iter", iter, "k", k, ": expected error, got result\n")
        fails <- fails + 1L
      }
      next
    }
    if (inherits(got, "error")) {
      cat("FAIL iter", iter, "k", k, ": unexpected error\n")
      print(got)
      fails <- fails + 1L
      next
    }

    cuts <- attr(got, "cuts")
    opt <- attr(got, "optimal_bytes")
    ok <- identical(cuts, oracle$cuts) &&
      isTRUE(all.equal(opt, oracle$optimal)) &&
      isTRUE(all.equal(max(got$bytes), oracle$optimal)) &&
      identical(got$row_start, c(1L, cuts + 1L)) &&
      identical(got$row_end, c(cuts, n)) &&
      sum(got$bytes) == sum(bytes) &&
      all(got$row_end - got$row_start + 1L >= 1L)

    # key integrity + type preservation + slices reconstruct input
    slices <- got$data
    ok <- ok && all(vapply(slices, is_tibble, logical(1))) &&
      identical(do.call(vctrs::vec_rbind, slices), df) &&
      identical(vapply(slices, function(z) class(z$x[[1L]]), character(1))[1L],
                "integer") &&
      is.factor(slices[[1L]]$f) &&
      inherits(slices[[1L]]$l, "Date")

    if (!ok) {
      cat("FAIL iter", iter, "n", n, "k", k, "\n")
      cat(" key:"); print(key)
      cat(" bytes:"); print(bytes)
      cat(" oracle cuts:"); print(oracle$cuts)
      cat(" got cuts:   "); print(cuts)
      cat(" oracle opt:", oracle$optimal, " got:", opt, "\n")
      fails <- fails + 1L
    }
  }
}

# No-key oracle against the unrestricted split-array problem
for (iter in 1:200) {
  n <- sample(2:12, 1L)
  bytes <- sample(1:20, n, replace = TRUE)
  df <- tibble(bytes = bytes, g = runif(n))
  for (k in 1:n) {
    oracle <- brute2(bytes, rep(TRUE, n - 1L), k)
    got <- partition_tibble(df, k = k, .bytes = "bytes")
    if (
      !identical(attr(got, "cuts"), oracle$cuts) ||
        !isTRUE(all.equal(attr(got, "optimal_bytes"), oracle$optimal))
    ) {
      cat("FAIL no-key iter", iter, "k", k, "\n")
      print(bytes)
      print(oracle); print(attr(got, "cuts"))
      fails <- fails + 1L
    }
  }
}

# Composite key (two columns) oracle
for (iter in 1:100) {
  n <- sample(2:9, 1L)
  bytes <- sample(1:8, n, replace = TRUE)
  k1 <- sample(letters[1:3], n, replace = TRUE)
  k2 <- sample(LETTERS[1:3], n, replace = TRUE)
  df <- tibble(k1 = k1, k2 = k2, bytes = bytes)
  combo <- paste(k1, k2, sep = "|")
  valid <- logical(n - 1L)
  for (i in seq_len(n - 1L)) {
    valid[[i]] <- all(is.na(match(combo[1:i], combo[(i + 1):n])))
  }
  for (k in 1:n) {
    oracle <- brute2(bytes, valid, k)
    got <- tryCatch(
      partition_tibble(df, k = k, .bytes = bytes, .key = c("k1", "k2")),
      error = function(e) e
    )
    if (is.null(oracle)) {
      if (!inherits(got, "error")) {
        cat("FAIL composite iter", iter, "k", k, ": expected error\n")
        fails <- fails + 1L
      }
    } else if (
      inherits(got, "error") ||
        !identical(attr(got, "cuts"), oracle$cuts) ||
        !isTRUE(all.equal(attr(got, "optimal_bytes"), oracle$optimal))
    ) {
      cat("FAIL composite iter", iter, "k", k, "\n")
      print(combo); print(oracle)
      fails <- fails + 1L
    }
  }
}

cat("\nTotal failures:", fails, "\n")
if (fails > 0L) quit(status = 1L)

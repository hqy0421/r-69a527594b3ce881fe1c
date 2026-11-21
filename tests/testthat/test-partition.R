# partition_tibble -------------------------------------------------------

test_that("basic split returns row ranges, bytes and typed slices", {
  df <- tibble(
    key = c("a", "b", "b", "a", "c"),
    bytes = c(3L, 1L, 4L, 2L, 5L),
    f = factor(c("x", "y", "y", "x", "z")),
    d = as.Date("2025-01-01") + 1:5
  )

  out <- partition_tibble(df, k = 2, .bytes = bytes, .key = "key")

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("shard", "row_start", "row_end", "bytes", "data"))
  expect_equal(out$shard, 1:2)
  # Only cut after row 4 is admissible because key "a" spans rows 1 and 4.
  expect_identical(attr(out, "cuts"), 4L)
  expect_identical(out$row_start, c(1L, 5L))
  expect_identical(out$row_end, c(4L, 5L))
  expect_equal(out$bytes, c(10, 5))
  expect_equal(attr(out, "optimal_bytes"), 10)

  # Every slice is a tibble keeping the original column types.
  expect_true(all(vapply(out$data, is_tibble, logical(1))))
  expect_s3_class(out$data[[1]]$f, "factor")
  expect_s3_class(out$data[[1]]$d, "Date")
  # Slices are contiguous, non-empty and reconstruct the input exactly.
  expect_identical(vec_rbind(!!!out$data), df)
})

test_that("without a key every boundary is admissible", {
  df <- tibble(bytes = c(7L, 2L, 5L, 10L, 4L))
  out <- partition_tibble(df, k = 3, .bytes = "bytes")

  expect_length(out$shard, 3)
  expect_equal(max(out$bytes), attr(out, "optimal_bytes"))
  # Contiguous non-empty ranges covering all rows once.
  expect_identical(out$row_start, c(1L, attr(out, "cuts") + 1L))
  expect_identical(out$row_end, c(attr(out, "cuts"), 5L))
  expect_equal(sum(out$bytes), sum(df$bytes))
})

test_that("k = 1 returns the whole table", {
  df <- tibble(key = c("a", "b", "a"), bytes = c(2L, 3L, 4L))
  out <- partition_tibble(df, k = 1, .bytes = bytes, .key = "key")

  expect_identical(attr(out, "cuts"), integer())
  expect_equal(attr(out, "optimal_bytes"), 9)
  expect_identical(out$row_start, 1L)
  expect_identical(out$row_end, 3L)
  expect_identical(out$data[[1]], df)
})

test_that("composite keys are treated as one unit", {
  df <- tibble(
    k1 = c("a", "a", "b", "b"),
    k2 = c("x", "y", "x", "y"),
    bytes = c(1L, 2L, 3L, 4L)
  )
  # All four composite keys are distinct, so every boundary is valid;
  # the solver still groups on the pair, not on either column alone.
  # Bytes 1,2,3,4: cut after row 3 gives shards 6 and 4 (max 6), the
  # optimum versus cut after row 2 (3 and 7, max 7).
  out <- partition_tibble(df, k = 2, .bytes = bytes, .key = c("k1", "k2"))
  expect_identical(attr(out, "cuts"), 3L)
  expect_equal(out$bytes, c(6, 4))

  # A repeated pair spanning non-adjacent rows blocks the cuts between.
  df2 <- tibble(
    k1 = c("a", "a", "b", "a"),
    k2 = c("x", "y", "x", "x"),
    bytes = c(1L, 2L, 3L, 4L)
  )
  expect_error(
    partition_tibble(df2, k = 2, .bytes = bytes, .key = c("k1", "k2")),
    "0 row boundaries are admissible"
  )
})

test_that("missing key values form one shared key", {
  df <- tibble(
    key = c(NA, "a", NA, "b"),
    bytes = c(1L, 2L, 3L, 4L)
  )
  # NA key spans rows 1 and 3, so only the cut after row 3 is admissible.
  out <- partition_tibble(df, k = 2, .bytes = bytes, .key = "key")
  expect_identical(attr(out, "cuts"), 3L)
})

test_that("reports the reason when no admissible split exists", {
  df <- tibble(key = c("a", "b", "a"), bytes = c(1L, 2L, 3L))
  # Key "a" spans the whole table, no boundary can separate two shards.
  expect_error(
    partition_tibble(df, k = 2, .bytes = bytes, .key = "key"),
    "0 row boundaries are admissible"
  )
})

test_that("ties resolve to the lexicographically leftmost cut vector", {
  # Equal byte sizes: every split has the same maximum, so the leftmost
  # cuts (2, 4) must win over (3, 4), (2, 5), etc.
  eq <- tibble(bytes = rep(1L, 6L))
  out <- partition_tibble(eq, k = 3, .bytes = "bytes")
  expect_identical(attr(out, "cuts"), c(2L, 4L))

  # With a key, only cuts 2 and 4 are admissible and both are optimal;
  # the leftmost cut (2) is chosen.
  df <- tibble(
    key = c("a", "a", "b", "b", "c", "c"),
    bytes = rep(1L, 6L)
  )
  out2 <- partition_tibble(df, k = 2, .bytes = bytes, .key = "key")
  expect_identical(attr(out2, "cuts"), 2L)
})

test_that("k equal to the number of rows isolates every row", {
  df <- tibble(bytes = c(2L, 5L, 3L))
  out <- partition_tibble(df, k = 3, .bytes = "bytes")
  expect_identical(attr(out, "cuts"), 1:2)
  expect_identical(out$row_start, 1:3)
  expect_identical(out$row_end, 1:3)
  expect_equal(out$bytes, c(2, 5, 3))
})

test_that("input validation gives clear errors", {
  df <- tibble(key = "a", bytes = 1L)

  expect_error(partition_tibble(df, k = 0, .bytes = bytes), "positive integer")
  expect_error(
    partition_tibble(df, k = 1.5, .bytes = bytes),
    "positive integer"
  )
  expect_error(
    partition_tibble(df, k = 2, .bytes = bytes),
    "must not exceed the number of rows"
  )
  expect_error(
    partition_tibble(df, k = 1, .bytes = missing_col),
    "Column `.bytes` must exist"
  )
  expect_error(
    partition_tibble(df, k = 1, .bytes = bytes + 1),
    "must be a column name"
  )
  expect_error(
    partition_tibble(df, k = 1, .bytes = bytes, .key = "nope"),
    "Can't find key column"
  )
  expect_error(
    partition_tibble(df, k = 1, .bytes = bytes, .key = 1),
    "character vector"
  )
  expect_error(partition_tibble("x", k = 1, .bytes = bytes), "data frame")
  expect_error(
    partition_tibble(tibble(bytes = integer()), k = 1, .bytes = bytes),
    "zero rows"
  )

  bad <- tibble(bytes = c(1L, 0L, 2L))
  expect_error(
    partition_tibble(bad, k = 1, .bytes = bytes),
    "positive numbers of bytes"
  )
  bad_na <- tibble(bytes = c(1L, NA, 2L))
  expect_error(
    partition_tibble(bad_na, k = 1, .bytes = bytes),
    "one finite byte count per row"
  )
})

# ---- Property tests against a brute-force oracle -----------------------

# Enumerate every combination of admissible cuts; keep the minimum
# maximum shard bytes and the lexicographically smallest cut vector.
brute_partition <- function(bytes, valid, k) {
  n <- length(bytes)
  pos <- which(valid)
  if (k == 1L) {
    return(list(cuts = integer(), optimal = sum(as.numeric(bytes))))
  }
  if (length(pos) < k - 1L) {
    return(NULL)
  }

  prefix <- c(0, cumsum(as.numeric(bytes)))
  if (length(pos) == 1L) {
    combos <- list(pos)
  } else {
    m <- utils::combn(pos, k - 1L, simplify = TRUE)
    combos <- lapply(seq_len(ncol(m)), function(j) m[, j])
  }

  lex_less <- function(a, b) {
    d <- which(a != b)
    length(d) > 0L && a[[d[[1L]]]] < b[[d[[1L]]]]
  }

  best <- NULL
  for (cc in combos) {
    t <- max(diff(prefix[c(0L, cc, n) + 1L]))
    if (
      is.null(best) ||
        t < best$optimal ||
        (t == best$optimal && lex_less(cc, best$cuts))
    ) {
      best <- list(cuts = cc, optimal = t)
    }
  }
  best
}

admissible_cuts <- function(key, n) {
  valid <- logical(n - 1L)
  for (i in seq_len(n - 1L)) {
    left <- key[seq_len(i)]
    right <- key[i + seq_len(n - i)]
    # match() keeps NAs distinct; intersect() would drop them.
    valid[[i]] <- all(is.na(match(left, right)))
  }
  valid
}

test_that("optimum and leftmost cuts match brute force (single key)", {
  set.seed(101)
  for (iter in 1:60) {
    n <- sample(2:9, 1L)
    bytes <- sample(1:9, n, replace = TRUE)
    key <- sample(letters[seq_len(sample(1:n, 1L))], n, replace = TRUE)
    if (runif(1L) < 0.4) {
      key[sample(seq_len(n), sample(seq_len(n), 1L))] <- NA_character_
    }
    df <- tibble(key = key, bytes = bytes)
    valid <- admissible_cuts(key, n)

    for (k in seq_len(n)) {
      oracle <- brute_partition(bytes, valid, k)
      got <- tryCatch(
        partition_tibble(df, k = k, .bytes = bytes, .key = "key"),
        error = identity
      )

      if (is.null(oracle)) {
        expect_s3_class(got, "error")
        next
      }
      expect_identical(attr(got, "cuts"), oracle$cuts)
      expect_equal(attr(got, "optimal_bytes"), oracle$optimal)
      expect_equal(max(got$bytes), oracle$optimal)
      expect_identical(vec_rbind(!!!got$data), df)
    }
  }
})

test_that("optimum matches brute force with no key constraint", {
  set.seed(202)
  for (iter in 1:40) {
    n <- sample(2:10, 1L)
    bytes <- sample(1:15, n, replace = TRUE)
    df <- tibble(bytes = bytes)
    valid <- rep(TRUE, n - 1L)
    for (k in seq_len(n)) {
      oracle <- brute_partition(bytes, valid, k)
      got <- partition_tibble(df, k = k, .bytes = "bytes")
      expect_identical(attr(got, "cuts"), oracle$cuts)
      expect_equal(attr(got, "optimal_bytes"), oracle$optimal)
    }
  }
})

test_that("optimum matches brute force for composite keys", {
  set.seed(303)
  for (iter in 1:40) {
    n <- sample(2:8, 1L)
    bytes <- sample(1:8, n, replace = TRUE)
    k1 <- sample(letters[1:3], n, replace = TRUE)
    k2 <- sample(LETTERS[1:3], n, replace = TRUE)
    df <- tibble(k1 = k1, k2 = k2, bytes = bytes)
    combo <- paste(k1, k2, sep = "|")
    valid <- admissible_cuts(combo, n)

    for (k in seq_len(n)) {
      oracle <- brute_partition(bytes, valid, k)
      got <- tryCatch(
        partition_tibble(df, k = k, .bytes = bytes, .key = c("k1", "k2")),
        error = identity
      )
      if (is.null(oracle)) {
        expect_s3_class(got, "error")
      } else {
        expect_identical(attr(got, "cuts"), oracle$cuts)
        expect_equal(attr(got, "optimal_bytes"), oracle$optimal)
      }
    }
  }
})

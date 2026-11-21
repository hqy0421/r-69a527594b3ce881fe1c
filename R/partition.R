#' Split a tibble into byte-bounded shards respecting business keys
#'
#' @description
#' Splits a tibble into exactly `k` non-empty, contiguous row shards,
#' minimizing the maximum total byte size of any shard.
#'
#' Rows sharing the same composite business key are always kept in the
#' same shard, even when they are not adjacent. Only cut positions where
#' no key spans across the boundary are allowed. Key first and last
#' positions are used to detect admissible cut positions, so no
#' combination of cut points is enumerated.
#'
#' The exact optimum is found with a binary search on the shard byte
#' budget. When several optimal splits exist, the split with the
#' lexicographically smallest cut positions (leftmost cuts first) is
#' returned.
#'
#' @param .data A data frame or tibble, with at least one row.
#' @param k A single positive integer, the number of non-empty shards.
#' @param .bytes The column (bare name or string) holding the positive
#'   number of bytes each row is expected to occupy.
#' @param .key Character vector of column names forming the composite
#'   business key. `NULL` (the default) imposes no key constraint, so
#'   every row boundary is a valid cut position. Missing key values are
#'   treated as one shared key.
#'
#' @return A tibble with one row per shard and the columns:
#'   * `shard` — shard number, from 1 to `k`.
#'   * `row_start`, `row_end` — one-based row range in `.data`,
#'     each shard is a contiguous, non-empty row segment.
#'   * `bytes` — total expected bytes of the shard, as a double.
#'   * `data` — list column of tibble slices, preserving the original
#'     column types and the tibble class.
#'
#'   The exact optimal value (minimum possible maximum shard bytes) is
#'   stored in attribute `optimal_bytes`, and the chosen cut positions
#'   (last row of each shard except the last) in attribute `cuts`.
#'
#' @family partitioning
#' @export
#' @examples
#' df <- tibble(
#'   key = c("a", "b", "b", "a", "c"),
#'   bytes = c(3L, 1L, 4L, 2L, 5L)
#' )
#'
#' # Two shards; rows of one key are never split apart
#' partition_tibble(df, k = 2, .bytes = bytes, .key = "key")
#'
#' # Without a key constraint every boundary is admissible
#' partition_tibble(df, k = 3, .bytes = "bytes")
#'
#' # No solution: key "a" spans rows 1 and 4, so three shards are impossible
#' try(partition_tibble(df, k = 3, .bytes = bytes, .key = "key"))
partition_tibble <- function(.data, k, .bytes, .key = NULL) {
  if (!is.data.frame(.data)) {
    abort_partition_not_data_frame()
  }

  n <- nrow(.data)
  if (n == 0L) {
    abort_partition_empty_input()
  }

  k <- check_partition_k(k, n)

  bytes_col <- partition_col_name(enquo(.bytes), ".bytes")
  bytes <- check_partition_bytes(.data[[bytes_col]], n)

  if (is.null(.key)) {
    # Without a key constraint every row boundary is a valid cut.
    valid <- rep(TRUE, n - 1L)
  } else {
    check_partition_key(.key, names(.data))
    valid <- partition_valid_cuts(.data[.key], n)
  }

  # Prefix sums, in double to avoid integer overflow.
  prefix <- c(0, cumsum(as.numeric(bytes)))
  total <- prefix[[n + 1L]]

  if (k == 1L) {
    cuts <- integer()
    optimal <- total
  } else {
    n_valid <- sum(valid)
    if (n_valid < k - 1L) {
      abort_partition_unsplittable(k, n_valid)
    }

    valid_pos <- which(valid)

    # Binary search on the smallest feasible shard byte budget.
    lo <- max(bytes)
    hi <- total
    while (lo < hi) {
      mid <- lo + (hi - lo) %/% 2
      g <- partition_min_segments(mid, prefix, valid_pos, n)[[1L]]
      if (!is.na(g) && g <= k) {
        hi <- mid
      } else {
        lo <- mid + 1
      }
    }
    optimal <- lo

    minseg <- partition_min_segments(
      optimal,
      prefix,
      valid_pos,
      n
    )
    reach <- attr(minseg, "reach")
    cuts <- partition_lex_cuts(
      k,
      reach,
      valid_pos,
      minseg,
      n
    )
  }

  row_start <- c(1L, cuts + 1L)
  row_end <- c(cuts, n)
  shard_bytes <- prefix[row_end + 1L] - prefix[row_start]
  data <- lapply(
    seq_along(row_start),
    function(i) vec_slice(.data, seq2(row_start[[i]], row_end[[i]]))
  )

  out <- tibble(
    shard = seq_len(k),
    row_start = row_start,
    row_end = row_end,
    bytes = shard_bytes,
    data = data
  )
  attr(out, "optimal_bytes") <- optimal
  attr(out, "cuts") <- cuts
  out
}

# Admissible cut positions ------------------------------------------------

#' Compute which row boundaries split no key
#'
#' A cut after row `i` is valid exactly when no key has its first
#' occurrence at or before `i` and its last occurrence after `i`.
#' Tracking open keys via first/last positions gives all valid cuts in
#' linear time.
#'
#' @return Logical vector of length `n - 1`, `TRUE` for valid cuts.
#' @noRd
partition_valid_cuts <- function(key_df, n) {
  if (n == 1L) {
    return(logical())
  }

  # vctrs grouping: missing values are treated as one group, so group
  # ids themselves are never NA.
  grp <- vctrs::vec_match(key_df, vctrs::vec_unique(key_df))
  rows <- seq_len(n)
  # First and last row of every group. The last occurrence maps through
  # the reversed vector: its position there is n + 1 - position here.
  first <- match(grp, grp)
  last <- n + 1L - match(grp, rev(grp))
  is_first <- first == rows
  is_last <- last == rows

  # Number of keys still open after each row is processed.
  open_after <- cumsum(is_first - is_last)
  open_after[seq_len(n - 1L)] == 0L
}

# Feasibility and optimal cuts -------------------------------------------

#' Minimum number of shards for a byte budget
#'
#' `partition_reach()` gives the furthest row reachable from every start
#' under the budget. The actual shard end is the last valid cut not
#' beyond that row, and a reverse sweep yields the minimum segment count
#' for every suffix.
#'
#' @return Integer vector of length `n + 1`; entry `s` is the minimum
#'   number of segments for rows `s..n`, `NA` when impossible under
#'   `budget`. Entry `n + 1` is 0.
#' @noRd
partition_min_segments <- function(budget, prefix, valid_pos, n) {
  reach <- partition_reach(budget, prefix, n)

  # Largest valid cut not beyond reach[s], for every s at once.
  # cut_rank[s] is the number of valid cuts at positions <= reach[s].
  cut_rank <- findInterval(reach, valid_pos)
  end <- integer(n)
  has_cut <- cut_rank >= 1L
  end[has_cut] <- valid_pos[cut_rank[has_cut]]

  # A cut only helps if it is not before the segment start; otherwise
  # the segment cannot be closed, unless it already reaches the end.
  s <- seq_len(n)
  end[end < s] <- 0L
  end[reach >= n] <- n

  minseg <- integer(n + 1L)
  minseg[[n + 1L]] <- 0L
  for (i in n:1L) {
    e <- end[[i]]
    if (e == 0L) {
      minseg[[i]] <- NA_integer_
    } else {
      minseg[[i]] <- minseg[[e + 1L]] + 1L
    }
  }
  attr(minseg, "reach") <- reach
  minseg
}

#' Furthest row reachable from every start under the byte budget
#'
#' The prefix sums are sorted, so the furthest row `j` satisfying
#' `prefix[j + 1] - prefix[s] <= budget` for every start `s` is found
#' with one vectorized `findInterval()` call rather than an R-level
#' loop. A row whose own size exceeds the budget cannot start a shard.
#' @noRd
partition_reach <- function(budget, prefix, n) {
  s <- seq_len(n)
  # Default left-closed/right-open intervals: an exact match on a prefix
  # boundary is counted (sum up to that row equals the budget).
  idx <- findInterval(prefix[s] + budget, prefix)
  reach <- idx - 1L
  # findInterval always counts row s itself; drop it when it does not
  # fit under the budget.
  over <- prefix[s + 1L] - prefix[s] > budget
  reach[over] <- s[over] - 1L
  reach
}

#' Leftmost lexicographic cut vector for the optimal budget
#'
#' For each shard in turn, pick the smallest admissible cut from which
#' the remaining suffix can still be split into the required number of
#' shards. Each cut is inspected at most once overall.
#' @noRd
partition_lex_cuts <- function(k, reach, valid_pos, minseg, n) {
  # Suffix counts of valid cuts by position: nvc_at[x] is 1 if a valid
  # cut exists after row x, else 0, for x in 1..n-1; nvc_ge[x] then
  # counts valid cuts at positions >= x.
  nvc_at <- integer(n)
  nvc_at[valid_pos] <- 1L
  nvc_ge <- c(rev(cumsum(rev(nvc_at[seq_len(n - 1L)]))), 0L)

  cuts <- integer(k - 1L)
  s <- 1L
  p_lo <- 1L
  for (j in seq_len(k - 1L)) {
    remaining <- k - j
    limit <- min(reach[[s]], n - 1L)

    # Valid cuts in [s, limit]; valid_pos is sorted.
    while (p_lo <= length(valid_pos) && valid_pos[[p_lo]] < s) {
      p_lo <- p_lo + 1L
    }
    p_hi <- p_lo - 1L
    while (p_hi < length(valid_pos) && valid_pos[[p_hi + 1L]] <= limit) {
      p_hi <- p_hi + 1L
    }

    chosen <- 0L
    for (p in p_lo:p_hi) {
      c <- valid_pos[[p]]
      suffix_segments <- minseg[[c + 1L]]
      if (
        !is.na(suffix_segments) &&
          suffix_segments <= remaining &&
          nvc_ge[[c + 1L]] >= remaining - 1L
      ) {
        chosen <- c
        p_lo <- p + 1L
        break
      }
    }

    # Feasibility of the optimal budget guarantees a candidate.
    if (chosen == 0L) {
      abort_partition_internal()
    }
    cuts[[j]] <- chosen
    s <- chosen + 1L
  }
  cuts
}

# Argument validation ----------------------------------------------------

check_partition_k <- function(k, n, call = caller_env()) {
  if (
    !is.numeric(k) || length(k) != 1L || is.na(k) || k <= 0 || k != floor(k)
  ) {
    tibble_abort(
      paste0(
        "`k` must be a single positive integer, not ",
        describe_argument(k),
        "."
      ),
      call = call
    )
  }

  k <- as.integer(k)
  if (k > n) {
    tibble_abort(
      c(
        paste0("Can't split ", n, " row(s) into ", k, " non-empty shards."),
        i = "`k` must not exceed the number of rows."
      ),
      call = call
    )
  }
  k
}

check_partition_bytes <- function(bytes, n, call = caller_env()) {
  if (is.null(bytes)) {
    tibble_abort(
      "Column `.bytes` must exist and contain byte counts.",
      call = call
    )
  }
  if (!is.numeric(bytes) || length(bytes) != n || anyNA(bytes)) {
    tibble_abort(
      paste0(
        "Column `.bytes` must be a numeric column with one finite ",
        "byte count per row."
      ),
      call = call
    )
  }
  if (any(bytes <= 0)) {
    tibble_abort(
      "Column `.bytes` must contain only positive numbers of bytes.",
      call = call
    )
  }
  bytes
}

check_partition_key <- function(.key, names, call = caller_env()) {
  if (!is.character(.key) || length(.key) == 0L || anyNA(.key)) {
    tibble_abort(
      "`.key` must be a character vector of column names, or NULL.",
      call = call
    )
  }
  missing <- setdiff(.key, names)
  if (has_length(missing)) {
    tibble_abort(
      pluralise_commas(
        "Can't find key column(s) ",
        tick(missing),
        " in `.data`."
      ),
      call = call
    )
  }
}

partition_col_name <- function(quo, arg, call = caller_env()) {
  name <- tryCatch(as_name(quo), error = function(e) NULL)
  if (is.null(name)) {
    tibble_abort(
      paste0("`", arg, "` must be a column name, not an expression."),
      call = call
    )
  }
  name
}

describe_argument <- function(x) {
  if (length(x) == 0L) {
    return(paste0("an empty ", typeof(x)))
  }
  if (length(x) == 1L) {
    return(paste0("a ", typeof(x), " (", x, ")"))
  }
  paste0("a ", typeof(x), " vector of length ", length(x))
}

# Errors -----------------------------------------------------------------

abort_partition_not_data_frame <- function(call = caller_env()) {
  tibble_abort("`.data` must be a data frame or tibble.", call = call)
}

abort_partition_empty_input <- function(call = caller_env()) {
  tibble_abort(
    c(
      "Can't partition a tibble with zero rows.",
      i = "Every shard must be non-empty."
    ),
    call = call
  )
}

abort_partition_unsplittable <- function(k, n_valid, call = caller_env()) {
  cuts_needed <- k - 1L
  tibble_abort(
    c(
      paste0(
        "Can't split the tibble into ",
        k,
        " non-empty shards while keeping each business key in one shard."
      ),
      x = paste0(
        n_valid,
        " row boundar",
        if (n_valid == 1L) "y is" else "ies are",
        " admissible, but ",
        cuts_needed,
        " cut",
        if (cuts_needed == 1L) " is" else "s are",
        " needed."
      ),
      i = "Some key spans rows across every other boundary."
    ),
    call = call
  )
}

abort_partition_internal <- function(call = caller_env()) {
  tibble_abort("Internal error in partition_tibble().", call = call)
}

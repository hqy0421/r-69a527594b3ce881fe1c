# Split a tibble into byte-bounded shards respecting business keys

Splits a tibble into exactly `k` non-empty, contiguous row shards,
minimizing the maximum total byte size of any shard.

Rows sharing the same composite business key are always kept in the same
shard, even when they are not adjacent. Only cut positions where no key
spans across the boundary are allowed. Key first and last positions are
used to detect admissible cut positions, so no combination of cut points
is enumerated.

The exact optimum is found with a binary search on the shard byte
budget. When several optimal splits exist, the split with the
lexicographically smallest cut positions (leftmost cuts first) is
returned.

## Usage

``` r
partition_tibble(.data, k, .bytes, .key = NULL)
```

## Arguments

- .data:

  A data frame or tibble, with at least one row.

- k:

  A single positive integer, the number of non-empty shards.

- .bytes:

  The column (bare name or string) holding the positive number of bytes
  each row is expected to occupy.

- .key:

  Character vector of column names forming the composite business key.
  `NULL` (the default) imposes no key constraint, so every row boundary
  is a valid cut position. Missing key values are treated as one shared
  key.

## Value

A tibble with one row per shard and the columns:

- `shard` — shard number, from 1 to `k`.

- `row_start`, `row_end` — one-based row range in `.data`, each shard is
  a contiguous, non-empty row segment.

- `bytes` — total expected bytes of the shard, as a double.

- `data` — list column of tibble slices, preserving the original column
  types and the tibble class.

The exact optimal value (minimum possible maximum shard bytes) is stored
in attribute `optimal_bytes`, and the chosen cut positions (last row of
each shard except the last) in attribute `cuts`.

## Examples

``` r
df <- tibble(
  key = c("a", "b", "b", "a", "c"),
  bytes = c(3L, 1L, 4L, 2L, 5L)
)

# Two shards; rows of one key are never split apart
partition_tibble(df, k = 2, .bytes = bytes, .key = "key")
#> # A tibble: 2 × 5
#>   shard row_start row_end bytes data            
#>   <int>     <int>   <int> <dbl> <list>          
#> 1     1         1       4    10 <tibble [4 × 2]>
#> 2     2         5       5     5 <tibble [1 × 2]>

# Without a key constraint every boundary is admissible
partition_tibble(df, k = 3, .bytes = "bytes")
#> # A tibble: 3 × 5
#>   shard row_start row_end bytes data            
#>   <int>     <int>   <int> <dbl> <list>          
#> 1     1         1       2     4 <tibble [2 × 2]>
#> 2     2         3       4     6 <tibble [2 × 2]>
#> 3     3         5       5     5 <tibble [1 × 2]>

# No solution: key "a" spans rows 1 and 4, so three shards are impossible
try(partition_tibble(df, k = 3, .bytes = bytes, .key = "key"))
#> Error in partition_tibble(df, k = 3, .bytes = bytes, .key = "key") : 
#>   Can't split the tibble into 3 non-empty shards while keeping
#> each business key in one shard.
#> ✖ 1 row boundary is admissible, but 2 cuts are needed.
#> ℹ Some key spans rows across every other boundary.
```

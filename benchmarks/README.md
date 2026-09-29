# Distribution memory benchmarks

Run from the repository root with R and `pkgload` installed:

```sh
Rscript --vanilla benchmarks/distributions.R . /tmp/rvec-bench-current
```

The final two arguments optionally set the number of observations (categories
for multinomial functions) and draws. Defaults are 1,000 of each:

```sh
Rscript --vanilla benchmarks/distributions.R . /tmp/rvec-bench-large 10000 1000
```

Large runs can require several GB when testing older implementations. Start
with the defaults. Each case runs in a fresh R process, loads the requested
package source tree, constructs inputs before measurement, resets the garbage
collector's high-water marks, and makes one distribution call with seed 42.

The cases cover random generation, density evaluation, conversion from negative
binomial `mu`, and multinomial evaluation with supplied and default size.
`shared` uses ordinary parameter vectors, `single` uses one-draw rvecs, and
`full` uses parameters with every draw stored. Other inputs establish the same
output draw count across layouts. Poisson has only one parameter, so its
one-draw layout is omitted: explicitly requesting more draws for a one-draw
rvec is an error under the existing interface.

The output directory contains:

- `results.csv`: elapsed time, output size, and peak vector-heap growth in
  decimal MB, excluding input construction and package loading.
- `session.txt`: source directory, run time, and R/package versions.

Peak vector-heap growth is the increase in R's Vcell high-water mark over the
live heap immediately before the call. It is **not** process RSS, total memory,
or cumulative allocation. Garbage-collection timing affects it; reductions in
allocations do not always reduce this measurement for small inputs. Timings
include first-call overhead and should not be treated as precise speed claims.
Repeat runs on the same machine, R version, and dependency versions when
comparing changes. Correctness and RNG compatibility are covered by the tests,
not inferred from memory measurements.

## Comparing the pre-refactor implementation

The last revision before memory refactoring is `0df93c8` (it already contains
the draw-count bug fix). Export it into a separate directory, then run the
same benchmark script against that source tree:

```sh
mkdir -p /tmp/rvec-before-memory
git archive 0df93c8 | tar -x -C /tmp/rvec-before-memory
Rscript --vanilla benchmarks/distributions.R /tmp/rvec-before-memory /tmp/rvec-bench-before
Rscript --vanilla benchmarks/distributions.R . /tmp/rvec-bench-after
```

Keep the two `results.csv` and `session.txt` pairs together with the exact Git
revision and any uncommitted changes for each source tree. Compare rows by
`case`, `layout`, `observations`, and `draws`. The same script can benchmark any
future checkout supporting these public functions. All benchmark files are
excluded from the package build.

A recorded before/after run is available in `results/`, including its source
revisions and session information.

## Binary arithmetic

`arithmetic.R` measures one addition in a fresh process. It accepts the package
source directory, layout (`full`, `single`, `ordinary`, or `row`), operand type
(`double`, `integer`, or `logical`), operand order (`forward` or `reverse`), and
output CSV path. Every case produces 1,000 observations by 1,000 draws. `row`
uses one observation with 1,000 draws and tests observation recycling.

```sh
Rscript --vanilla benchmarks/arithmetic.R . single double forward /tmp/arith-after.csv
```

The CSV records the same memory and timing metrics as the distribution
benchmark. A companion `.session.txt` records dependencies and a checksum of
`R/vec_arith.R`. Inputs are created before measurement. See the caveats above
about peak heap growth and timings. The pre-arithmetic-refactor revision is
`0f1695e`; export it to another directory and supply that directory as the first
argument to compare it with the working tree. Run all layouts/types/orders in
separate processes, and retain the source revisions with the results.

## Constructors, casts, comparisons, and summaries

`common-operations.R` measures a single double-input operation in a fresh
process, using 1,000 observations and 1,000 draws:

```sh
Rscript --vanilla benchmarks/common-operations.R . compare_single /tmp/common-after.csv
```

Cases are `constructor`, `cast`, `compare_single`, `compare_ordinary`,
`draws_mean`, and `sd`. Export revision `d08797c` and supply that directory
instead of `.` for the baseline. The script collects garbage before resetting
high-water marks so dead objects from input setup do not dominate small
measurements. Output size can exceed additional allocation: same-type
constructors and casts can share existing matrix storage until modification.
A session file is written alongside each CSV. The memory/timing caveats above
also apply here.

## Conditional selection

`if-else.R` measures `if_else_rvec()` with ordinary, one-draw, or full rvec
`false` and `missing` branches. The `true` branch is ordinary in every case,
and the condition contains an even mix of true, false, and missing values.
Each case produces 1,000 observations by 1,000 draws in a fresh process:

```sh
Rscript --vanilla benchmarks/if-else.R . ordinary /tmp/if-else-after.csv
```

Export revision `16cb8f3` and supply that directory for the baseline. The CSV
and companion session file use the same metrics and caveats as the other
benchmarks above.

## Covariance

`covariance.R` measures covariance between two full rvecs or between a full
rvec and an ordinary vector. Each case uses 1,000 observations and 1,000 draws
in a fresh process:

```sh
Rscript --vanilla benchmarks/covariance.R . rvec_rvec /tmp/covariance-after.csv
```

Cases are `rvec_rvec` and `rvec_vector`. Export revision `1521441` and supply
that directory for the baseline. The CSV and companion session file use the
same metrics and caveats as the other benchmarks above.

## Weighted summaries

`weighted-summaries.R` measures a weighted summary with rvec weights and
ordinary or full-rvec values. Each case uses 1,000 observations and 1,000 draws
in a fresh process:

```sh
Rscript --vanilla benchmarks/weighted-summaries.R . mean ordinary /tmp/weighted-after.csv
```

Summaries are `mean`, `median`, `mad`, `var`, and `sd`; cases are `ordinary`
and `full`. Export revision `8e9bf5e` and supply that directory for the baseline.
The CSV and companion session file use the same metrics and caveats as the
other benchmarks above. One-draw alignment is covered by regression tests;
the baseline errors on those inputs, so they are not benchmark comparisons.

## Logical predicates

`logical-predicates.R` measures missingness and finiteness predicates on a
logical rvec with 1,000 observations and 1,000 draws, including `NA` values.
Run each case in a fresh process:

```sh
Rscript --vanilla benchmarks/logical-predicates.R . is.finite /tmp/logical-after.csv
```

Cases are `is.nan`, `is.finite`, `is.infinite`, and the unchanged `is.na`
control. Export revision `84872e8` and supply that directory for the baseline.
The CSV reports the usual peak vector-heap growth, elapsed time, and output
size. A separate warmed call uses `Rprofmem()` to record allocations exceeding
1,000,000 bytes in a companion `.allocations.txt` file. The CSV's
`allocations_over_1mb_mb` is their sum in decimal MB, not total allocation or
peak memory. This requires R with memory-profiling support. A companion
`.session.txt` records the environment and math/missingness source checksums.

## Logical math

`logical-math.R` measures mathematical operations on a logical rvec with
1,000 observations and 1,000 draws, including `NA` values. Run each case in a
fresh process:

```sh
Rscript --vanilla benchmarks/logical-math.R . abs /tmp/logical-math-after.csv
```

Cases are `abs`, `sqrt`, `cumsum`, `sum`, and the unchanged `is.finite` control.
Export revision `1b8c0cf` and supply that directory for the baseline. The CSV
and companion files use the same metrics as `logical-predicates.R`, including
allocation tracing of a separate warmed call with a 1,000,000-byte threshold.
The `sum` case includes the existing Summary dispatch overhead; this change
only reduces the logical-to-integer conversion inside the math method.

## Summary dispatch

`summary-dispatch.R` measures a single unnamed rvec passed to `sum`, `prod`,
`any`, or `all`. Each case uses 1,000 observations and 1,000 draws, including
`NA` values. Run each summary/type combination in a fresh process:

```sh
Rscript --vanilla benchmarks/summary-dispatch.R . sum dbl /tmp/summary-after.csv
```

Types are `dbl`, `int`, and `lgl`. Export revision `65bd57b` and supply that
directory for the baseline. The CSV and companion files use the same metrics
as `logical-predicates.R`, including allocation tracing of a separate warmed
call with a 1,000,000-byte threshold. Named and multiple inputs and custom
subclasses retain the previous dispatch and are covered by regression tests.

## Matrix multiplication investigation

`matrix-multiplication.R` compares the current package paths with experimental
alternatives. The alternatives are local to the benchmark and are not package
implementations. Run each case in a fresh process against revision `20efcc3`:

```sh
Rscript --vanilla benchmarks/matrix-multiplication.R . dot 1000 1000 /tmp/dot.csv
Rscript --vanilla benchmarks/matrix-multiplication.R . dot_stream 1000 1000 /tmp/dot-stream.csv
Rscript --vanilla benchmarks/matrix-multiplication.R . sparse_left 1000 100 /tmp/sparse-left.csv
Rscript --vanilla benchmarks/matrix-multiplication.R . sparse_left_direct 1000 100 /tmp/sparse-left-direct.csv
```

`dot_stream` computes one draw's product and sum at a time, using double rvecs
with equal dimensions. Repeat `dot` and `dot_stream` with 10,000 observations
for the larger recorded case. `sparse_right` and `sparse_right_direct` cover
the other operand order. Sparse cases use a tridiagonal coefficient matrix
and finite double rvec data. Metrics and companion session files follow the
other peak vector-heap benchmarks.

Direct sparse multiplication is not behaviorally interchangeable with the
current implementation. For example:

```r
m <- Matrix::Diagonal(2L)
x <- matrix(c(1, Inf), nrow = 2L)
as.matrix(m) %*% x  # NaN, Inf: current semantics
as.matrix(m %*% x)  # 1, Inf: skips the implicit zero times Inf
```

The same distinction occurs in the other operand order. Streaming dot products
also failed to reduce peak vector-heap growth in the recorded cases. These
results therefore document an investigation, not a shipped memory improvement.

## Expansion from rvecs

`expansion.R` measures `expand_from_rvec()` on a data frame with 1,000 rows and
1,000 draws. Run each type in a fresh process:

```sh
Rscript --vanilla benchmarks/expansion.R . dbl /tmp/expansion-after.csv
```

Types are `dbl`, `int`, `lgl`, `chr`, and `mixed` (one column of each type).
Export revision `8b9e172` and supply that directory for the baseline. Inputs
include missing values and are constructed outside measurement. The CSV and
companion files use the same metrics as `logical-predicates.R`, including
allocation tracing of a separate warmed call above a 1,000,000-byte threshold.
The measurement includes the full expansion, including repeated identifiers
and the draw column, not only the reshaping of value columns.

## Remaining memory candidates

`remaining-memory.R` contains investigation-only prototypes for collapse,
pooling, modes, and formatting. They are not package implementations. Run
against revision `bbb8890`, with each case in a fresh process:

```sh
Rscript --vanilla benchmarks/remaining-memory.R . pool old repeated /tmp/pool-old.csv
Rscript --vanilla benchmarks/remaining-memory.R . pool new repeated /tmp/pool-new.csv
Rscript --vanilla benchmarks/remaining-memory.R . mode new unique /tmp/mode-new.csv
```

Cases are `pool`, `mode`, `format` (character summaries), `logical_format`,
and `collapse`. Variants are `old` (current package) and `new` (prototype).
Each input matrix has 1,000 rows and 1,000 draws. The `repeated` scenario uses
97 distinct integers, or their character equivalents; logical formatting uses
TRUE/FALSE/NA. `unique` gives each cell a distinct integer or character value
and is recorded for mode and character formatting.

Prototypes change pooling dimensions directly, process one row table at a time
for modes and character formatting, pass logical data directly to rowMeans2,
or initialize collapse's first matrix with the first value column's storage
type. Collapse continues to reuse that matrix so existing cross-column type
promotion is preserved. These experiments do not establish compatibility with
all supported dependency versions or extension classes.

The CSV records peak vector-heap growth and the sum of allocations exceeding
1,000,000 bytes in a separate warmed call; these are different metrics.
`identical_to_current` compares results outside measurement. Companion files
record allocation traces, source checksums, and the session environment.

## Pooling implementation and formatting control

`pooling-formatting.R` benchmarks public pooling calls and unchanged logical
formatting. Use revision `bbb8890` as the before baseline, with each case in a
fresh process:

```sh
Rscript --vanilla benchmarks/pooling-formatting.R . pool int /tmp/pool-after.csv
Rscript --vanilla benchmarks/pooling-formatting.R . pool_grouped dbl /tmp/grouped-after.csv
Rscript --vanilla benchmarks/pooling-formatting.R . logical_format lgl /tmp/format-control.csv
```

Pooling types are `dbl`, `int`, `lgl`, and `chr`; `logical_format` requires
`lgl`. Inputs have 1,000 observations and 1,000 draws. `pool_grouped` pools
within ten equally sized groups using `by`. The CSV and companion files use
the same peak and warmed large-allocation metrics as the other benchmarks.

Logical formatting remains unchanged: direct logical rowMeans2 can round
slightly differently from the existing refined double calculation. With
matrixStats 1.5.0 in the recorded environment:

```r
m <- matrix(c(rep(TRUE, 14285L), rep(FALSE, 85715L)), nrow = 1L)
formatC(matrixStats::rowMeans2(1 * m, na.rm = TRUE), format = "fg") # "0.1428"
formatC(matrixStats::rowMeans2(m, na.rm = TRUE), format = "fg")     # "0.1429"
```

The previous investigation's logical-format prototype is therefore not a
compatible replacement; its allocation savings must not be reported as shipped.

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

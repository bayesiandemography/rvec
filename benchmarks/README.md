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

# Recorded comparison

These runs used 1,000 observations/categories and 1,000 draws on the same
machine and R installation. The distribution sources were:

- Before: `0df93c8`, exported into a separate source directory.
- After: `0cbb332`, with only NEWS and benchmark files modified.

See the session files for R and dependency versions. These are example
measurements, not test thresholds. Some before/after processes overlapped;
do not use their timings to draw speed conclusions. Compare memory by matching
case and layout. Both runs completed all 20 cases.

At this scale, gamma and negative binomial cases show clear reductions, while
multinomial peak heap growth changes little. Larger multinomial cases expose
reductions more clearly, as explained in the parent README. R's garbage
collector makes peak growth sensitive to input size and the heap state.

## Arithmetic comparison

`arithmetic.csv` contains 48 measurements: before and after for four layouts,
three numeric types, and both operand orders. Before uses `0f1695e`; after uses
the uncommitted binary-arithmetic implementation on `arith-refactor` based on
that revision. Companion arithmetic session files record source checksums and
dependencies. The benchmark cases ran sequentially, alongside package checks;
timings are indicative only. All cases completed successfully.

## Common operations comparison

`common-operations.csv` records six cases before and after the constructors,
casts, comparisons, and double-input summary changes. Before uses `d08797c`;
after uses the uncommitted changes based on that revision on `arith-refactor`.
Session details are in `common-before-session.txt` and
`common-after-session.txt`. Each case uses a fresh process and collects input
setup garbage before resetting high-water marks. Same-type outputs may share
input storage; their object size is not additional allocated memory. All 12
cases completed successfully. Timings are indicative only.

## Conditional-selection comparison

`if-else.csv` records ordinary, one-draw, and full-rvec branch layouts before
and after the `if_else_rvec()` change. Before uses `16cb8f3`; after uses the
uncommitted implementation based on that revision. Session details are in
`if-else-before-session.txt` and `if-else-after-session.txt`.

For 1,000 observations by 1,000 draws, peak vector-heap growth fell from about
41.3 MB to 34.1 MB for both ordinary and one-draw `false` and `missing`
branches. The full-rvec case remained at about 45.4 MB. Timings are indicative
only.

## Covariance comparison

`covariance.csv` records covariance between two rvecs and between an rvec and
an ordinary vector. Before uses `1521441`; after uses the uncommitted
implementation based on that revision. Session details are in
`covariance-before-session.txt` and `covariance-after-session.txt`.

For 1,000 observations by 1,000 draws, peak vector-heap growth fell from about
40.2 MB to 28.5 MB for two rvecs and from about 20.5 MB to 16.5 MB for an rvec
and ordinary vector. Timings are indicative only.

## Weighted-summary comparison

`weighted-summaries.csv` records all five weighted summaries with rvec weights
and ordinary or full-rvec values. Before uses `8e9bf5e`; after uses the
uncommitted implementation based on that revision. Representative session
files (from the ordinary-value mean case) record each source checksum and the
common environment in `weighted-summaries-before-session.txt` and
`weighted-summaries-after-session.txt`.

For 1,000 observations by 1,000 draws, ordinary-value means and medians reduced
peak vector-heap growth from about 32.1 MB to 24.1 MB. Ordinary-value MAD,
variance, and standard deviation stayed near 44.1 MB; removing the repeated
8 MB double matrix does not necessarily reduce the garbage-collector high-water
mark. Full-rvec cases were essentially unchanged. Timings are indicative only.

## Logical-predicate comparison

`logical-predicates.csv` compares `84872e8` with the uncommitted implementation
based on that revision. Representative session files from `is.finite` record
source checksums and the common environment in
`logical-predicates-before-session.txt` and
`logical-predicates-after-session.txt`.

For a 1,000 by 1,000 logical rvec, peak vector-heap growth for `is.nan`,
`is.finite`, and `is.infinite` fell from about 16.0 MB to 4.0 MB. Allocation
tracing of a separate warmed call likewise fell from four approximately 4 MB
allocations to one: the three integer-conversion temporaries are eliminated,
leaving the logical result. The allocation metric includes only allocations
exceeding 1,000,000 bytes. The `is.na` control stayed at about 4.0 MB; existing
constructor fast paths already avoid extra matrix copies there. Timings are
indicative only.

## Logical-math comparison

`logical-math.csv` compares `1b8c0cf` with the uncommitted implementation based
on that revision. Representative session files from `abs` record the math
source checksum and common environment in `logical-math-before-session.txt`
and `logical-math-after-session.txt`.

For a 1,000 by 1,000 logical rvec, peak vector-heap growth fell from about
16.0 MB to 8.0 MB for `abs` and `cumsum`, from 20.0 MB to 12.0 MB for `sqrt`,
and from 16.1 MB to 8.1 MB for `sum`. Allocation tracing of a separate warmed
call confirmed removal of two approximately 4 MB conversion temporaries.
The allocation metric includes only allocations exceeding 1,000,000 bytes.
The `is.finite` control stayed at about 4.0 MB. Timings are indicative only.

## Summary-dispatch comparison

`summary-dispatch.csv` compares `65bd57b` with the uncommitted implementation
based on that revision. Representative session files from the double-input
`sum` case record the math source checksum and common environment in
`summary-dispatch-before-session.txt` and `summary-dispatch-after-session.txt`.

For one unnamed 1,000 by 1,000 rvec, peak vector-heap growth decreased by
about 8.1 MB for double inputs and 4.1 MB for integer and logical inputs across
all four summaries. Double `sum` dropped from about 8.1 MB to 0.03 MB; logical
`sum` dropped from about 8.1 MB to 4.1 MB because its integer conversion remains.
`prod` retains its existing numerical algorithm and additional allocations.

Tracing a separate warmed call confirmed removal of one full input-matrix
allocation (8 MB double, 4 MB integer/logical). The allocation metric only
counts allocations exceeding 1,000,000 bytes; zero does not mean the call
allocates nothing. Timings are indicative only.

## Matrix-multiplication investigation

`matrix-multiplication-investigation.csv` records current paths at `20efcc3`
and benchmark-local prototypes, not before/after package implementations.
`matrix-multiplication-session.txt` records the common environment and unchanged
matrix source checksum, taken from the 1,000-observation dot-product case.

For two double rvecs with 1,000 draws, streaming one draw at a time increased
peak vector-heap growth from 8.1 MB to 24.3 MB at 1,000 observations, and from
80.1 MB to 163.1 MB at 10,000 observations. The streamed results were identical
for these inputs, but the prototype allocated extracted columns and temporary
products and ran slower. Live temporary size alone does not predict R's
vector-heap high-water mark.

For a sparse 1,000 by 1,000 tridiagonal matrix and 100 draws, direct sparse
multiplication reduced peak growth from about 9.0 MB to 1.8 MB with the matrix
on the left, and from 9.8 MB to 2.6 MB with it on the right. However, direct
sparse arithmetic skips implicit zero products and changes NA/NaN/Inf behavior;
it cannot replace the current dense arithmetic unconditionally. The README
contains a minimal reproducer, and regression tests protect both operand orders.
No production optimization was retained. Timings are indicative only.

## Expansion comparison

`expansion.csv` compares `8b9e172` with the uncommitted implementation based
on that revision. Representative session files from the double-value case
record the reshaping source checksum and common environment in
`expansion-before-session.txt` and `expansion-after-session.txt`.

For 1,000 rows by 1,000 draws, peak vector-heap growth fell from about 40.0 MB
to 32.0 MB for a double or character value column, and from 32.0 MB to 28.0 MB
for integer or logical values. With one column of each type, the recorded peak
fell from 88.0 MB to 44.0 MB. Garbage-collection timing affects that mixed-case
peak; warmed allocation tracing showed a 24 MB reduction, from 96 MB to 72 MB
in allocations above 1,000,000 bytes. This matches one avoided full-value copy
per column. Timings are indicative only.

## Remaining-candidate investigation

`remaining-memory.csv` compares current implementations at `bbb8890` with
benchmark-local prototypes; no package implementation changed. The common
environment and source checksums are in `remaining-memory-session.txt`.
Each input matrix has 1,000 rows and 1,000 draws.

- Pooling an integer rvec: changing dimensions directly reduced peak growth
  from 4.04 MB to 0.04 MB and removed one 4 MB allocation.
- Logical summary formatting: passing logical data directly to rowMeans2
  reduced peak growth from 8.31 MB to 0.31 MB and removed the 8 MB double matrix.
- Modes: streaming row tables left the repeated-value case at about 34.1 MB.
  The distinct-value case showed a smaller peak; see the CSV. Earlier exploratory
  harnesses produced different peaks, so the size of the benefit is sensitive
  to garbage-collection timing and string-table allocation.
- Character summary formatting: streaming removed an 8 MB allocation but
  left peak growth essentially unchanged (40.1 MB repeated, 45.3 MB distinct).
- Collapse: typing only the initial matrix removed a 4 MB allocation, while
  peak growth remained 92.5 MB. Subsequent columns retain the current type
  promotion rather than changing behavior based on a memory-only refactor.

The large-allocation metric excludes allocations of 1,000,000 bytes or less;
zero does not mean no allocation. All benchmark results matched current output.
Temporary compatibility checks also matched values, names, warnings, and error
classes/messages across 216 pooling cases, 432 mode cases, 54 character-format
cases, 54 logical-format cases, and 144 collapse cases. Pooling comparisons
included checking that modifying a result did not modify its input.
These prototypes still need permanent tests and full package validation before
being adopted. Timings are indicative only.

## Pooling implementation

`pooling-formatting.csv` compares `bbb8890` with the uncommitted pooling
implementation based on that revision. Representative session files from the
integer ungrouped case record pooling/formatting source checksums and the
common environment in `pooling-formatting-before-session.txt` and
`pooling-formatting-after-session.txt`.

For 1,000 observations and 1,000 draws, ungrouped public pooling reduced peak
vector-heap growth from about 8.8 MB to 0.8 MB for doubles/characters and from
4.8 MB to 0.8 MB for integers/logicals. Pooling within ten groups fell from
25.3 MB to 17.3 MB and from 13.4 MB to 9.4 MB respectively. These public-call
measurements include overhead excluded by the earlier helper-only prototypes.
Allocation tracing confirmed removal of the reconstruction copy.

Logical formatting is unchanged at about 8.3 MB. Broader proportion tests
found that the earlier direct-logical prototype changes rounding: 14,285 TRUE
values out of 100,000 format as `p=0.1428` with the current double/refined
calculation but `p=0.1429` with direct logical input in this environment. The
prototype was not adopted, and the existing calculation has a regression test.
Timings are indicative only.

## Draw-mode implementation

`draws-mode.csv` compares `0f9f999` with the uncommitted implementation based
on that revision. Representative session files from the distinct-integer case
record the draws source checksum and common environment in
`draws-mode-before-session.txt` and `draws-mode-after-session.txt`.

For 1,000 observations and 1,000 draws with distinct numeric values, peak
vector-heap growth fell from 61.0 MB to 44.1 MB for integers and from 57.0 MB
to 40.1 MB for doubles. Repeated-value and character cases showed essentially
unchanged peaks. The implementation avoids retaining all row tables and the
transpose performed by apply; the effect on measured peak depends on type,
cardinality, and garbage collection. Timings are indicative only.

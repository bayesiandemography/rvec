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

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

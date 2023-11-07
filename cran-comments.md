## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The win-builder check log contains the following note:

> Possibly misspelled words in DESCRIPTION:
>   Rvecs (14:54)
>   rvec (12:47)
>   rvecs (12:37)

These aren't actually misspellings - "rvec" is the name of the new data structure implemented by the rvec package.


One more small thing: I have currently commented out the

{os: ubuntu-latest,   r: 'oldrel-1'}

check in the standard github R-CMD-check. The oldrel-1 check currently
fails because it use a pre-4.3.0 version of R. The package 'rvec'
depends on R >= 4.3.0, because it needs the new matrixOps generic
availabe in R 4.3.0.

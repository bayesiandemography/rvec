
## 8 November 2023

Thank you so much for the instant feedback.

> Please proof-read your description text.
> Currently it reads: "... An rvec holds holds multiple ..."
> Probably it should be: "... An rvec holds multiple ..."


Thank you for spotting this!! I have removed the duplicated "hold".


> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title")

At present the only description of the methods is the vignette
bundled with the package. If I write up a description elsewhere I will
include it in the DESCRIPTION in a future version of the package.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>      missing.Rd: \value
> 
> Please fix and resubmit.


Thank you for pointing this out. I have added the 'value' section.
Doing so prompted me to make a small change to the behaviour of
the `anyNA` methods, and to rewrite parts of the help 
for the methods for `anyNA`, `is.na`, etc. I have noted these changes
in the NEWS file, and bumped the version number to 0.0.6.


Best wishes
John





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

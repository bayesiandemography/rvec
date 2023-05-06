

#' Weights of birds
#'
#' A simulated posterior sample from a model
#' of bird weights (in kilograms)
#'
#' @format A matrix with 3 rows and 10 columns.
#' - Each row records estimates for a type of bird
#' (chicken, duck goose)
#' - Each column represents one random draw
#' from the joint distribution of weights.
"birdweight"


#' Dice rolls
#'
#' Data on outcomes from 10 rolls of a dice
#'
#' @format A matrix with 6 rows and 10 columns.
#' - Each row records outcomes for a particular value.
#' - Each column records outcomes for a particular draw.
"dicerolls"


#' Divorce rates in New Zealand
#'
#' Posterior sample from a model of divorce rates
#' in New Zealand.
#'
#' @format A tibble with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+.
#' - `sex`: `"Female"` or `"Male"`.
#' - `time`: Calendar year.
#' - `sim`: Index for draw.
#' - `rate`: Divorce rate.
#'
#' @source Derived from data in tables "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)" and
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website.
#' Divorce data downloaded on 22 March 2023, and
#' population data downloaded on 26 March 2023.
"divorce"

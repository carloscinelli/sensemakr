#' Data from survey of Darfurian refugees in eastern Chad.
#' @description
#' Data on attitudes of Darfurian refugees in eastern Chad. The main "treatment"
#' variable is \code{directharm}, which indicates that the individual was physically
#' injured during attacks on villages in Darfur, largely between 2003 and 2004.
#' The main outcome of interest is \code{peaceindex}, a measure of pro-peace
#' attitudes.
#'
#' Key  covariates include \code{herder_dar}
#' (whether they were a herder in Darfur), \code{farmer_dar} (whether they were a
#' farmer in Darfur), \code{age}, \code{female} (indicator for female), and
#' \code{past_voted} (whether they report having voted in an earlier election,
#' prior to the conflict).
#'
#' @format A data frame with 1276 rows and 14 columns.
#' \describe{
#'   \item{wouldvote}{If elections were held in Darfur in the future, would you vote? (0/1)}
#'   \item{peacefactor}{A measure of pro-peace attitudes, from a factor analysis of several questions. Rescaled such that 0 is minimally pro-peace and 1 is maximally pro-peace.}
#'   \item{peace_formerenemies}{Would you be willing to make peace with your former enemies? (0/1)}
#'   \item{peace_jjindiv}{Would you be willing to make peace with Janjweed individuals who carried out violence? (0/1)}
#'   \item{peace_jjtribes}{Would you be willing to make peace with the tribes that were part of the Janjaweed? (0/1)}
#'   \item{gos_soldier_execute}{Should Government of Sudan soldiers who perpetrated attacks on civilians be executed? (0/1)}
#'   \item{directlyharmed}{A binary variable indicating whether the respondent
#'   was personally physically injured during attacks on villages in Darfur
#'  largely between 2003-2004. 529 respondents report being personally injured,
#'  while 747 do not report being injured.}
#'   \item{age}{Age of respondent in whole integer years. Ages in the data
#'   range from 18 to 100.}
#'   \item{farmer_dar}{The respondent was a farmer in
#'  Darfur (0/1). 1,051 respondents were farmers, 225 were not.}
#'   \item{herder_dar}{The respondent was a herder in
#'   Darfur (0/1). 190 respondents were farmers, 1,086 were not.}
#'   \item{pastvoted}{The respondent reported having
#'   voted in a previous election before the conflict (0/1). 821 respondents reported
#'   having voted in a previous election, 455 reported not having voted in a
#'   previous election.}
#'   \item{hhsize_darfur}{Household size while in Darfur.}
#'   \item{village}{Factor variable indicating village of respondent. 486
#'   unique villages are accounted for in the data.}
#'   \item{female}{The respondent identifies as female (0/1). 582 respondents are female-identified, 694 are not.}
#' }
#' @references
#' Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018)
#'
#' Hazlett, C. (2018). Angry or weary? The effect of personal violence on attitudes towards peace in darfur. Working Paper.
"darfur"

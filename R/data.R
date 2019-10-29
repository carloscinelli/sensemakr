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




#' Data from Colombia
#'
#' @description
#' Data on Colombia...
#'
#' The main "treatment"
#' variables are \code{santos2014}, which indicates the share of ...
#' and \code{fat_2011to2015_gtd}, which indicates...
#'
#' The main outcome of interest is \code{yes_vote}, a measure of...
#'
#' Key  covariates include
#' \code{gdppc} (GDP per capita),
#' \code{elev} (elevation),
#'  \code{pop13} (population in ),
#'
#' @format A data frame with 1123 rows and 16 columns.
#' \describe{
#'   \item{dept_code}{}
#'   \item{department}{}
#'   \item{town_code}{}
#'   \item{town}{}
#'   \item{total_eligible}{}
#'   \item{yes_vote}{}
#'   \item{santos10}{}
#'   \item{santos14}{}
#'   \item{gdppc}{}
#'   \item{pop13}{}
#'   \item{elev}{}
#'   \item{fat_all}{}
#'   \item{fat_2001to2005_gtd}{}
#'   \item{fat_2006to2010_gtd}{}
#'   \item{fat_2011to2015_gtd}{}
#'   \item{fat_2010to2013}{}
#' }
#' @examples
#' # loads data
#' data(colombia)
#'
#' # Violence Models ---------------------------------------------------------
#'
#' ### Naive model --- Model 1
#' model1 <- lm(yes_vote ~ fat_2001to2005_gtd, data = colombia)
#'
#' ### Model 2
#' model2 <- lm(yes_vote ~ fat_2001to2005_gtd + fat_2006to2010_gtd +
#'                fat_2011to2015_gtd + total_eligible + santos10 + gdppc ,
#'              data = colombia)
#'
#' ### Sensitivity analysis - Model 2
#' sense.model2 <- sensemakr(model2,
#'                           treatment = "fat_2011to2015_gtd",
#'                           benchmark = "santos10",
#'                           kd = 1)
#'
#' ### contour plot point estimate
#' plot(sense.model2)
#'
#' ### contour plot t-value
#' plot(sense.model2, sensitivity.of = "t-value")
#'
#'
#' # Political Affiliation Model ---------------------------------------------
#'
#' ### Model 3
#' model3  <- lm(yes_vote ~ santos14 + fat_2010to2013 + elev + gdppc + pop13,
#'               data = colombia)
#'
#' ### Sensitivity analysis - Model 3
#' sense.model3 <- sensemakr(model3, treatment = "santos14",
#'                           benchmark = c("gdppc","elev"),
#'                           kd = 3)
#'
#' ### contour plot point estimate
#' plot(sense.model3, lim = .9)
#'
#' ### contour plot t-value
#' plot(sense.model3, sensitivity.of = "t-value", lim = 0.9)
#'
#' @references
#' Hazlett, Chad, and Francesca Parente. "Credible or Confounded? Applying sensitivity analyses to improve research and its evaluation under imperfect identification." (2018)
"colombia"

#' Data from survey of Darfurian refugees in eastern Chad.
#' @description
#' Data on attitudes of Darfurian refugees in eastern Chad. The main "treatment"
#' variable is \code{directlyharmed}, which indicates that the individual was physically
#' injured during attacks on villages in Darfur, largely between 2003 and 2004.
#' The main outcome of interest is \code{peacefactor}, a measure of pro-peace
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
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#'
#' Hazlett, Chad. (2019) "Angry or Weary? How Violence Impacts Attitudes toward Peace among Darfurian Refugees." Journal of Conflict Resolution: 0022002719879217.
#'
"darfur"




#' Data from the 2016 referendum for peace with the FARC in Colombia.
#'
#' @description
#' Data on support for the 2016 referendum for peace with the FARC in Colombia, as discussed in Hazlett and Parente (2020).
#' The main "treatment"
#' variables are \code{santos2014}, which indicates the share of town population voting in support of
#' Santos in the 2014 Presidential election, and \code{fat_2011to2015_gtd}, which indicates
#' the number of fatalities due to FARC violence between 2011 and 2015, again at the town level.
#' The main outcome of interest is \code{yes_vote}, the proportion (0-100) at the town-level
#' voting in support of the peace referendum. The question of interest in Hazlett and Parente (2020) is
#' what can be said about the causal effect of either violence (fatalities) or
#' of political affiliation with Santos, recognizing that analyses of either cannot
#' likely rule out all confounding.
#'
#' @format A data frame with 1123 rows and 16 columns.
#' \describe{
#'   \item{department}{Name for the provincial level unit, called departments or departamentos, of which there are 32 in the country.}
#'   \item{dept_code}{Short code for the department}
#'   \item{town}{Name for the town, which is the smallest electoral unit available and is the unit of analysis.}
#'   \item{town_code}{Code for the town.}
#'   \item{total_eligible}{Total eligible voters in the town}
#'   \item{yes_vote}{Proportion (out of 100) voting in favor of the peace deal.}
#'   \item{santos10}{Proportion (out of 100) voting for Santos in 2010 presidential election.}
#'   \item{santos14}{Proportion (out of 100) voting for Santos in the 2014 presidential election.}
#'   \item{gdppc}{The town-level GDP per capita.}
#'   \item{pop13}{Town-level population in 2013.}
#'   \item{elev}{Town's mean elevation.}
#'   \item{fat_all}{Sum of all known fatalities due to FARC violence in the town (from Global Terrorism Database, GTD).}
#'   \item{fat_2001to2005_gtd}{Sum of fatalities due to FARC in the town in 2001 to 2005 (from GTD).}
#'   \item{fat_2006to2010_gtd}{Sum of fatalities due to FARC in the town in 2006 to 2010 (from GTD).}
#'   \item{fat_2011to2015_gtd}{Sum of fatalities due to FARC in the town in 2011 to 2015 (from GTD).}
#'   \item{fat_2010to2013}{Sum of fatalities due to FARC in the town in 2010 to 2013 (from GTD).}
#' }
#' @examples
#' # loads data
#' data(colombia)
#'
#' #-----------------------------------------------------
#' # Violence Models
#' #-----------------------------------------------------
#'
#' ### Model 1 (bivariate)
#' model1 <- lm(yes_vote ~ fat_2001to2005_gtd, data = colombia)
#'
#' ### Model 2 (more controls, and lagged violence.)
#' model2 <- lm(yes_vote ~ fat_2001to2005_gtd + fat_2006to2010_gtd +
#'                fat_2011to2015_gtd + total_eligible + santos10 + gdppc ,
#'              data = colombia)
#'
#' ### Sensitivity analysis - Model 2, for effect of most recent violence.
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
#' #---------------------------------------------
#' # Political Affiliation Model
#' #---------------------------------------------
#'
#' ### Model 3: santos2014 as measure of political support for Santos, with control variables.
#' model3  <- lm(yes_vote ~ santos14 + fat_2010to2013 + elev + gdppc + pop13,
#'               data = colombia)
#'
#' ### Sensitivity analysis - Model 3
#' sense.model3 <- sensemakr(model3, treatment = "santos14",
#'                           benchmark = c("gdppc","elev"),
#'                           kd = 3)
#' summary(sense.model3)
#'
#' ### contour plot point estimate
#' plot(sense.model3, lim = .9)
#'
#' ### contour plot t-value
#' plot(sense.model3, sensitivity.of = "t-value", lim = 0.9)
#'
#' @references
#' Hazlett, C., and Parente, F. (2020). "Who supports peace with the FARC? A sensitivity-based approach under imperfect identification"
"colombia"

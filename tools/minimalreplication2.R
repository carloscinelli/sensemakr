library(sensemakr)

colombia = readRDS("data/ColombiaReplicationData.rds")
names(colombia[9:12])
colombia[9:12] <- NULL
names(colombia)

save(colombia, file = "data/colombia.rda")
str(dat)
colombia <- colombia[,c("dept_code", "department", "town_code", "town",
                       "total_eligible", "yes_vote", "santos10", "santos14",
                       "gdppc", "pop13", "elev", "fat_all", "fat_2001to2005_gtd",
                       "fat_2006to2010_gtd", "fat_2011to2015_gtd", "fat_2010to2013" )]
rm(list =ls())

load("data/colombia.rda")

# Violence Models ---------------------------------------------------------

### Naive model --- Model 1
model1 <- lm(yes_vote ~ fat_2001to2005_gtd, data = colombia)

### Model 2
model2 <- lm(yes_vote ~ fat_2001to2005_gtd + fat_2006to2010_gtd +
               fat_2011to2015_gtd + total_eligible + santos10 + gdppc ,
             data = colombia)

### Sensitivity analysis (model 2)
sense.model2 <- sensemakr(model2, treatment = "fat_2011to2015_gtd", benchmark="santos10", kd = 1)

### contour plot point estimate
plot(sense.model2)

### contour plot t-value
plot(sense.model2, sensitivity.of = "t-value")



# Political Affiliation Models --------------------------------------------

### Model 3
model3  <- lm(yes_vote ~ santos14 + fat_2010to2013 + elev + gdppc + pop13, data = colombia)


q <- 0.1
sense.model3 = sensemakr(model3, treatment = "santos14",
                         benchmark=c("gdppc","elev"), kd = 3, q = q)

rv <- sense.model3$sensitivity_stats$rv_qa
rv
r2yd <- sense.model3$sensitivity_stats$r2yd.x

rv > 1 - (1/q^2)*(1/r2yd - 1)
# rv > 2 - 1/r2yd

f2 <- sense.model3$sensitivity_stats$f2yd.x
f2

rv < (rv/(1-rv))/(f2*q)

### contour plot point estimate
plot(sense.model3, lim = .9)

### contour plot t-value
plot(sense.model3, sensitivity.of = "t-value", lim=0.9, nlevels =50)
abline(a = 0,  b = 1)


pt(20, model3$df.residual, log.p = T)

robustness_value(model3, covariates = "santos14", alpha = (1 - pt(5, model3$df.residual))*2)
adjusted_t(model3, treatment = "santos14", r2dz.x = 0.6297902 , r2yz.dx = 0.6297902)
adjusted_t(model3, treatment = "santos14", r2dz.x = 0.4 , r2yz.dx = 0.24)


adjusted_t(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6)
adjusted_estimate(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6) +
  1.96*adjusted_se(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6)

adjusted_estimate(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6) -
  1.96*adjusted_se(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6)

bias(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6)
adjusted_se(model3, treatment = "santos14", r2dz.x = 0.13, r2yz.dx = 0.6)

adjusted_t(model3, treatment = "santos14",  r2dz.x = 0.01, r2yz.dx = 0.2)

adjusted_estimate(model3, treatment = "santos14",  r2dz.x = 0.01, r2yz.dx = 0.2) +
  1.96*adjusted_se(model3, treatment = "santos14", r2dz.x = 0.01, r2yz.dx = 0.2)

adjusted_estimate(model3, treatment = "santos14",  r2dz.x = 0.01, r2yz.dx = 0.2) -
  1.96*adjusted_se(model3, treatment = "santos14", r2dz.x = 0.01, r2yz.dx = 0.2)

bias(model3, treatment = "santos14",        r2dz.x = 0.01, r2yz.dx = 0.2)



adjusted_t(model3, treatment = "santos14", r2dz.x = 0.006, r2yz.dx = 0.005)

adjusted_t(model3, treatment = "santos14", r2dz.x = 0.008, r2yz.dx = 0.008)

sense.santos.out.nobm = sensemakr(model3, treatment = "santos14")

plot(sense.santos.out.nobm, type="extreme", lim=.9, r2yz.dx=c(1,.5,.3))

## ------------------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE,
                      fig.width=6,
                      fig.height=6)

## ------------------------------------------------------------------------

# devtools::install_github("chadhazlett/sensemakr")

# setwd("~/projects/sensemakr_fin/sensemakr/")

# devtools::build_vignettes() 


## ------------------------------------------------------------------------
library(sensemakr)
data("darfur")

## ------------------------------------------------------------------------
model_cntns = lm(data = darfur,
                 peacefactor ~ directlyharmed +
                   age + hhsize_darfur)


## ------------------------------------------------------------------------
sense_cntns = sensemakr(model=model_cntns,
                        treatment="directlyharmed")

str(sense_cntns$benchmarks,max.level = 1)

class(sense_cntns)


## ------------------------------------------------------------------------
plot(sense_cntns,lim=0.02)

## ------------------------------------------------------------------------
plot(sense_cntns,showvars=list('age','hhsize_darfur'),lim=0.02)
plot(sense_cntns,showvars=list('age'),lim=0.02)

## ------------------------------------------------------------------------

model_fctr  = lm(data = darfur,
                 peacefactor ~ directlyharmed + 
                   age + hhsize_darfur +
                   female + village)


## ------------------------------------------------------------------------


sense_fctr = sensemakr(model=model_fctr,
                       treatment="directlyharmed")



## ------------------------------------------------------------------------
str(sense_fctr$benchmarks,max.level = 1)

sense_fctr$benchmarks$benchmark_group

row.names(sense_fctr$benchmarks$benchmark_group)


## ------------------------------------------------------------------------

str(sense_fctr$benchmarks$benchmark_eachvar)



## ------------------------------------------------------------------------

str(sense_fctr$benchmarks$benchmark_group)


## ------------------------------------------------------------------------
str(sense_fctr$benchmarks$benchmark_masked)


## ------------------------------------------------------------------------

plot(sense_fctr,showvars='all',lim=0.02)



## ------------------------------------------------------------------------

# plot(sense_fctr,showvars='masked',lim=0.02)
# same as

plot(sense_fctr,lim=0.5)


## ------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(sense_fctr,showvars=list('village','villageMngao','age'),lim=0.5)


plot(sense_fctr,showvars=list('village','villageMngao','age'),lim=0.02)



## ------------------------------------------------------------------------
model_cust_grp  = lm(data = darfur,
                     peacefactor ~ directlyharmed + 
                       age + hhsize_darfur +
                       female + village
                     )


## ------------------------------------------------------------------------
identical(model_cust_grp,model_fctr)


## ------------------------------------------------------------------------

sense_cust_grp = sensemakr(model=model_cust_grp,
                           treatment="directlyharmed",
                           group_list = list(c('village','female'),'age')
                           )

str(sense_cust_grp$benchmarks,max.level = 1)

## ------------------------------------------------------------------------
names(sense_cust_grp$benchmarks)

head(sense_cust_grp$benchmarks$benchmark_masked)
head(sense_cust_grp$benchmarks$benchmark_group)

## ------------------------------------------------------------------------
class(model_cust_grp)

colnames(attr(terms(formula(model_cust_grp)),'factor'))


## ------------------------------------------------------------------------
plot(sense_cust_grp,showvars='all',lim=0.5)

## ------------------------------------------------------------------------
plot(sense_cust_grp)

## ------------------------------------------------------------------------

plot(sense_cust_grp,showvars=list('village,female'),lim=0.5)


## ------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(sense_cust_grp, lim=.2)
plot(sense_cust_grp, contour = "t-value")
plot(sense_cust_grp, contour = "lower bound")
plot(sense_cust_grp, contour = "upper bound")


## ------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(sense_cust_grp, contour = "t-value",showvars='all')
plot(sense_cust_grp, contour = "t-value",showvars='all',lim=0.02)
plot(sense_cust_grp, contour = "lower bound",showvars='all',lim=0.02)
plot(sense_cust_grp, contour = "upper bound",showvars='all',lim=0.02)


## ------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(sense_cust_grp, contour = "t-value",
     showvars=list('village','villageMngao','age'))

plot(sense_cust_grp, contour = "lower bound",
     showvars=list('village','villageMngao','age'))

plot(sense_cust_grp, contour = "upper bound",
     showvars=list('village','villageMngao','age'))

## ------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(sense_cust_grp, type = "worst-case")

plot(sense_cust_grp, type = "worst-case",showvars='masked',lim=0.5)

plot(sense_cust_grp, type = "worst-case",showvars='all',lim=0.5)

plot(sense_cust_grp, type = "worst-case",lim=0.5,
     showvars=list('village','villageMngao','age'))



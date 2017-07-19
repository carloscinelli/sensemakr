data("darfur",package='sensemakr')

# model the user wants to run sensitivity analysis
mod_test  <- lm(peacefactor ~ directlyharmed + female + village, data = darfur)


test = sensemakr.lm(model=mod_test,D='directlyharmed')
str(test,max.level=1)
str(test$benchmarks,max.level=1)

# group results of 'village'
test$benchmarks$benchmark_R2_group

# low level coef of village 'levels'
head(test$benchmarks$benchmark_R2)

# user supplied group_list for arbitrary grouping of terms
# each list entry is a character vector.
# elements of a character vector are simultaneously with-held
test2 = sensemakr.lm(model=mod_test,D='directlyharmed',
                     group_list = list(c('village','female')))

str(test2$benchmarks,max.level=1)
# village AND female simultaneously with-held
test2$benchmarks$benchmark_R2_group

# low level coef of village 'levels' still there
head(test2$benchmarks$benchmark_R2)

# so, can 'mask' later in print/summary/plot methods

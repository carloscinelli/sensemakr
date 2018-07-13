# Reusable test object for successful tests.
test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                       farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                       female + village,
                     treatment = "directlyharmed",
                     data = darfur,
                     benchmark = "female")

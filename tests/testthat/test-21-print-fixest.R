context("Print")

test_that("HTML tables", {
  data("darfur")
  # runs regression model
  model <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)

  expect_error(ovb_minimal_reporting(model))
  darfur_out <- sensemakr(model, treatment = "directlyharmed",
                          benchmark_covariates = "female", kd = 1:3)
  out <- ovb_minimal_reporting(darfur_out, format = "html", verbose = F)
  check <- "<table>\n<thead>\n<tr>\n\t<th style=\"text-align:left;border-bottom: 1px solid transparent;border-top: 1px solid black\"> </th>\n\t<th colspan = 6 style=\"text-align:center;border-bottom: 1px solid black;border-top: 1px solid black\"> Outcome: peacefactor</th>\n</tr>\n<tr>\n\t<th style=\"text-align:left;border-top: 1px solid black\"> Treatment </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> Est. </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> S.E. </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> t-value </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> $R^2_{Y \\sim D |{\\bf X}}$ </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\">  $RV_{q = 1}$ </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> $RV_{q = 1, \\alpha = 0.05}$ </th>\n</tr>\n</thead>\n<tbody>\n <tr>\n\t<td style=\"text-align:left; border-bottom: 1px solid black\"><i>directlyharmed</i></td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">0.097 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">0.023 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">4.184 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">2.2\\% </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">13.9\\% </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">7.6\\% </td>\n</tr>\n</tbody>\n<tr>\n<td colspan = 7 style='text-align:right;border-top: 1px solid black;border-bottom: 1px solid transparent;font-size:11px'>Note: df = 783; Bound ( 1x female ):  $R^2_{Y\\sim Z| {\\bf X}, D}$ = 12.5\\%, $R^2_{D\\sim Z| {\\bf X} }$ = 0.9\\%</td>\n</tr>\n</table>"
  expect_equal(out, check)

  out <- ovb_minimal_reporting(darfur_out, format = "pure_html", verbose = F)
  check <- "<table style='align:center'>\n<thead>\n<tr>\n\t<th style=\"text-align:left;border-bottom: 1px solid transparent;border-top: 1px solid black\"> </th>\n\t<th colspan = 6 style=\"text-align:center;border-bottom: 1px solid black;border-top: 1px solid black\"> Outcome: peacefactor</th>\n</tr>\n<tr>\n\t<th style=\"text-align:left;border-top: 1px solid black\"> Treatment </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> Est. </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> S.E. </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> t-value </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> R<sup>2</sup><sub>Y~D|X</sub> </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\">  RV<sub>q = 1</sub> </th>\n\t<th style=\"text-align:right;border-top: 1px solid black\"> RV<sub>q = 1, &alpha; = 0.05</sub> </th>\n</tr>\n</thead>\n<tbody>\n <tr>\n\t<td style=\"text-align:left; border-bottom: 1px solid black\"><i>directlyharmed</i></td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">0.097 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">0.023 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">4.184 </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">2.2\\% </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">13.9\\% </td>\n\t<td style=\"text-align:right;border-bottom: 1px solid black\">7.6\\% </td>\n</tr>\n</tbody>\n<tr>\n<td colspan = 7 style='text-align:right;border-bottom: 1px solid transparent;font-size:11px'>Note: df = 783; Bound ( 1x female ):  R<sup>2</sup><sub>Y~Z|X,D</sub> = 12.5\\%, R<sup>2</sup><sub>D~Z|X</sub> = 0.9\\%</td>\n</tr>\n</table>"
  expect_equal(out, check)

}
)

context("Cumulative effects (of time-dependent covariates)")


test_that("Lag-lead is calculated correctly", {
  LL <- get_laglead(0:2, c(-2, -0.5, 0, 0.5, 2),
    ll_fun = function(t, tz) t >= tz)
  expect_data_frame(LL, nrows = 15L, ncols = 3L)
  expect_class(LL, "LL_df")
  expect_identical(LL$t, rep(0:2, each = 5))
  expect_identical(LL$tz, rep(c(-2, -0.5, 0, 0.5, 2), times = 3))
  expect_equal(LL$LL, c(rep(0, 5), rep(1, 3), rep(0, 2), rep(1, 4), 0))
})

test_that("LL helpers and as_ped produce equivalent LL windows", {

  # data("sim_df1")
  # lag-lead window functions used in simulation
  ll_fun <- function(t, tz) t >= tz
  ll_fun2 <- function(t, tz) t >= tz + 2 & t <= tz + 2 + 5
  ped <- sim_df1 %>% as_ped(
    Surv(time, status) ~ .  + cumulative(time, z.tz1, tz_var = "tz1") +
      cumulative(time, z.tz2, tz_var = "tz2",
        ll_fun = function(t, tz) (t >= tz + 2) & (t <= tz + 2 + 5)),
    id = "id")
  LL1 <- ped$LL_tz1[1:10, ]
  LL1.1 <- get_laglead(0:10, 1:10, ll_fun) %>%
    filter(t != 0) %>% tidyr::spread(tz, LL)
  expect_equal(as.matrix(LL1.1[, -1]), LL1, check.attributes = FALSE)
  LL2 <- ped$LL_tz2[1:10, ]
  LL2.2 <- get_laglead(0:10, -5:5, ll_fun2) %>%
    filter(t != 0 ) %>%
    tidyr::spread(tz, LL)
  expect_equal(as.matrix(LL2.2[, -1]), LL2, check.attributes = FALSE)
  LL1.2 <- get_laglead(ped) %>% filter(tz_var == "tz1") %>% filter(t != 0) %>%
    tidyr::spread(tz, LL) %>% select(-1:-2) %>% as.matrix()
  LL2.2 <- get_laglead(ped) %>% filter(tz_var == "tz2") %>% filter(t != 0) %>%
    tidyr::spread(tz, LL) %>% select(-1:-2) %>% as.matrix()
  expect_equal(LL1, LL1.2, check.attributes = FALSE)
  expect_equal(LL2, LL2.2, check.attributes = FALSE)

})

test_that("Cumulative effects are calculated correctly", {

  # data("sim_df2")

  ped <- as_ped(sim_df2, Surv(time, status)~ x1 + x2 +
      cumulative(latency(tz3), z.tz3, tz_var = "tz3"),
    cut = 0:10)
  ped5 <- subset(ped, id == 5)
  expect_identical(ped5$LL[1, ], c(2.5, 2, 3, rep(0, 2)))
  expect_identical(ped5$LL[9, ], c(2.5, 2, 3, 3, 2))
  expect_identical(ped5$LL[10, ], c(2.5, 2, 3, 3, 2))
  # pam <- mgcv::gam(ped_status ~ s(tend) + x1 + s(x2) +
  #     s(tz3_latency, by = z.tz3),
  #   data = ped, family = poisson(), offset = offset)
  # ndf <- make_newdata(ped, tz3_latency = unique(tz3_latency), z.tz3 = c(1))
  # ndf <- ndf %>% add_term(pam, term = "z.tz3") %>% slice(1:7)
  # expect_equal(ndf$fit, c(.72, .88, 0.73, 0.46, 0.38, 0.26, 0.14),
  #   tolerance = 10e-3)

  # ## partial effects
  # partial <- gg_partial(ped, pam, "z.tz3", tend = seq(0, 10, by = 1),
  #   tz3_latency = 0:12, z.tz3 = c(1), reference = list(z.tz3 = 1))
  # expect_is(partial, c("gg", "ggplot"))
  # expect_data_frame(partial$data, nrows = 143L, ncols = 15L)
  # partial_ll <- gg_partial_ll(ped, pam, "z.tz3", tend = seq(0, 10, by = 1),
  #   tz3_latency = 0:12, z.tz3 = c(1), reference = list(z.tz3 = 1))
  # expect_is(partial_ll, c("gg", "ggplot"))
  # expect_data_frame(partial_ll$data, nrows = 53L, ncols = 8L)

  ## cumulative effect visualization helpers:
  # cumu_eff <- get_cumu_eff(ped, pam, term = "z.tz3",
  #   z1 = seq(-1, 1, length.out = 5), z2 = 0)
  # expect_identical(unique(ped$interval), unique(cumu_eff$interval))
  # expect_matrix(cumu_eff$z.tz3, nrows = 10L, ncols = 5L, any.missing = FALSE)
  # expect_identical(cumu_eff$z.tz3[1, ], cumu_eff$z.tz3[2, ])
  # expect_subset(
  #   x = c("cumu_eff", "se_cumu_eff", "cumu_eff_lower", "cumu_eff_upper"),
  #   choices = colnames(cumu_eff))
  # expect_identical(all(cumu_eff$cumu_eff >= cumu_eff$cumu_eff_lower), TRUE)
  # expect_identical(all(cumu_eff$cumu_eff <= cumu_eff$cumu_eff_upper), TRUE)
  # expect_numeric(cumu_eff$se_cumu_eff, lower = 0, finite = TRUE,
  #   any.missing = FALSE)

})

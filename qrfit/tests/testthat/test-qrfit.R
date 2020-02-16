# Contents of file `test-qrfit.R`

context("qrfit")

test_that("`qrfit()` gives same result as `optimCheck::optim_proj()`", {
  # for each test check that `min(abs_err, rel_err) < tol`
  # for each element of the solution beta_hat.
  ntest <- 10 # number of tests
  tol <- .01 # tolerance level
  
  max_err <- rep(NA, ntest)
  for(ii in 1:ntest) {
    # simulate random data
    n <- sample(100:1000, 1)
    p <- sample(1:20, 1)
    X <- matrix(rnorm(n*p), n, p)
    beta <- rnorm(p)
    y <- c(X %*% beta) + rnorm(n)
    tau <- runif(1)
    # fit the quantile regression
    beta_hat <- qr_fit(y, X, tau)
    # projection plots, except we don't plot, instead saving the output
    # of each plot to an S3 object of type `optproj`.
    oproj <- optimCheck::optim_proj(xsol = beta_hat,
                                    fun = function(beta)
                                      qr_obj(y = y, X = X, beta = beta, tau = tau),
                                    maximize = FALSE, plot = FALSE)
    # `diff` calculates the abs and rel error between the
    # candidate solution `xsol` and the minimum in each projection plot.
    # see ?diff.optcheck for details.
    err <- abs(diff(oproj)) # abs and rel error
    max_err[ii] <- max(pmin(err[,"abs"], err[,"rel"]))
    expect_lt(max_err[ii], tol)
  }
})
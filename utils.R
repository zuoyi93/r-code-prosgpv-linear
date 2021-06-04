# utility functions

# --------------------------
# function to make Figure 1
# --------------------------

get.plot.fig.1 <- function(lambda = 2, range = 10) {

  # preparation: subplot 1
  x <- seq(-range, range, 0.01)
  y <- x
  y[y >= -lambda & y <= lambda] <- 0

  x1 <- x[x < -lambda]
  y1 <- y[which(x < -lambda)]
  xlambda <- x[x >= -lambda & x <= lambda]
  ylambda <- y[which(x >= -lambda & x <= lambda)]
  x3 <- x[x > lambda]
  y3 <- y[which(x > lambda)]

  # preparation: subplot 2

  m <- seq(-range, range, 0.01)
  n <- rep(0, length(m))
  n[m > lambda] <- m[m > lambda] - lambda
  n[m < -lambda] <- m[m < -lambda] + lambda

  # preparation:subplot 3

  a <- 3.7 * lambda

  diag.seq.solid <- seq(-range, -a, 0.01)
  diag.seq.dashed <- seq(-a, a, 0.01)
  mcp.seq <- seq(lambda, a, 0.01)
  mcp.seq.2 <- seq(-a, -lambda, 0.01)
  scad.seq <- seq(lambda, 2 * lambda, 0.01)
  scad.seq.2 <- seq(-2 * lambda, -lambda, 0.01)
  scad.seq.3 <- seq(2 * lambda, a, 0.01)
  scad.seq.4 <- seq(-a, -2 * lambda, 0.01)

  # preparation: subplot 4

  lambda.2 <- 4

  # output the figure
  par(mfrow = c(2, 2))

  # subplot 1: l0 penalty
  plot(x1, y1,
    type = "l", xlim = c(-range, range), ylim = c(-range, range),
    ylab = "Thresholding function", xlab = expression(hat(theta)), main = "(1) Hard thresholding",
    axes = F, frame.plot = T
  )
  lines(xlambda, ylambda)
  lines(x3, y3)
  lines(c(-lambda, -lambda), c(-lambda, 0), lty = "dashed")
  lines(c(lambda, lambda), c(0, lambda), lty = "dashed")
  lines(c(-lambda, lambda), c(-lambda, lambda), lty = "dashed")
  segments(-lambda, -lambda, -lambda, -range, lty = "dashed", col = "blue")
  segments(lambda, 0, lambda, -range, lty = "dashed", col = "blue")
  axis(1, at = seq(-range, range, lambda))
  axis(2, at = seq(-range, range, lambda))
  mtext(c(bquote("-" ~ lambda), expression(lambda)),
    line = 3,
    side = 1, at = c(-lambda, lambda), col = "blue"
  )


  # subplot 2: lasso penalty

  plot(m, n,
    type = "l", xlim = c(-range, range), ylim = c(-range, range),
    ylab = "Thresholding function", xlab = expression(hat(theta)), main = "(2) Lasso",
    axes = F, frame.plot = T
  )
  lines(c(-range, range), c(-range, range), lty = "dashed")
  axis(1, at = seq(-range, range, lambda))
  axis(2, at = seq(-range, range, lambda))
  segments(-lambda, 0, -lambda, -range, lty = "dashed", col = "blue")
  segments(lambda, 0, lambda, -range, lty = "dashed", col = "blue")
  mtext(c(expression("-" ~ lambda), expression(lambda)),
    line = 3,
    side = 1, at = c(-lambda, lambda), col = "blue"
  )

  # subplot 3: scad and mcp

  plot(0,
    type = "n", xlim = c(-range, range), ylim = c(-range, range),
    ylab = "Thresholding function", xlab = expression(hat(theta)), main = "(3) SCAD/MCP",
    axes = F, frame.plot = T
  )
  axis(1, at = seq(-range, range, lambda))
  axis(2, at = seq(-range, range, lambda))
  lines(diag.seq.solid, diag.seq.solid)
  lines(-diag.seq.solid, -diag.seq.solid)
  lines(diag.seq.dashed, diag.seq.dashed, lty = "dashed")
  lines(xlambda, ylambda)
  lines(mcp.seq, seq(0, 7.4, length.out = length(mcp.seq)), col = "limegreen")
  lines(mcp.seq.2, seq(-7.4, 0, length.out = length(mcp.seq.2)), col = "limegreen")
  lines(scad.seq, seq(0, 2, length.out = length(scad.seq)), col = "red")
  lines(scad.seq.2, seq(-2, 0, length.out = length(scad.seq.2)), col = "red")
  lines(scad.seq.3, seq(2, a, length.out = length(scad.seq.3)), col = "red")
  lines(scad.seq.4, seq(-a, -2, length.out = length(scad.seq.4)), col = "red")
  segments(-lambda, 0, -lambda, -range, lty = "dashed", col = "blue")
  segments(lambda, 0, lambda, -range, lty = "dashed", col = "blue")
  segments(2 * lambda, lambda, 2 * lambda, -range, lty = "dashed", col = "blue")
  segments(-2 * lambda, -lambda, -2 * lambda, -range, lty = "dashed", col = "blue")
  segments(a, a, a, -range, lty = "dashed", col = "blue")
  segments(-a, -a, -a, -range, lty = "dashed", col = "blue")
  mtext(c(
    expression("-" ~ lambda), expression(lambda),
    expression("2" ~ lambda), bquote("-2" ~ lambda),
    bquote(gamma ~ lambda), bquote("-" ~ gamma ~ lambda)
  ), line = 3, side = 1, at = c(
    -lambda, lambda, 2 * lambda, -2 * lambda,
    a, -a
  ), col = "blue")
  legend(-10, 9,
    legend = c("SCAD", "MCP"),
    col = c("red", "limegreen"), lty = c(1, 1), box.lty = 0
  )

  # sub plot 4: sgpv thresholding

  plot(0,
    type = "n", xlim = c(-range, range), ylim = c(-range, range),
    ylab = "Thresholding function", xlab = expression(hat(theta)), main = "(4) ProSGPV",
    axes = F, frame.plot = T
  )
  lines(c(-lambda.2, lambda.2), c(-lambda.2, lambda.2), lty = "dashed")
  axis(1, at = seq(-range, range, lambda))
  axis(2, at = seq(-range, range, lambda))
  segments(-range, -range, -lambda.2, -lambda.2)
  segments(-lambda.2, 0, lambda.2, 0)
  segments(lambda.2, lambda.2, range, range)
  segments(lambda.2, 0, lambda.2, lambda.2, lty = "dashed")
  segments(-lambda.2, -lambda.2, -lambda.2, 0, lty = "dashed")
  segments(-lambda, 0, -lambda, -range, lty = "dashed", col = "blue")
  segments(lambda, 0, lambda, -range, lty = "dashed", col = "blue")
  segments(-lambda.2, -lambda.2, -lambda.2, -range, lty = "dashed", col = "blue")
  segments(lambda.2, 0, lambda.2, -range, lty = "dashed", col = "blue")
  mtext(c(bquote("-" ~ lambda), expression(lambda)),
    line = 3,
    side = 1, at = c(-lambda, lambda), col = "blue"
  )
  mtext(c(bquote("-" ~ lambda["new"]), expression(lambda["new"])),
    line = 3,
    side = 1, at = c(-lambda.2, lambda.2), col = "blue"
  )
}

# -------------------------
# generate data for fig.2
# -------------------------

gen.data.fig.2 <- function() {
  beta <- c(0, 0, 0.28, 0, 0)

  cov.structure <- matrix(0, 5, 5)
  for (i in 1:5) {
    for (j in 1:5) {
      cov.structure[i, j] <- 0.5^(abs(i - j))
    }
  }

  # generate X
  X <- mvrnorm(n = 400, rep(0, 5), Sigma = cov.structure)

  # generate Y
  Y <- rnorm(X %*% beta, X %*% beta, sd = 1)

  return(list(X, Y))
}

# ---------------------------
# functions to produce fig.2
# ---------------------------

get.coef.fig.2 <- function(xs, ys, lambda, lasso) {
  p <- ncol(xs)

  # evaluate lasso at lambda
  pe.lasso <- coef(lasso, s = lambda)[-1]
  index <- which(pe.lasso != 0)

  # define the output
  out.coef <- numeric(p)
  out.lb <- numeric(p)
  out.ub <- numeric(p)

  if (lambda == 0) {

    # full ols model
    full.ols <- lm(ys ~ xs)

    pe <- summary(full.ols)$coef[-1, 1]
    se <- summary(full.ols)$coef[-1, 2]
    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(summary(full.ols)$coef[-1, 2])

    out.coef <- as.numeric(pe)
    out.lb <- as.numeric(lb)
    out.ub <- as.numeric(ub)
  } else if (length(index) != 0) {

    # run fully relaxed LASSO
    f.l <- lm(ys ~ xs[, index])

    # get confidence bands
    pe <- summary(f.l)$coef[-1, 1]
    se <- summary(f.l)$coef[-1, 2]
    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(summary(f.l)$coef[-1, 2])

    out.coef[index] <- as.numeric(pe)
    out.lb[index] <- as.numeric(lb)
    out.ub[index] <- as.numeric(ub)
  } else if (length(index) == 0) {

    # intercept only model
    null.bound.lasso <- 0
  }

  return(c(pe.lasso, out.coef, out.lb, out.ub, null.bound.lasso))
}


# output figure
get.plot.fig.2 <- function(data, p = 5) {

  # load data
  X <- data[[1]]
  Y <- data[[2]]

  xs <- scale(X)
  ys <- scale(Y)

  # find the lambda cutoffs

  # lasso
  lasso <- glmnet(xs, ys)
  yhat.m <- predict(lasso, newx = xs)
  p.k <- apply(coef(lasso), 2, function(z) sum(z != 0))

  # GIC
  lasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((ys - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  lambda.gic <- lasso$lambda[lasso.lambda.index]
  q.index <- which(coef(lasso, s = lambda.gic)[-1] != 0)

  # get a range of lambda
  lambda.grid <- seq(0, 0.2, length.out = 200)

  # input lambda and output coefficients
  results <- sapply(
    lambda.grid,
    function(x) get.coef.fig.2(xs, ys, lambda = x, lasso)
  )

  # create a data set for plot
  to.plot <- data.frame(
    lambda = rep(lambda.grid, each = p),
    v = rep(paste("V", 1:p, sep = ""), length(lambda.grid)),
    pe.lasso = c(results[1:p, ]),
    pe.ols = c(results[(p + 1):(2 * p), ]),
    lb = c(results[(2 * p + 1):(3 * p), ]),
    ub = c(results[(3 * p + 1):(4 * p), ])
  )

  to.plot$closer <- sapply(
    1:nrow(to.plot),
    function(x) {
      ifelse(abs(to.plot$lb[x]) <
        abs(to.plot$ub[x]), "lb", "ub")
    }
  )

  to.plot$curve <- sapply(
    1:nrow(to.plot),
    function(x) to.plot[x, to.plot$closer[x]]
  )

  # find the limit of the canvas
  ylim <- c(
    min(c(to.plot$pe.ols, to.plot$lb, to.plot$ub)) * 1.1,
    max(c(to.plot$pe.ols, to.plot$lb, to.plot$ub)) * 1.1
  )

  # panel 1: lasso

  # change color
  color.lines <- c("#DD8D29", "#B40F20", "#008b45", "#E2D200", "#46ACC8")
  color.use <- rep("black", 5)
  color.use[3] <- color.lines[3]

  # find the coefficient in full OLS
  location.beta <- to.plot$pe.lasso[to.plot$lambda == 0]

  p1 <- ggplot(data = to.plot) +
    geom_line(aes(x = lambda, y = pe.lasso, col = v)) +
    guides(col = FALSE) +
    annotate("text", x = lambda.gic, y = -0.28, label = bquote(lambda["gic"])) +
    scale_color_manual(values = color.lines) +
    scale_x_continuous(
      breaks = seq(0, 0.2, 0.05),
      limits = c(-0.01, 0.2)
    ) +
    labs(x = expression(lambda), y = "Coefficients", title = "(1) Lasso") +
    scale_y_continuous(
      breaks = seq(-0.2, 0.5, 0.1),
      limits = c(-0.3, 0.5)
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 0))
    ) +
    annotate("text",
      x = -0.01, y = location.beta, label = paste("V", 1:p, sep = ""),
      col = color.use
    ) +
    coord_cartesian(ylim = ylim, clip = "off") +
    geom_vline(xintercept = lambda.gic, linetype = "dashed", col = "black")

  # panel 2: fully relaxed lasso
  location.beta <- to.plot$pe.ols[to.plot$lambda == 0]

  p2 <- ggplot(data = to.plot, aes(x = lambda, y = pe.ols, col = v)) +
    geom_line() +
    annotate("text", x = lambda.gic, y = -0.28, label = bquote(lambda["gic"])) +
    labs(
      x = expression(lambda), y = "Coefficients",
      title = "(2) Fully-relaxed lasso"
    ) +
    guides(col = FALSE) +
    scale_color_manual(values = color.lines) +
    scale_x_continuous(
      breaks = seq(0, 0.2, 0.05),
      limits = c(-0.01, 0.2)
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 0))
    ) +
    scale_y_continuous(
      breaks = seq(-0.2, 0.5, 0.1),
      limits = c(-0.3, 0.5)
    ) +
    annotate("text",
      x = -0.01, y = location.beta, label = paste("V", 1:p, sep = ""),
      col = color.use
    ) +
    coord_cartesian(ylim = ylim, clip = "off") +
    geom_vline(xintercept = lambda.gic, linetype = "dashed", col = "black")

  # panel 3: fully relaxed lasso with confidence interval

  p3 <- ggplot(data = to.plot) +
    geom_line(aes(x = lambda, y = pe.ols, col = v)) +
    guides(col = FALSE) +
    annotate("text", x = lambda.gic, y = -0.28, label = bquote(lambda["gic"])) +
    scale_color_manual(values = color.lines) +
    geom_line(aes(x = lambda, y = lb, col = v), alpha = 0.5) +
    geom_line(aes(x = lambda, y = ub, col = v), alpha = 0.5) +
    scale_x_continuous(
      breaks = seq(0, 0.2, 0.05),
      limits = c(-0.01, 0.2)
    ) +
    labs(
      x = expression(lambda), y = "Coefficients",
      title = "(3) Fully-relaxed lasso w/ confidence interval"
    ) +
    scale_y_continuous(
      breaks = seq(-0.2, 0.5, 0.1),
      limits = c(-0.3, 0.5)
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 0))
    ) +
    annotate("text",
      x = -0.01, y = location.beta, label = paste("V", 1:p, sep = ""),
      col = color.use
    ) +
    coord_cartesian(ylim = ylim, clip = "off") +
    geom_vline(xintercept = lambda.gic, linetype = "dashed", col = "black")

  # panel 4: one bound from fully relaxed lasso + null region
  location.beta <- to.plot$curve[to.plot$lambda == 0]

  # create a data set for null bound
  n.bound <- results[(4 * p + 1), ]
  bound.d <- data.frame(lambda = lambda.grid, lb = -n.bound, ub = n.bound)

  to.plot$v <- factor(to.plot$v, levels = paste("V", 1:p, sep = ""))

  p4 <- ggplot(data = to.plot) +
    geom_line(aes(x = lambda, y = curve, col = v), alpha = 0.5) +
    guides(col = FALSE) +
    annotate("text", x = lambda.gic, y = -0.28, label = bquote(lambda["gic"])) +
    scale_color_manual(values = color.lines) +
    geom_ribbon(
      data = bound.d, aes(x = lambda, ymin = lb, ymax = ub),
      alpha = .2
    ) +
    scale_x_continuous(
      breaks = seq(0, 0.2, 0.05),
      limits = c(-0.01, 0.2)
    ) +
    labs(
      x = expression(lambda), y = "Coefficients",
      title = "(4) Proposed two-stage algorithm"
    ) +
    scale_y_continuous(
      breaks = seq(-0.2, 0.5, 0.1),
      limits = c(-0.3, 0.5)
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 0))
    ) +
    annotate("text",
      x = -0.01, y = location.beta, label = paste("V", 1:p, sep = ""),
      col = color.use
    ) +
    coord_cartesian(ylim = ylim, clip = "off") +
    geom_vline(xintercept = lambda.gic, linetype = "dashed", col = "black")

  ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

# ----------------------------------------------------
# functions to generate capture rate results in fig.3
# ----------------------------------------------------

# main function for one-time simulation
one.time.main.fig.3 <- function(n, p, s, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(n = n, p = p, s = s, rho = rho, nu = nu)
  x <- sim.data[[1]]
  y <- sim.data[[2]]
  true.index <- sim.data[[3]]
  noise.index <- setdiff(1:p, true.index)

  # method 1: scad
  scad <- ncvreg(x, y, penalty = "SCAD")
  yhat.m <- predict(scad, x)
  p.k <- apply(coef(scad), 2, function(z) sum(z != 0)) + 1

  scad.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((y - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))

  out.scad <- which(coef(scad)[, scad.lambda.index][-1] != 0)

  # method 2: mc+
  mcplus <- plus(x, y, method = "mc+")
  yhat.m <- predict(mcplus, newx = x, lam = mcplus$lam)$newy
  p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
  mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
    n * log(sum((y - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  out.mcplus <- which(coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ] != 0)

  # method 3: pro.sgpv
  out.sgpv <- pro.sgpv(x, y)$var.index

  # method 4: adaptive lasso
  lasso <- glmnet(x, y)
  init.est <- coef(lasso, s = lasso$lambda[floor(length(lasso$lambda) / 2)])[-1]
  adalasso <- glmnet(x, y, penalty.factor = 1 / (abs(init.est)))

  yhat.m <- predict(adalasso, newx = x)
  p.k <- apply(coef(adalasso), 2, function(z) sum(z != 0)) + 1

  adalasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((y - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))

  out.al <- which(coef(adalasso)[, adalasso.lambda.index][-1] != 0)

  # capture rate
  cover.m1 <- setequal(true.index, out.sgpv)
  cover.m2 <- setequal(true.index, out.mcplus)
  cover.m3 <- setequal(true.index, out.scad)
  cover.m4 <- setequal(true.index, out.al)

  # power
  correct.m1 <- sum(true.index %in% out.sgpv) / s
  correct.m2 <- sum(true.index %in% out.mcplus) / s
  correct.m3 <- sum(true.index %in% out.scad) / s
  correct.m4 <- sum(true.index %in% out.al) / s

  # type I error rate

  noise.index <- setdiff(1:p, true.index)

  type1.m1 <- sum(out.sgpv %in% noise.index) / (p - s)
  type1.m2 <- sum(out.mcplus %in% noise.index) / (p - s)
  type1.m3 <- sum(out.scad %in% noise.index) / (p - s)
  type1.m4 <- sum(out.al %in% noise.index) / (p - s)

  # false discovery rate
  fdr.m1 <- ifelse(length(out.sgpv) > 0,
    sum(out.sgpv %in% noise.index) / length(out.sgpv), 0
  )
  fdr.m2 <- ifelse(length(out.mcplus) > 0,
    sum(out.mcplus %in% noise.index) / length(out.mcplus), 0
  )
  fdr.m3 <- ifelse(length(out.scad) > 0,
    sum(out.scad %in% noise.index) / length(out.scad), 0
  )
  fdr.m4 <- ifelse(length(out.al) > 0,
    sum(out.al %in% noise.index) / length(out.al), 0
  )

  # false non discovery rate
  fndr.m1 <- ifelse(p == length(out.sgpv), 0,
    sum(setdiff(1:p, out.sgpv) %in% true.index) / (p - length(out.sgpv))
  )
  fndr.m2 <- ifelse(p == length(out.mcplus), 0,
    sum(setdiff(1:p, out.mcplus) %in% true.index) / (p - length(out.mcplus))
  )
  fndr.m3 <- ifelse(p == length(out.scad), 0,
    sum(setdiff(1:p, out.scad) %in% true.index) / (p - length(out.scad))
  )
  fndr.m4 <- ifelse(p == length(out.al), 0,
    sum(setdiff(1:p, out.al) %in% true.index) / (p - length(out.al))
  )

  return(c(
    cover.m1, cover.m2, cover.m3, cover.m4,
    correct.m1, correct.m2, correct.m3, correct.m4,
    type1.m1, type1.m2, type1.m3, type1.m4,
    fdr.m1, fdr.m2, fdr.m3, fdr.m4,
    fndr.m1, fndr.m2, fndr.m3, fndr.m4
  ))
}

# function to average over simulation
many.sim.main.fig.3 <- function(num.sim = 1000, n, p, s,
                                rho = rho, nu = nu) {
  out <- NULL
  invisible(capture.output(out <- replicate(
    num.sim,
    one.time.main.fig.3(
      n = n, p = p, s = s,
      rho = rho, nu = nu
    )
  )))

  return(apply(out, 1, mean))
}


# capture rate of the true model
get.plot.fig.3 <- function(data, cor = c("ind", "medium", "high"),
                           xaxis = c("n", "p"), p = 200, num.sim = 1e3) {
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )

  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (xaxis == "p") {
    plot.d <- data.frame(
      x = rep(seq(p, 5 * p, p / 10), each = 4),
      Method = rep(c("ProSGPV", "MC+", "SCAD", "Adaptive lasso"), 41),
      rate = c(data)
    )

    ci.d <- data.frame(
      x = rep(seq(p, 5 * p, p / 10), 4),
      method = rep(c("ProSGPV", "MC+", "SCAD", "Adaptive lasso"), each = 41)
    )
  } else {
    plot.d <- data.frame(
      x = rep(2:40, each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39),
      rate = c(data)
    )

    ci.d <- data.frame(
      x = rep(2:40, 4),
      method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), each = 39)
    )
  }

  plot.d$Method <- factor(plot.d$Method,
    levels = c("ProSGPV", "MC+", "SCAD", "Adaptive lasso")
  )

  lb.out <- NULL
  ub.out <- NULL

  for (i in 1:4) {
    pe <- as.numeric(data[i, ])
    lb.out <- c(lb.out, pe - 1.96 * sqrt(pe * (1 - pe) / num.sim))
    ub.out <- c(ub.out, pe + 1.96 * sqrt(pe * (1 - pe) / num.sim))
  }

  ci.d$lb <- lb.out
  ci.d$ub <- ub.out

  ci.d$lb[ci.d$lb < 0] <- 0
  ci.d$ub[ci.d$ub > 1] <- 1

  ci.d$method <- factor(ci.d$method, levels = c(
    "ProSGPV", "MC+",
    "SCAD", "Adaptive lasso"
  ))

  if (xaxis == "p") xlim <- c(p, 5 * p, p / 10) else xlim <- c(1, 40)
  if (xaxis == "p") xbreaks <- seq(p, 5 * p, p / 2) else xbreaks <- seq(2, 40, 4)
  xlab <- ifelse(xaxis == "p", "p", "n/p")

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = rate, col = Method)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(
      limits = xlim,
      breaks = xbreaks
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = xlab, y = "Capture rate", col = "Method",
      title = title.p
    ) +
    geom_ribbon(data = ci.d, aes(
      x = x, ymin = lb, ymax = ub,
      fill = method
    ), alpha = .4) +
    scale_fill_manual(values = dcols) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}

# -----------------------------------------------------------------------
# functions to generate parameter estimation and prediction in fig.4 & 5
# -----------------------------------------------------------------------

one.time.main.pe.pr <- function(n, p, s, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(
    n = round(n * 5 / 3), p = p, s = s, rho = rho, nu = nu
  )
  X <- sim.data[[1]]
  Y <- sim.data[[2]]
  true.index <- sim.data[[3]]
  true.beta <- sim.data[[4]]

  # generate training index (60%)
  train.index <- sample(1:round(n * 5 / 3), n, replace = F)
  test.index <- setdiff(1:length(Y), train.index)

  df <- data.frame(X = X, Y = Y)

  if (n > p) {

    # oracle mae
    oracle.lm <- lm(Y ~ ., data = df[train.index, c(true.index, ncol(df))])
    pe.oracle <- numeric(p)
    out.oracle.coef <- as.numeric(coef(oracle.lm)[-1])

    for (i in 1:length(true.index)) {
      pe.oracle[true.index[i]] <- out.oracle.coef[i]
    }

    mae.oracle <- mean(abs(pe.oracle - true.beta))

    oracle.pred <- predict(oracle.lm, newdata = df[test.index, c(true.index, ncol(df))])
    rmse.oracle <- sqrt(mean((oracle.pred - Y[test.index])^2))

    # method 1: prosgpv
    sgpv.out <- pro.sgpv(X[train.index, ], Y[train.index])
    sgpv.coef <- coef(sgpv.out)
    mae.sgpv <- mean(abs(true.beta - sgpv.coef))
    pe.sgpv <- mae.sgpv / mae.oracle
    sgpv.pred <- predict(sgpv.out, newdata = X[test.index, ])
    pr.sgpv <- sqrt(mean((sgpv.pred - Y[test.index])^2)) / rmse.oracle

    # method 2: mc+
    mcplus <- plus(X[train.index, ], Y[train.index], method = "mc+")
    yhat.m <- predict(mcplus, newx = X[train.index, ], lam = mcplus$lam)$newy
    p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
    mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    mcplus.coef <- coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ]
    mae.mcp <- mean(abs(true.beta - mcplus.coef))
    pe.mcplus <- mae.mcp / mae.oracle
    mcplus.pred <- predict(mcplus,
      newx = X[test.index, ],
      lam = mcplus$lam
    )$newy[mcplus.lambda.index, ]
    pr.mcplus <- sqrt(mean((mcplus.pred - Y[test.index])^2)) / rmse.oracle

    # method 3: scad
    scad <- ncvreg(X[train.index, ], Y[train.index], penalty = "SCAD")
    yhat.m <- predict(scad, X[train.index, ])
    p.k <- apply(coef(scad), 2, function(z) sum(z != 0)) + 1

    scad.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    scad.coef <- coef(scad)[, scad.lambda.index][-1]
    mae.scad <- mean(abs(true.beta - scad.coef))
    pe.scad <- mae.scad / mae.oracle
    scad.pred <- predict(scad, X[test.index, ], s = scad$lambda[scad.lambda.index])
    pr.scad <- sqrt(mean((scad.pred - Y[test.index])^2)) / rmse.oracle

    # method 4: adaptive lasso
    lasso <- glmnet(X[train.index, ], Y[train.index])
    init.est <- coef(lasso, s = lasso$lambda[floor(length(lasso$lambda) / 2)])[-1]
    adalasso <- glmnet(X[train.index, ], Y[train.index],
      penalty.factor = 1 / (abs(init.est))
    )
    yhat.m <- predict(adalasso, newx = X[train.index, ])
    p.k <- apply(coef(adalasso), 2, function(z) sum(z != 0)) + 1

    adalasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    adalasso.coef <- coef(adalasso)[, adalasso.lambda.index][-1]
    mae.al <- mean(abs(true.beta - adalasso.coef))
    pe.al <- mae.al / mae.oracle
    pred.al <- predict(adalasso,
      newx = X[test.index, ],
      s = adalasso$lambda[adalasso.lambda.index]
    )
    pr.al <- sqrt(mean((pred.al - Y[test.index])^2)) / rmse.oracle
  } else {

    # large p and small n case

    # method 1: prosgpv
    sgpv.out <- pro.sgpv(X[train.index, ], Y[train.index])
    sgpv.coef <- coef(sgpv.out)
    pe.sgpv <- mean(abs(true.beta - sgpv.coef))
    sgpv.pred <- predict(sgpv.out, newdata = X[test.index, ])
    pr.sgpv <- sqrt(mean((sgpv.pred - Y[test.index])^2))

    # method 2: mc+
    mcplus <- plus(X[train.index, ], Y[train.index], method = "mc+")
    yhat.m <- predict(mcplus, newx = X[train.index, ], lam = mcplus$lam)$newy
    p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
    mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    mcplus.coef <- coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ]
    pe.mcplus <- mean(abs(true.beta - mcplus.coef))
    mcplus.pred <- predict(mcplus,
      newx = X[test.index, ],
      lam = mcplus$lam
    )$newy[mcplus.lambda.index, ]
    pr.mcplus <- sqrt(mean((mcplus.pred - Y[test.index])^2))

    # method 3: scad
    scad <- ncvreg(X[train.index, ], Y[train.index], penalty = "SCAD")
    yhat.m <- predict(scad, X[train.index, ])
    p.k <- apply(coef(scad), 2, function(z) sum(z != 0)) + 1

    scad.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    scad.coef <- coef(scad)[, scad.lambda.index][-1]
    pe.scad <- mean(abs(true.beta - scad.coef))
    scad.pred <- predict(scad, X[test.index, ], s = scad$lambda[scad.lambda.index])
    pr.scad <- sqrt(mean((scad.pred - Y[test.index])^2))

    # method 4: adaptive lasso
    lasso <- glmnet(X[train.index, ], Y[train.index])
    init.est <- coef(lasso, s = lasso$lambda[floor(length(lasso$lambda) / 2)])[-1]
    adalasso <- glmnet(X[train.index, ], Y[train.index],
      penalty.factor = 1 / (abs(init.est))
    )
    yhat.m <- predict(adalasso, newx = X[train.index, ])
    p.k <- apply(coef(adalasso), 2, function(z) sum(z != 0)) + 1

    adalasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
      n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
    }))

    adalasso.coef <- coef(adalasso)[, adalasso.lambda.index][-1]
    pe.al <- mean(abs(true.beta - adalasso.coef))
    pred.al <- predict(adalasso,
      newx = X[test.index, ],
      s = adalasso$lambda[adalasso.lambda.index]
    )
    pr.al <- sqrt(mean((pred.al - Y[test.index])^2))
  }


  return(c(
    pe.sgpv, pe.mcplus, pe.scad, pe.al,
    pr.sgpv, pr.mcplus, pr.scad, pr.al
  ))
}


many.sim.main.pe.pr <- function(num.sim = 1e3, n, p, s,
                                rho = rho, nu = nu) {
  out <- NULL
  invisible(capture.output(
    out <- replicate(num.sim, one.time.main.pe.pr(
      n = n, p = p, s = s, rho = rho, nu = nu
    ))
  ))
  median.pe.sgpv <- median(out[1, ])
  l.pe.sgpv <- as.numeric(quantile(out[1, ], 0.25))
  u.pe.sgpv <- as.numeric(quantile(out[1, ], 0.75))

  median.pe.mcp <- median(out[2, ])
  l.pe.mcp <- as.numeric(quantile(out[2, ], 0.25))
  u.pe.mcp <- as.numeric(quantile(out[2, ], 0.75))

  median.pe.scad <- median(out[3, ])
  l.pe.scad <- as.numeric(quantile(out[3, ], 0.25))
  u.pe.scad <- as.numeric(quantile(out[3, ], 0.75))

  median.pe.al <- median(out[4, ])
  l.pe.al <- as.numeric(quantile(out[4, ], 0.25))
  u.pe.al <- as.numeric(quantile(out[4, ], 0.75))

  median.pr.sgpv <- median(out[5, ])
  l.pr.sgpv <- as.numeric(quantile(out[5, ], 0.25))
  u.pr.sgpv <- as.numeric(quantile(out[5, ], 0.75))

  median.pr.mcp <- median(out[6, ])
  l.pr.mcp <- as.numeric(quantile(out[6, ], 0.25))
  u.pr.mcp <- as.numeric(quantile(out[6, ], 0.75))

  median.pr.scad <- median(out[7, ])
  l.pr.scad <- as.numeric(quantile(out[7, ], 0.25))
  u.pr.scad <- as.numeric(quantile(out[7, ], 0.75))

  median.pr.al <- median(out[8, ])
  l.pr.al <- as.numeric(quantile(out[8, ], 0.25))
  u.pr.al <- as.numeric(quantile(out[8, ], 0.75))


  return(c(
    median.pe.sgpv, l.pe.sgpv, u.pe.sgpv,
    median.pe.mcp, l.pe.mcp, u.pe.mcp,
    median.pe.scad, l.pe.scad, u.pe.scad,
    median.pe.al, l.pe.al, u.pe.al,

    median.pr.sgpv, l.pr.sgpv, u.pr.sgpv,
    median.pr.mcp, l.pr.mcp, u.pr.mcp,
    median.pr.scad, l.pr.scad, u.pr.scad,
    median.pr.al, l.pr.al, u.pr.al
  ))
}


# ---------------------------------------------------------------
# functions to plot parameter estimation in fig.4
# ---------------------------------------------------------------

get.plot.fig.4 <- function(data, cor, p = 200, np = c("n", "p")) {


  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )


  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (np == "n") {
    plot.d <- data.frame(
      x = rep(2:40, each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(1, 40)
    xbreaks <- seq(2, 40, 4)
    xlab <- "n/p"
    ylab <- "Relative MAE"
    ylim <- c(1, 4)
    ybreaks <- seq(1, 4, 1)
  } else {
    plot.d <- data.frame(
      x = rep(seq(p, 5 * p, p / 10), each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 41),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(p, 5 * p, p / 10)
    xbreaks <- seq(p, 5 * p, p / 2)
    xlab <- "p"
    ylab <- "MAE"
    ylim <- c(0, 0.1)
    ybreaks <- seq(0, 0.1, 0.02)
  }


  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
    )
  )

  plot.d$ub[plot.d$ub > 4] <- 4

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = pe, col = Method)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(limits = xlim, breaks = xbreaks) +
    scale_y_continuous(limits = ylim, breaks = ybreaks) +
    labs(
      x = xlab, y = ylab, col = "Method",
      title = title.p
    ) +
    geom_ribbon(data = plot.d, aes(
      x = x, ymin = lb,
      ymax = ub, fill = Method
    ), alpha = .3) +
    scale_fill_manual(values = dcols) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}



# ---------------------------------------------------------------
# functions to plot parameter estimation in fig.5
# ---------------------------------------------------------------

get.plot.fig.5 <- function(data, cor, p = 200, np = c("n", "p")) {


  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )

  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (np == "n") {
    plot.d <- data.frame(
      x = rep(2:40, each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(1, 40)
    xbreaks <- seq(2, 40, 4)
    xlab <- "n/p"
    ylab <- "Relative RMSE"
    ylim <- c(1, 1.4)
    ybreaks <- seq(1, 1.4, 0.1)
  } else {
    plot.d <- data.frame(
      x = rep(seq(p, 5 * p, p / 10), each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 41),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(p, 5 * p, p / 10)
    xbreaks <- seq(p, 5 * p, p / 2)
    xlab <- "p"
    ylab <- "RMSE"
    ylim <- c(4.5, 8.5)
    ybreaks <- seq(4.5, 8.5, 1)
  }


  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
    )
  )

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = pe, col = Method)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(limits = xlim, breaks = xbreaks) +
    scale_y_continuous(limits = ylim, breaks = ybreaks) +
    labs(
      x = xlab, y = ylab, col = "Method",
      title = title.p
    ) +
    geom_ribbon(data = plot.d, aes(
      x = x, ymin = lb,
      ymax = ub, fill = Method
    ), alpha = .3) +
    scale_fill_manual(values = dcols) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


# ---------------------------------------------------------------
# functions to genearate sensitivity of null bound in supp.fig.1
# ---------------------------------------------------------------

# get the index of selected variables
get.out.supp.fig.1 <- function(candidate.index, xs, ys, n, p = 50) {
  out.original <- out.0 <- out.sigma <- out.i.sqrt.log.n <-
    out.sqrt.log.n <- integer(0)

  if (length(candidate.index) > 0) {
    # run fully relaxed LASSO
    f.l <- lm(ys ~ xs[, candidate.index])

    # get confidence bands
    pe <- summary(f.l)$coef[-1, 1]
    se <- summary(f.l)$coef[-1, 2]
    null.bound.p <- mean(se)

    null.bound.original <- null.bound.p
    null.bound.sqrt.log.n <- null.bound.p * sqrt(log(n / p))
    null.bound.i.sqrt.log.n <- null.bound.p / sqrt(log(n / p))
    null.bound.sigma <- summary(f.l)$sigma / 12
    null.bound.0 <- 0

    out.original <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.original)]
    out.sqrt.log.n <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.sqrt.log.n)]
    out.i.sqrt.log.n <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.i.sqrt.log.n)]
    out.sigma <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.sigma)]
    out.0 <- candidate.index[which(abs(pe) > 1.96 * se)]
  } else {
    null.bound.original <- null.bound.0 <-
      null.bound.sqrt.log.n <- null.bound.sigma <- null.bound.i.sqrt.log.n <- NA
  }


  return(list(
    null.bound.original, null.bound.sqrt.log.n,
    null.bound.sigma, null.bound.0, null.bound.i.sqrt.log.n,
    out.original, out.sqrt.log.n, out.sigma, out.0, out.i.sqrt.log.n
  ))
}

# main function
one.time.supp.fig.1 <- function(n, p, s, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(n = n, p = p, s = s, rho = rho, nu = nu)
  X <- sim.data[[1]]
  Y <- sim.data[[2]]
  true.index <- sim.data[[3]]

  # proposed method
  xs <- scale(X)
  ys <- scale(Y)

  n <- nrow(xs)

  lasso <- glmnet(xs, ys)
  yhat.m <- predict(lasso, newx = xs)
  p.k <- lasso$df + 2

  lasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((ys - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  lambda <- lasso$lambda[lasso.lambda.index]
  candidate.index <- which(coef(lasso, s = lambda)[-1] != 0)

  # original
  temp <- get.out.supp.fig.1(candidate.index, xs, ys, n)
  bound.original <- temp[[1]]
  index.original <- temp[[6]]

  # root log n
  bound.root.log.n <- temp[[2]]
  index.root.log.n <- temp[[7]]

  # sigma
  bound.sigma <- temp[[3]]
  index.sigma <- temp[[8]]

  # 0
  bound.0 <- temp[[4]]
  index.0 <- temp[[9]]

  # inverse root log n
  bound.i.root.log.n <- temp[[5]]
  index.i.root.log.n <- temp[[10]]


  return(c(
    bound.original,
    bound.root.log.n,
    bound.sigma,
    bound.0,
    bound.i.root.log.n,
    setequal(true.index, index.original),
    setequal(true.index, index.root.log.n),
    setequal(true.index, index.sigma),
    setequal(true.index, index.0),
    setequal(true.index, index.i.root.log.n)
  ))
}

many.sim.supp.fig.1 <- function(num.sim = 1e3, n, p = 50, s = 10,
                                rho, nu) {
  out <- NULL
  out <- replicate(num.sim, one.time.supp.fig.1(
    n = n, p = p, s = s, rho = rho, nu = nu
  ))

  return(c(
    median(out[1, ], na.rm = T), # median bound original
    quantile(out[1, ], 0.25, na.rm = T), # first quartile bound original
    quantile(out[1, ], 0.75, na.rm = T), # third quartile bound original

    median(out[2, ], na.rm = T), # median bound root log n
    quantile(out[2, ], 0.25, na.rm = T), # first quartile bound root log n
    quantile(out[2, ], 0.75, na.rm = T), # third quartile bound root log n

    median(out[3, ], na.rm = T), # median bound sigma
    quantile(out[3, ], 0.25, na.rm = T), # first quartile bound sigma
    quantile(out[3, ], 0.75, na.rm = T), # third quartile bound sigma

    median(out[4, ], na.rm = T), # median bound 0
    quantile(out[4, ], 0.25, na.rm = T), # first quartile bound 0
    quantile(out[4, ], 0.75, na.rm = T), # third quartile bound 0

    median(out[5, ], na.rm = T), # median bound inverse root log n
    quantile(out[5, ], 0.25, na.rm = T), # first quartile bound i. root log n
    quantile(out[5, ], 0.75, na.rm = T), # third quartile bound i. root log n

    mean(out[6, ]), # capture rate original
    mean(out[7, ]), # capture rate root log n
    mean(out[8, ]), # capture rate sigma
    mean(out[9, ]), # capture rate 0
    mean(out[10, ]) # capture rate inverse root log n
  ))
}


get.plot.supp.fig.1.bound <- function(data, cor) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )


  # color scheme
  dcols <- c("black", "springgreen3", "blue", "yellow4", "red")

  plot.d <- data.frame(
    n = rep(2:40, each = 5),
    Method = rep(c(
      "Original null bound", "Null bound root log n", "Null bound sigma",
      "0 null bound", "Null bound inverse"
    ), 39),
    rate = c(data[c(1, 4, 7, 10, 13), ])
  )

  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "Original null bound", "Null bound root log n", "Null bound inverse",
      "Null bound sigma",
      "0 null bound"
    )
  )

  # add confidence interval
  ci.d <- data.frame(
    n = rep(2:40, each = 5),
    method = rep(c(
      "Original null bound", "Null bound root log n", "Null bound sigma",
      "0 null bound", "Null bound inverse"
    ), 39),
    lb = c(data[c(2, 5, 8, 11, 14), ]),
    ub = c(data[c(3, 6, 9, 12, 15), ])
  )

  ci.d$method <- factor(ci.d$method,
    levels = c(
      "Original null bound",  "Null bound root log n", "Null bound inverse",
      "Null bound sigma",
      "0 null bound"
    )
  )

  ggplot() +
    geom_line(data = plot.d, aes(x = n, y = rate, col = Method)) +
    scale_color_manual(
      values = dcols,
      labels = c(
        "Original null bound",
        bquote("Null bound *" ~ sqrt(log("n/p"))),
        bquote("Null bound /" ~ sqrt(log("n/p"))),
        bquote(hat(sigma) ~ "/12"),
        "0 null bound"
      )
    ) +
    geom_ribbon(data = ci.d, aes(
      x = n, ymin = lb, ymax = ub,
      fill = method
    ), alpha = .2) +
    scale_fill_manual(values = dcols) +
    scale_x_continuous(limits = c(1, 40), breaks = seq(2, 40, 4)) +
    scale_y_continuous(limits = c(0, 0.12), breaks = seq(0, 0.12, 0.03)) +
    labs(
      x = "n/p", y = "Null bound size", col = "Method",
      title = title.p
    ) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


get.plot.supp.fig.1.capture <- function(data, cor, num.sim = 1e3) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )


  # color scheme
  dcols <- c("black", "springgreen3", "blue", "yellow4", "red")

  plot.d <- data.frame(
    n = rep(2:40, each = 5),
    Method = rep(c(
      "Original null bound", "Null bound root log n", "Null bound sigma",
      "0 null bound", "Null bound inverse"
    ), 39),
    rate = c(data[16:20, ])
  )

  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "Original null bound",  "Null bound root log n", "Null bound inverse",
      "Null bound sigma",
      "0 null bound"
    )
  )

  # add confidence interval
  ci.d <- data.frame(
    n = rep(2:40, 5),
    method = rep(c(
      "Original null bound", "Null bound root log n", "Null bound sigma",
      "0 null bound", "Null bound inverse"
    ), each = 39)
  )

  lb.out <- NULL
  ub.out <- NULL

  for (i in 16:20) {
    pe <- as.numeric(data[i, ])
    lb.out <- c(lb.out, pe - 1.96 * sqrt(pe * (1 - pe) / num.sim))
    ub.out <- c(ub.out, pe + 1.96 * sqrt(pe * (1 - pe) / num.sim))
  }

  ci.d$lb <- lb.out
  ci.d$ub <- ub.out

  ci.d[ci.d < 0] <- 0

  ci.d$method <- factor(ci.d$method,
    levels = c(
      "Original null bound",  "Null bound root log n", "Null bound inverse",
      "Null bound sigma",
      "0 null bound"
    )
  )

  ggplot() +
    geom_line(data = plot.d, aes(x = n, y = rate, col = Method)) +
    scale_color_manual(
      values = dcols,
      labels = c(
        "Original null bound",
        bquote("Null bound *" ~ sqrt(log("n/p"))),
        bquote("Null bound /" ~ sqrt(log("n/p"))),
        bquote(hat(sigma) ~ "/12"),
        "0 null bound"
      )
    ) +
    geom_ribbon(data = ci.d, aes(
      x = n, ymin = lb, ymax = ub,
      fill = method
    ), alpha = .2) +
    scale_fill_manual(values = dcols) +
    scale_x_continuous(limits = c(1, 40), breaks = seq(2, 40, 4)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = "n/p", y = "Capture rate", col = "Method",
      title = title.p
    ) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}



# --------------------------------------------------------------------
# functions to generate one-stage and two-stage results in supp.fig.2
# --------------------------------------------------------------------

# main function
one.time.supp.fig.2 <- function(n, p, s, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(n = n, p = p, s = s, rho = rho, nu = nu)
  X <- sim.data[[1]]
  Y <- sim.data[[2]]
  true.index <- sim.data[[3]]

  # one-stage
  out.sgpv.1s <- pro.sgpv(X, Y, stage = 1)$var.index

  # two-stage
  out.sgpv.2s <- pro.sgpv(X, Y, stage = 2)$var.index

  return(c(
    setequal(true.index, out.sgpv.1s),
    setequal(true.index, out.sgpv.2s)
  ))
}

many.sim.supp.fig.2 <- function(num.sim = 1e3, n, p = 50, s = 10,
                                rho, nu) {
  out <- NULL
  out <- replicate(num.sim, one.time.supp.fig.2(
    n = n, p = p, s = s, rho = rho, nu = nu
  ))

  return(apply(out, 1, mean))
}


get.plot.supp.fig.2 <- function(data, cor, num.sim = 1e3) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )


  # color scheme
  dcols <- c("springgreen3", "black")

  plot.d <- data.frame(
    n = rep(2:40, each = 2),
    Method = rep(c(
      "One-stage", "Two-stage"
    ), 39),
    rate = c(data)
  )

  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "One-stage", "Two-stage"
    )
  )

  # add confidence interval
  ci.d <- data.frame(
    n = rep(2:40, 2),
    method = rep(c(
      "One-stage", "Two-stage"
    ), each = 39)
  )

  lb.out <- NULL
  ub.out <- NULL

  for (i in 1:2) {
    pe <- as.numeric(data[i, ])
    lb.out <- c(lb.out, pe - 1.96 * sqrt(pe * (1 - pe) / num.sim))
    ub.out <- c(ub.out, pe + 1.96 * sqrt(pe * (1 - pe) / num.sim))
  }

  ci.d$lb <- lb.out
  ci.d$ub <- ub.out

  ci.d[ci.d < 0] <- 0

  ci.d$method <- factor(ci.d$method,
    levels = c(
      "One-stage", "Two-stage"
    )
  )

  ggplot() +
    geom_line(data = plot.d, aes(x = n, y = rate, col = Method)) +
    scale_color_manual(
      values = dcols,
      labels = c(
        "One-stage", "Two-stage"
      )
    ) +
    geom_ribbon(data = ci.d, aes(
      x = n, ymin = lb, ymax = ub,
      fill = method
    ), alpha = .2) +
    scale_fill_manual(values = dcols) +
    scale_x_continuous(limits = c(1, 40), breaks = seq(2, 40, 4)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = "n/p", y = "Capture rate", col = "Method",
      title = title.p
    ) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


# --------------------------------------------------------------------
# functions to plot power and type i error rate results in supp.fig.3
# --------------------------------------------------------------------

get.plot.supp.fig.3 <- function(t1.data, cor,
                                power.data, p = 200, xaxis = c("n", "p")) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )

  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (xaxis == "n") {
    plot.d <- data.frame(
      x = rep(rep(2:40, each = 4), 2),
      Method = rep(rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39), 2),
      rate = c(c(t1.data), c(power.data)),
      Type = rep(c("Type I Error rate", "Power"), each = 156)
    )
  } else {
    plot.d <- data.frame(
      x = rep(rep(seq(p, 5 * p, p / 10), each = 4), 2),
      Method = rep(rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 41), 2),
      rate = c(c(t1.data), c(power.data)),
      Type = rep(c("Type I Error rate", "Power"), each = 164)
    )
  }


  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
    )
  )

  plot.d$Type <- factor(plot.d$Type, levels = c("Type I Error rate", "Power"))


  if (xaxis == "p") xlim <- c(p, 5 * p, p / 10) else xlim <- c(1, 40)
  if (xaxis == "p") xbreaks <- seq(p, 5 * p, p / 2) else xbreaks <- seq(2, 40, 4)
  xlab <- ifelse(xaxis == "p", "p", "n/p")

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = rate, col = Method, linetype = Type)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(
      limits = xlim,
      breaks = xbreaks
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = xlab, y = "Average rate", col = "Method",
      title = title.p
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


# -----------------------------------------------------
# functions to plot fdr and fndr results in supp.fig.4
# -----------------------------------------------------

get.plot.supp.fig.4 <- function(fdr.data, cor,
                                fndr.data, p = 200, xaxis = c("n", "p")) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )

  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (xaxis == "n") {
    plot.d <- data.frame(
      x = rep(rep(2:40, each = 4), 2),
      Method = rep(rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39), 2),
      rate = c(c(fdr.data), c(fndr.data)),
      Type = rep(c("pFDR", "pFNDR"), each = 156)
    )
  } else {
    plot.d <- data.frame(
      x = rep(rep(seq(p, 5 * p, p / 10), each = 4), 2),
      Method = rep(rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 41), 2),
      rate = c(c(fdr.data), c(fndr.data)),
      Type = rep(c("pFDR", "pFNDR"), each = 164)
    )
  }


  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
    )
  )

  plot.d$Type <- factor(plot.d$Type, levels = c("pFDR", "pFNDR"))


  if (xaxis == "p") xlim <- c(p, 5 * p, p / 10) else xlim <- c(1, 40)
  if (xaxis == "p") xbreaks <- seq(p, 5 * p, p / 2) else xbreaks <- seq(2, 40, 4)
  xlab <- ifelse(xaxis == "p", "p", "n/p")

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = rate, col = Method, linetype = Type)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(
      limits = xlim,
      breaks = xbreaks
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = xlab, y = "Average rate", col = "Method",
      title = title.p
    ) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


# ------------------------------------------------------------------
# functions to generate sensitivity in mcplus in supp.fig.5
# ------------------------------------------------------------------

# main function
one.time.supp.fig.5 <- function(n = 200, p = 200, s = 4, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(n = round(n * 5 / 3), p = p, s = s, rho = rho, nu = nu)
  x <- sim.data[[1]]
  y <- sim.data[[2]]
  true.index <- sim.data[[3]]
  true.beta <- sim.data[[4]]

  # generate training index (60%)
  train.index <- sample(1:round(n * 5 / 3), n, replace = F)
  test.index <- setdiff(1:length(y), train.index)

  # method 1: gic
  mcplus <- plus(x[train.index, ], y[train.index], method = "mc+")
  yhat.m <- predict(mcplus, newx = x[train.index, ], lam = mcplus$lam)$newy
  p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
  mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
    n * log(sum((y[train.index] - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))

  mcplus.coef <- coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ]
  mcplus.index <- which(mcplus.coef != 0)
  pe.mcplus <- mean(abs(true.beta - mcplus.coef))
  mcplus.pred <- predict(mcplus,
    newx = x[test.index, ],
    lam = mcplus$lam
  )$newy[mcplus.lambda.index, ]
  pr.mcplus <- sqrt(mean((mcplus.pred - y[test.index])^2))

  # method 2: universal lambda

  lasso <- glmnet(x[train.index, ], y[train.index])
  lasso.coef <- coef(lasso, s = median(lasso$lambda))[-1]
  p0 <- sum(lasso.coef != 0)
  nn <- length(train.index)
  tau.2 <- sum((y[train.index] - x[train.index, ] %*% lasso.coef)^2) / (nn - p0)^2
  R <- tau.2 * (2 * p0 / p - 1) +
    sum((t(x[train.index, ]) %*% (y[train.index] - x[train.index, ] %*% lasso.coef))^2) /
      (p * (nn - p0)^2)
  delta <- nn / p
  sigma <- sqrt(abs(tau.2 - R / delta) * nn)
  lambda.mcplus.u <- sqrt(2 / nn * log(p)) * sigma
  mcplus.coef.u <- coef(mcplus, lam = lambda.mcplus.u)
  mcplus.u.index <- which(mcplus.coef.u != 0)
  pe.mcplus.u <- mean(abs(mcplus.coef.u - true.beta))
  pr.mcplus.u <- sqrt(mean((x[test.index, ] %*% mcplus.coef.u - y[test.index])^2))

  return(c(
    setequal(true.index, mcplus.index),
    pe.mcplus,
    pr.mcplus,
    setequal(true.index, mcplus.u.index),
    pe.mcplus.u,
    pr.mcplus.u
  ))
}

many.sim.supp.fig.5 <- function(num.sim = 1e3, n, p = 50, s = 4,
                                rho = rho, nu = nu) {
  out <- NULL
  invisible(capture.output(
    out <- replicate(num.sim, one.time.supp.fig.5(
      n = n, p = p, s = s, rho = rho, nu = nu
    ))
  ))
  return(c(
    mean(out[1, ]), # capture rate of the true model
    median(out[2, ]), # parameter estimation
    as.numeric(quantile(out[2, ], 0.25)),
    as.numeric(quantile(out[2, ], 0.75)),
    median(out[3, ]), # prediction
    as.numeric(quantile(out[3, ], 0.25)),
    as.numeric(quantile(out[3, ], 0.75)),

    mean(out[4, ]), # capture rate of the true model
    median(out[5, ]), # parameter estimation
    as.numeric(quantile(out[5, ], 0.25)),
    as.numeric(quantile(out[5, ], 0.75)),
    median(out[6, ]), # prediction
    as.numeric(quantile(out[6, ], 0.25)),
    as.numeric(quantile(out[6, ], 0.75))
  ))
}


get.plot.supp.fig.5 <- function(data, cor, outcome, num.sim = 1e3) {

  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )

  dcols <- c("black", "blue")

  if (outcome == "rate") {
    plot.d <- data.frame(
      n = rep(seq(200, 1000, 20), each = 2),
      Method = rep(c(
        "GIC", "Universal"
      ), 41),
      rate = c(data[c(1, 8), ])
    )

    plot.d$Method <- factor(plot.d$Method,
      levels = c(
        "GIC", "Universal"
      )
    )

    ylim <- c(0, 1)
    ybreaks <- seq(0, 1, 0.2)

    ci.d <- data.frame(
      x = rep(seq(200, 1000, 20), 2),
      method = rep(c(
        "GIC", "Universal"
      ), each = 41)
    )

    lb.out <- NULL
    ub.out <- NULL

    for (i in c(1, 8)) {
      pe <- as.numeric(data[i, ])
      lb.out <- c(lb.out, pe - 1.96 * sqrt(pe * (1 - pe) / num.sim))
      ub.out <- c(ub.out, pe + 1.96 * sqrt(pe * (1 - pe) / num.sim))
    }

    ci.d$lb <- lb.out
    ci.d$ub <- ub.out

    ci.d$lb[ci.d$lb < 0] <- 0
    ci.d$ub[ci.d$ub > 1] <- 1

    ci.d$method <- factor(ci.d$method, levels = c(
      "GIC", "Universal"
    ))
  } else if (outcome == "pe") {
    plot.d <- data.frame(
      x = rep(seq(200, 1000, 20), each = 2),
      Method = rep(c(
        "GIC", "Universal"
      ), 41),
      pe = c(data[c(2, 9), ]),
      lb = c(data[c(3, 10), ]),
      ub = c(data[c(4, 11), ])
    )

    ylab <- "MAE"
    ylim <- c(0, 0.1)
    ybreaks <- seq(0, 0.1, 0.02)
  } else if (outcome == "pr") {
    plot.d <- data.frame(
      x = rep(seq(200, 1000, 20), each = 2),
      Method = rep(c(
        "GIC", "Universal"
      ), 41),
      pe = c(data[c(5, 12), ]),
      lb = c(data[c(6, 13), ]),
      ub = c(data[c(7, 14), ])
    )

    ylab <- "RMSE"
    ylim <- c(4.5, 8.5)
    ybreaks <- seq(4.5, 8.5, 1)
  }

  if (outcome == "rate") {
    ggplot() +
      geom_line(data = plot.d, aes(x = n, y = rate, col = Method)) +
      scale_color_manual(values = dcols) +
      scale_x_continuous(limits = c(200, 1000), breaks = seq(200, 1000, 100)) +
      scale_y_continuous(limits = ylim, breaks = ybreaks) +
      labs(
        x = "p", y = "Average capture rate", col = "Method",
        title = title.p
      ) + 
      geom_ribbon(data = ci.d, aes(
        x = x, ymin = lb, ymax = ub,
        fill = method
      ), alpha = .3) +
      scale_fill_manual(values = dcols) +
      guides(fill = F) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      )
  } else if (outcome == "pe") {
    ggplot() +
      geom_line(data = plot.d, aes(x = x, y = pe, col = Method)) +
      scale_color_manual(values = dcols) +
      scale_x_continuous(limits = c(200, 1000), breaks = seq(200, 1000, 100)) +
      scale_y_continuous(limits = ylim, breaks = ybreaks) +
      labs(
        x = "p", y = ylab, col = "Method",
        title = title.p
      ) +
      geom_ribbon(data = plot.d, aes(
        x = x, ymin = lb,
        ymax = ub, fill = Method
      ), alpha = .3) +
      scale_fill_manual(values = dcols) +
      guides(fill = F) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      )
  } else if (outcome == "pr") {
    ggplot() +
      geom_line(data = plot.d, aes(x = x, y = pe, col = Method)) +
      scale_color_manual(values = dcols) +
      scale_x_continuous(limits = c(200, 1000), breaks = seq(200, 1000, 100)) +
      scale_y_continuous(limits = ylim, breaks = ybreaks) +
      labs(
        x = "p", y = ylab, col = "Method",
        title = title.p
      ) +
      geom_ribbon(data = plot.d, aes(
        x = x, ymin = lb,
        ymax = ub, fill = Method
      ), alpha = .3) +
      scale_fill_manual(values = dcols) +
      guides(fill = F) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      )
  }
}


# -------------------------------------------------
# functions to generate running time in supp.fig.6
# -------------------------------------------------


run.1.scad <- function(x, y) {
  n <- nrow(x)
  scad <- ncvreg(x, y, penalty = "SCAD")
  yhat.m <- predict(scad, x)
  p.k <- apply(coef(scad), 2, function(z) sum(z != 0)) + 1

  scad.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((y - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))

  out.scad <- which(coef(scad)[, scad.lambda.index][-1] != 0)
}

run.1.mcp <- function(x, y, n, p) {
  n <- nrow(x)
  mcplus <- plus(x, y, method = "mc+")
  yhat.m <- predict(mcplus, newx = x, lam = mcplus$lam)$newy
  p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
  mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
    n * log(sum((y - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  out.mcplus <- which(coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ] != 0)
}

run.1.sgpv <- function(x, y) {
  sth <- pro.sgpv(x, y)
}

run.1.al <- function(x, y) {
  n <- nrow(x)
  lasso <- glmnet(x, y)
  init.est <- coef(lasso, s = lasso$lambda[floor(length(lasso$lambda) / 2)])[-1]
  adalasso <- glmnet(x, y, penalty.factor = 1 / (abs(init.est)))

  yhat.m <- predict(adalasso, newx = x)
  p.k <- apply(coef(adalasso), 2, function(z) sum(z != 0)) + 1

  adalasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((y - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))

  out.al <- which(coef(adalasso)[, adalasso.lambda.index][-1] != 0)
}

# main function
one.time.supp.fig.6 <- function(n, p, s, rho, nu) {

  # generate data
  sim.data <- gen.sim.data(n = n, p = p, s = s, rho = rho, nu = nu)
  x <- sim.data[[1]]
  y <- sim.data[[2]]

  # method 1: SCAD
  time.scad <- as.numeric(system.time(run.1.scad(x, y))[3])

  # method 2: MCP
  invisible(capture.output(
    time.mcp <- as.numeric(system.time(run.1.mcp(x, y))[3])
  ))

  # method 3: proposed 2-stage method
  time.2s <- as.numeric(system.time(run.1.sgpv(x, y))[3])

  # method 4: adaptive lasso
  time.al <- as.numeric(system.time(run.1.al(x, y))[3])

  return(c(time.scad, time.mcp, time.2s, time.al))
}


many.sim.supp.fig.6 <- function(num.sim = 1e3, n, p, s, rho, nu) {
  out <- NULL
  out <- replicate(num.sim, one.time.supp.fig.6(
    n = n, s = s, p = p, rho = rho, nu = nu
  ))

  median.scad <- median(out[1, ])
  l.scad <- as.numeric(quantile(out[1, ], 0.25))
  u.scad <- as.numeric(quantile(out[1, ], 0.75))

  median.mcp <- median(out[2, ])
  l.mcp <- as.numeric(quantile(out[2, ], 0.25))
  u.mcp <- as.numeric(quantile(out[2, ], 0.75))

  median.sgpv <- median(out[3, ])
  l.sgpv <- as.numeric(quantile(out[3, ], 0.25))
  u.sgpv <- as.numeric(quantile(out[3, ], 0.75))

  median.al <- median(out[4, ])
  l.al <- as.numeric(quantile(out[4, ], 0.25))
  u.al <- as.numeric(quantile(out[4, ], 0.75))

  return(c(
    median.sgpv, l.sgpv, u.sgpv,
    median.mcp, l.mcp, u.mcp,
    median.scad, l.scad, u.scad,
    median.al, l.al, u.al
  ))
}

# function to plot the supp fig 6

get.plot.supp.fig.6 <- function(data, cor, p = 200, np = c("n", "p")) {
  
  data[data>0.5] <- 0.5
  # get keywords from data names
  title.p <- ifelse(cor == "ind", "Independent",
    ifelse(cor == "medium", "Medium correlation",
      ifelse(cor == "high", "High correlation", "Weird")
    )
  )


  # color scheme
  dcols <- c("black", "springgreen3", "blue", "red")

  if (np == "n") {
    plot.d <- data.frame(
      x = rep(2:40, each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 39),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(1, 40)
    xbreaks <- seq(2, 40, 4)
    xlab <- "n/p"
  } else {
    plot.d <- data.frame(
      x = rep(seq(p, 5 * p, p / 10), each = 4),
      Method = rep(c(
        "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
      ), 41),
      pe = c(data[c(1, 4, 7, 10), ]),
      lb = c(data[c(2, 5, 8, 11), ]),
      ub = c(data[c(3, 6, 9, 12), ])
    )

    xlim <- c(p, 5 * p)
    xbreaks <- seq(p, 5 * p, p / 2)
    xlab <- "p"
  }


  plot.d$Method <- factor(plot.d$Method,
    levels = c(
      "ProSGPV", "MC+", "SCAD", "Adaptive lasso"
    )
  )

  ggplot() +
    geom_line(data = plot.d, aes(x = x, y = pe, col = Method)) +
    scale_color_manual(values = dcols) +
    scale_x_continuous(limits = xlim, breaks = xbreaks) +
    scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
    labs(
      x = xlab, y = "Running time in seconds", col = "Method",
      title = title.p
    ) +
    geom_ribbon(data = plot.d, aes(
      x = x, ymin = lb,
      ymax = ub, fill = Method
    ), alpha = .3) +
    scale_fill_manual(values = dcols) +
    guides(fill = F) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
}


# -----------------------------
# Tehran housing data analysis
# -----------------------------

tehran.one.time <- function(X, Y, index.use = c(1:26, index.keep)){
  
  n <- nrow(X)
  
  # generate training and test indices
  train.index <- sample(1:n, n * 0.7, replace = F)
  test.index <- setdiff(1:n, train.index)
  
  # method 1: prosgpv
  sgpv.out <- pro.sgpv(X[train.index, index.use], Y[train.index])
  sgpv.coef <- coef(sgpv.out)
  sgpv.index <- sgpv.out$var.index
  sgpv.pred <- predict(sgpv.out, newdata = X[test.index, index.use])
  pr.sgpv <- sqrt(mean((sgpv.pred - Y[test.index])^2))
  
  # method 2: mc+
  mcplus <- plus(X[train.index, index.use], Y[train.index], method = "mc+")
  yhat.m <- predict(mcplus, newx = X[train.index, index.use], lam = mcplus$lam)$newy
  p.k <- apply(coef(mcplus, lam = mcplus$lam), 1, function(z) sum(z != 0)) + 1 # no intercept
  mcplus.lambda.index <- which.min(sapply(1:nrow(yhat.m), function(z) {
    n * log(sum((Y[train.index] - yhat.m[z, ])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  
  mcplus.coef <- coef(mcplus, lam = mcplus$lam)[mcplus.lambda.index, ]
  mcplus.index <- which(mcplus.coef != 0)
  mcplus.pred <- predict(mcplus,
                         newx = X[test.index, index.use],
                         lam = mcplus$lam
  )$newy[mcplus.lambda.index, ]
  pr.mcplus <- sqrt(mean((mcplus.pred - Y[test.index])^2))
  
  # method 3: scad
  scad <- ncvreg(X[train.index, index.use], Y[train.index], penalty = "SCAD")
  yhat.m <- predict(scad, X[train.index, index.use])
  p.k <- apply(coef(scad), 2, function(z) sum(z != 0)) + 1
  
  scad.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  
  scad.coef <- coef(scad)[, scad.lambda.index][-1]
  scad.index <- as.numeric(which(scad.coef!=0))
  scad.pred <- predict(scad, X[test.index, index.use], s = scad$lambda[scad.lambda.index])
  pr.scad <- sqrt(mean((scad.pred - Y[test.index])^2))
  
  # method 4: adaptive lasso
  lasso <- glmnet(X[train.index, index.use], Y[train.index])
  init.est <- coef(lasso, s = lasso$lambda[floor(length(lasso$lambda) / 2)])[-1]
  adalasso <- glmnet(X[train.index, index.use], Y[train.index],
                     penalty.factor = 1 / (abs(init.est))
  )
  yhat.m <- predict(adalasso, newx = X[train.index, index.use])
  p.k <- apply(coef(adalasso), 2, function(z) sum(z != 0)) + 1
  
  adalasso.lambda.index <- which.min(sapply(1:ncol(yhat.m), function(z) {
    n * log(sum((Y[train.index] - yhat.m[, z])^2)) + p.k[z] * log(log(n)) * log(p.k[z])
  }))
  
  adalasso.coef <- coef(adalasso)[, adalasso.lambda.index][-1]
  adalasso.index <- as.numeric(which(adalasso.coef!=0))
  pred.al <- predict(adalasso,
                     newx = X[test.index, index.use],
                     s = adalasso$lambda[adalasso.lambda.index]
  )
  pr.al <- sqrt(mean((pred.al - Y[test.index])^2))

  return(list(
    sgpv.index,
    length(sgpv.index),
    pr.sgpv,
    
    mcplus.index,
    length(mcplus.index),
    pr.mcplus,
    
    scad.index,
    length(scad.index),
    pr.scad,
    
    adalasso.index,
    length(adalasso.index),
    pr.al
    
  ))
  
  
}

# get histogram of model size
get.hist <- function(data, snr = c("high", "medium")) {
  
  hist.d <- data.frame(
    Size = unlist(data[c(2,5,8,11), ]),
    Algorithm = rep(c("ProSGPV", "MC+",
      "SCAD", "Adaptive lasso"
    ), 1e3)
  )
  hist.d$Algorithm <- factor(hist.d$Algorithm,
                             levels = c("ProSGPV", "MC+",
                                        "SCAD", "Adaptive lasso")
  )
  
  cols <- c("black", "springgreen3", "blue", "red")
  
  # histogram
  if(snr == "high"){
    ggplot(hist.d, aes(x = Size, fill = Algorithm)) +
      geom_density(alpha = 0.6, color = NA) +
      theme_classic() +
      scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
      scale_fill_manual(values = cols) +
      labs(x = "Model size", y = "Density", title = "High SNR: 26 variables total") 
  }else{
    ggplot(hist.d, aes(x = Size, fill = Algorithm)) +
      geom_density(alpha = 0.6, color = NA) +
      theme_classic() +
      scale_x_continuous(limits = c(0, 9), breaks = seq(0, 9, 1)) +
      scale_fill_manual(values = cols) +
      labs(x = "Model size", y = "Density", title = "Medium SNR: 9 variables total") 
  }
 
 
}


# get boxplot of prediction accuracy
get.box <- function(data, snr = c("high", "medium")) {
  
  box.d <- data.frame(
    error = unlist(data[c(3,6,9,12), ]),
    Method = rep(c("ProSGPV", "MC+",
                      "SCAD", "Adaptive lasso"
    ), 1e3)
  )
  box.d$Method <- factor(box.d$Method,
                             levels = c("ProSGPV", "MC+",
                                        "SCAD", "Adaptive lasso")
  )
  
  cols <- c("black", "springgreen3", "blue", "red")
  
  # boxplot
  if(snr == "high"){
    ggplot(box.d, aes(x = Method, fill = Method , y = error, color = Method)) +
      geom_boxplot(alpha = 0.6) +
      theme_classic() +  theme(legend.position = "none") +
      scale_fill_manual(values = cols) + 
      scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) +
      scale_color_manual(values = cols) +
      labs(x = "Method", y = "Prediction RMSE in the test set", title = "High SNR: 26 variables total") 
  }else{
    ggplot(box.d, aes(x = Method, fill = Method , y = error, color = Method)) +
      geom_boxplot(alpha = 0.6) + 
      theme_classic() +  theme(legend.position = "none") +
      scale_fill_manual(values = cols) + 
      scale_y_continuous(limits = c(0, 2500), breaks = seq(0, 2500, 500)) +
      scale_color_manual(values = cols) +
      labs(x = "Method", y = "Prediction RMSE in the test set", title = "Medium SNR: 9 variables total") 
  }
  
}





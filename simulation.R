# generate simulation data

library("ProSGPV")
library("MASS")
library("ncvreg")
library("plus")
library("doParallel")


source("utils.R")

# -----------------
# fig.2 algorithm
# -----------------

set.seed(55)
sim.data.fig.2 <- gen.data.fig.2()


# -----------------
# fig.3 simulation
# -----------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

out.long.ind.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0, nu = 0.7)
}

out.long.ind.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0, nu = 2)
}

out.long.medium.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0.35, nu = 0.7)
}

out.long.medium.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0.35, nu = 2)
}

out.long.high.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0.7, nu = 0.7)
}

out.long.high.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = n, p = 50, s = 10, rho = 0.7, nu = 2)
}

out.wide.ind.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = 200, p = p, s = 4, rho = 0, nu = 2)
}

out.wide.medium.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = 200, p = p, s = 4, rho = 0.35, nu = 2)
}

out.wide.high.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.fig.3(n = 200, p = p, s = 4, rho = 0.7, nu = 2)
}

# figure 3
fig.3.long.ind.medium <- out.long.ind.medium[1:4, ]
fig.3.long.medium.medium <- out.long.medium.medium[1:4, ]
fig.3.long.high.medium <- out.long.high.medium[1:4, ]
fig.3.long.ind.high <- out.long.ind.high[1:4, ]
fig.3.long.medium.high <- out.long.medium.high[1:4, ]
fig.3.long.high.high <- out.long.high.high[1:4, ]
fig.3.wide.ind.high <- out.wide.ind.high[1:4, ]
fig.3.wide.medium.high <- out.wide.medium.high[1:4, ]
fig.3.wide.high.high <- out.wide.high.high[1:4, ]


# supp fig 3

supp.fig.3.power.long.ind.medium <- out.long.ind.medium[5:8, ]
supp.fig.3.power.long.medium.medium <- out.long.medium.medium[5:8, ]
supp.fig.3.power.long.high.medium <- out.long.high.medium[5:8, ]
supp.fig.3.power.long.ind.high <- out.long.ind.high[5:8, ]
supp.fig.3.power.long.medium.high <- out.long.medium.high[5:8, ]
supp.fig.3.power.long.high.high <- out.long.high.high[5:8, ]
supp.fig.3.power.wide.ind.high <- out.wide.ind.high[5:8, ]
supp.fig.3.power.wide.medium.high <- out.wide.medium.high[5:8, ]
supp.fig.3.power.wide.high.high <- out.wide.high.high[5:8, ]

supp.fig.3.t1.long.ind.medium <- out.long.ind.medium[9:12, ]
supp.fig.3.t1.long.medium.medium <- out.long.medium.medium[9:12, ]
supp.fig.3.t1.long.high.medium <- out.long.high.medium[9:12, ]
supp.fig.3.t1.long.ind.high <- out.long.ind.high[9:12, ]
supp.fig.3.t1.long.medium.high <- out.long.medium.high[9:12, ]
supp.fig.3.t1.long.high.high <- out.long.high.high[9:12, ]
supp.fig.3.t1.wide.ind.high <- out.wide.ind.high[9:12, ]
supp.fig.3.t1.wide.medium.high <- out.wide.medium.high[9:12, ]
supp.fig.3.t1.wide.high.high <- out.wide.high.high[9:12, ]

# supp fig 4

supp.fig.4.fdr.long.ind.medium <- out.long.ind.medium[13:16, ]
supp.fig.4.fdr.long.medium.medium <- out.long.medium.medium[13:16, ]
supp.fig.4.fdr.long.high.medium <- out.long.high.medium[13:16, ]
supp.fig.4.fdr.long.ind.high <- out.long.ind.high[13:16, ]
supp.fig.4.fdr.long.medium.high <- out.long.medium.high[13:16, ]
supp.fig.4.fdr.long.high.high <- out.long.high.high[13:16, ]
supp.fig.4.fdr.wide.ind.high <- out.wide.ind.high[13:16, ]
supp.fig.4.fdr.wide.medium.high <- out.wide.medium.high[13:16, ]
supp.fig.4.fdr.wide.high.high <- out.wide.high.high[13:16, ]

supp.fig.4.fndr.long.ind.medium <- out.long.ind.medium[17:20, ]
supp.fig.4.fndr.long.medium.medium <- out.long.medium.medium[17:20, ]
supp.fig.4.fndr.long.high.medium <- out.long.high.medium[17:20, ]
supp.fig.4.fndr.long.ind.high <- out.long.ind.high[17:20, ]
supp.fig.4.fndr.long.medium.high <- out.long.medium.high[17:20, ]
supp.fig.4.fndr.long.high.high <- out.long.high.high[17:20, ]
supp.fig.4.fndr.wide.ind.high <- out.wide.ind.high[17:20, ]
supp.fig.4.fndr.wide.medium.high <- out.wide.medium.high[17:20, ]
supp.fig.4.fndr.wide.high.high <- out.wide.high.high[17:20, ]

# ---------------------------
# fig.4 and fig.5 simulation
# ---------------------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

fig.4.5.long.ind.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0, nu = 0.7)
}

fig.4.5.long.medium.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0.35, nu = 0.7)
}

fig.4.5.long.high.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0.7, nu = 0.7)
}

fig.4.5.long.ind.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0, nu = 2)
}

fig.4.5.long.medium.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0.35, nu = 2)
}

fig.4.5.long.high.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = n, p = 50, s = 10, rho = 0.7, nu = 2)
}

fig.4.5.wide.ind.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = 200, p = p, s = 4, rho = 0, nu = 2)
}

fig.4.5.wide.medium.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = 200, p = p, s = 4, rho = 0.35, nu = 2)
}

fig.4.5.wide.high.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.main.pe.pr(n = 200, p = p, s = 4, rho = 0.7, nu = 2)
}

fig.4.long.ind.medium <- fig.4.5.long.ind.medium[1:12, ]
fig.4.long.medium.medium <- fig.4.5.long.medium.medium[1:12, ]
fig.4.long.high.medium <- fig.4.5.long.high.medium[1:12, ]
fig.4.long.ind.high <- fig.4.5.long.ind.high[1:12, ]
fig.4.long.medium.high <- fig.4.5.long.medium.high[1:12, ]
fig.4.long.high.high <- fig.4.5.long.high.high[1:12, ]
fig.4.wide.ind.high <- fig.4.5.wide.ind.high[1:12, ]
fig.4.wide.medium.high <- fig.4.5.wide.medium.high[1:12, ]
fig.4.wide.high.high <- fig.4.5.wide.high.high[1:12, ]

fig.5.long.ind.medium <- fig.4.5.long.ind.medium[13:24, ]
fig.5.long.medium.medium <- fig.4.5.long.medium.medium[13:24, ]
fig.5.long.high.medium <- fig.4.5.long.high.medium[13:24, ]
fig.5.long.ind.high <- fig.4.5.long.ind.high[13:24, ]
fig.5.long.medium.high <- fig.4.5.long.medium.high[13:24, ]
fig.5.long.high.high <- fig.4.5.long.high.high[13:24, ]
fig.5.wide.ind.high <- fig.4.5.wide.ind.high[13:24, ]
fig.5.wide.medium.high <- fig.4.5.wide.medium.high[13:24, ]
fig.5.wide.high.high <- fig.4.5.wide.high.high[13:24, ]

# ----------------------
# supp.fig.1 simulation
# ----------------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

supp.fig.1.long.ind.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0, nu = 0.7)
}

supp.fig.1.long.medium.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0.35, nu = 0.7)
}

supp.fig.1.long.high.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0.7, nu = 0.7)
}

supp.fig.1.long.ind.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0, nu = 2)
}

supp.fig.1.long.medium.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0.35, nu = 2)
}

supp.fig.1.long.high.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.1(n = n, rho = 0.7, nu = 2)
}


# ----------------------
# supp.fig.2 simulation
# ----------------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

supp.fig.2.long.ind.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0, nu = 0.7)
}

supp.fig.2.long.medium.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0.35, nu = 0.7)
}

supp.fig.2.long.high.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0.7, nu = 0.7)
}

supp.fig.2.long.ind.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0, nu = 2)
}

supp.fig.2.long.medium.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0.35, nu = 2)
}

supp.fig.2.long.high.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.2(n = n, rho = 0.7, nu = 2)
}

# ----------------------
# supp.fig.5 simulation
# ----------------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

supp.fig.5.wide.ind.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.5(n = 200, p = p, s = 4, rho = 0, nu = 2)
}

supp.fig.5.wide.medium.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.5(n = 200, p = p, s = 4, rho = 0.35, nu = 2)
}

supp.fig.5.wide.high.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.5(n = 200, p = p, s = 4, rho = 0.7, nu = 2)
}


# ----------------------
# supp.fig.6 simulation
# ----------------------

set.seed(1)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
registerDoParallel(no_cores)

supp.fig.6.long.ind.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0, nu = 0.7)
}

supp.fig.6.long.medium.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0.35, nu = 0.7)
}

supp.fig.6.long.high.medium <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0.7, nu = 0.7)
}

supp.fig.6.long.ind.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0, nu = 2)
}

supp.fig.6.long.medium.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0.35, nu = 2)
}

supp.fig.6.long.high.high <- foreach(
  n = seq(100, 2000, 50),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = n, p = 50, s = 10, rho = 0.7, nu = 2)
}

supp.fig.6.wide.ind.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = 200, p = p, s = 10, rho = 0, nu = 2)
}

supp.fig.6.wide.medium.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = 200, p = p, s = 4, rho = 0.35, nu = 2)
}

supp.fig.6.wide.high.high <- foreach(
  p = seq(200, 1000, 20),
  .combine = cbind,
  .multicombine = TRUE
) %dopar% {
  many.sim.supp.fig.6(n = 200, p = p, s = 4, rho = 0.7, nu = 2)
}



# --------------------------------------------------------------------
# Tehran housing data analysis - full model 26 variables - R^2 = 0.98
# --------------------------------------------------------------------

x <- as.matrix(t.housing[, -ncol(t.housing)]) # 372 by 26
y <- t.housing[, "V9"] # 372

# ---------------------------------------
# high SNR: all 26 variables: R^2 = 0.98
# ---------------------------------------

summary(lm(V9 ~ ., data = t.housing)) # R^2 = 0.98

# -------------------------------------
# medium SNR: 9 variables: R^2 = 0.40
# -------------------------------------

# remove variables whose absolute correlation with V9 is above 0.6
index.drop <- which(abs(sapply(1:(ncol(t.housing) - 1), function(x) {
  cor(t.housing[, x], t.housing[, ncol(t.housing)])
})) > 0.5)

index.keep <- setdiff(1:(ncol(t.housing) - 1), index.drop)

length(index.keep) # 9

summary(lm(V9 ~ ., data = t.housing[
  ,
  setdiff(1:ncol(t.housing), index.drop)
])) # R^2=0.40

# -----------------------------------
# get results from 1000 experinments
# -----------------------------------

set.seed(1)

# IN THE FULL DATA
temp.full <- replicate(1e3, tehran.one.time(X, Y, 1:26))

# -------------------
# variable selection
# -------------------

# pro.sgpv selects 7, 9, 10, 12, 23 most often
sort(table(paste(temp.full[1, ])), decreasing = T)[1]

# mcplus selects 6 and 7 most often though less than 20% of the time
sort(table(paste(temp.full[4, ])), decreasing = T)[1]

# the model that scad selects with highest frequency has 1%
sort(table(paste(temp.full[7, ])), decreasing = T)[1]

# adaptive lasso selects 6 and 7 almost always
sort(table(paste(temp.full[10, ])), decreasing = T)[1]

# -----------
# model size
# -----------

hist.full <- get.hist(temp.full, snr = "high")

# ------------------------
# prediction performance
# ------------------------

box.full <- get.box(temp.full, snr = "high")


# IN A SUBSET
set.seed(1)

temp.part <- replicate(1e3, tehran.one.time(x, y, index.keep))

# -------------------
# variable selection
# -------------------

# pro.sgpv selects 1 2 7 8  most often
sort(table(paste(temp.part[1, ])), decreasing = T)[1]

# mcplus selects 0 94% of the time
sort(table(paste(temp.part[4, ])), decreasing = T)[1]

# the model that scad selects with highest frequency has 16%
sort(table(paste(temp.part[7, ])), decreasing = T)[1]

# adaptive lasso selects 1, 2, 3, 4, 6, 7, 8 almost always
sort(table(paste(temp.part[10, ])), decreasing = T)[1]

# -----------
# model size
# -----------

hist.part <- get.hist(temp.part, snr = "medium")

# ------------------------
# prediction performance
# ------------------------

box.part <- get.box(temp.part, snr = "medium")



# --------------
# save all data
# --------------

save(sim.data.fig.2,
  fig.3.long.ind.medium,
  fig.3.long.medium.medium,
  fig.3.long.high.medium,
  fig.3.long.ind.high,
  fig.3.long.medium.high,
  fig.3.long.high.high,
  fig.3.wide.ind.high,
  fig.3.wide.medium.high,
  fig.3.wide.high.high,
  fig.4.long.ind.medium,
  fig.4.long.medium.medium,
  fig.4.long.high.medium,
  fig.4.long.ind.high,
  fig.4.long.medium.high,
  fig.4.long.high.high,
  fig.4.wide.ind.high,
  fig.4.wide.medium.high,
  fig.4.wide.high.high,
  fig.5.long.ind.medium,
  fig.5.long.medium.medium,
  fig.5.long.high.medium,
  fig.5.long.ind.high,
  fig.5.long.medium.high,
  fig.5.long.high.high,
  fig.5.wide.ind.high,
  fig.5.wide.medium.high,
  fig.5.wide.high.high,
  supp.fig.1.long.ind.medium,
  supp.fig.1.long.medium.medium,
  supp.fig.1.long.high.medium,
  supp.fig.1.long.ind.high,
  supp.fig.1.long.medium.high,
  supp.fig.1.long.high.high,
  supp.fig.2.long.ind.medium,
  supp.fig.2.long.medium.medium,
  supp.fig.2.long.high.medium,
  supp.fig.2.long.ind.high,
  supp.fig.2.long.medium.high,
  supp.fig.2.long.high.high,
  supp.fig.3.power.long.ind.medium,
  supp.fig.3.power.long.medium.medium,
  supp.fig.3.power.long.high.medium,
  supp.fig.3.power.long.ind.high,
  supp.fig.3.power.long.medium.high,
  supp.fig.3.power.long.high.high,
  supp.fig.3.power.wide.ind.high,
  supp.fig.3.power.wide.medium.high,
  supp.fig.3.power.wide.high.high,
  supp.fig.3.t1.long.ind.medium,
  supp.fig.3.t1.long.medium.medium,
  supp.fig.3.t1.long.high.medium,
  supp.fig.3.t1.long.ind.high,
  supp.fig.3.t1.long.medium.high,
  supp.fig.3.t1.long.high.high,
  supp.fig.3.t1.wide.ind.high,
  supp.fig.3.t1.wide.medium.high,
  supp.fig.3.t1.wide.high.high,
  supp.fig.4.fdr.long.ind.medium,
  supp.fig.4.fdr.long.medium.medium,
  supp.fig.4.fdr.long.high.medium,
  supp.fig.4.fdr.long.ind.high,
  supp.fig.4.fdr.long.medium.high,
  supp.fig.4.fdr.long.high.high,
  supp.fig.4.fdr.wide.ind.high,
  supp.fig.4.fdr.wide.medium.high,
  supp.fig.4.fdr.wide.high.high,
  supp.fig.4.fndr.long.ind.medium,
  supp.fig.4.fndr.long.medium.medium,
  supp.fig.4.fndr.long.high.medium,
  supp.fig.4.fndr.long.ind.high,
  supp.fig.4.fndr.long.medium.high,
  supp.fig.4.fndr.long.high.high,
  supp.fig.4.fndr.wide.ind.high,
  supp.fig.4.fndr.wide.medium.high,
  supp.fig.4.fndr.wide.high.high,
  supp.fig.5.wide.ind.high,
  supp.fig.5.wide.medium.high,
  supp.fig.5.wide.high.high,
  supp.fig.6.long.ind.medium,
  supp.fig.6.long.medium.medium,
  supp.fig.6.long.high.medium,
  supp.fig.6.long.ind.high,
  supp.fig.6.long.medium.high,
  supp.fig.6.long.high.high,
  supp.fig.6.wide.ind.high,
  supp.fig.6.wide.medium.high,
  supp.fig.6.wide.high.high,
  hist.full,
  hist.part,
  box.full,
  box.part,
  file = "data.RData"
)

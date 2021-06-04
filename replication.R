# Code needed to replicate results in the manuscript

library("ProSGPV")
library("ggplot2")
library("ggpubr")
library("corrr")
library("dplyr")
library("wesanderson")
library("RColorBrewer")

load("data.Rdata")
source("utils.R")

# --------------------------------------------------------------
# figure 1: thresholding functions of five penalization methods
# --------------------------------------------------------------

png("Figures/Figure 1.png", units = "in", width = 9, height = 9, res = 300)
get.plot.fig.1()
dev.off()

# ----------------------------------
# figure 2: how the algorithm works
# ----------------------------------

png("Figures/Figure 2.png", width = 9, height = 6, units = "in", res = 300)
get.plot.fig.2(sim.data.fig.2)
dev.off()

# --------------------------------------------------
# figure 3: capture rates of the exactly true model
# --------------------------------------------------

gp.fig.3.long.ind.medium <- get.plot.fig.3(
  data = fig.3.long.ind.medium,
  cor = "ind", xaxis = "n"
)

gp.fig.3.long.medium.medium <- get.plot.fig.3(
  data = fig.3.long.medium.medium,
  cor = "medium", xaxis = "n"
)

gp.fig.3.long.high.medium <- get.plot.fig.3(
  data = fig.3.long.high.medium,
  cor = "high", xaxis = "n"
)

gp.fig.3.long.ind.high <- get.plot.fig.3(
  data = fig.3.long.ind.high,
  cor = "ind", xaxis = "n"
)

gp.fig.3.long.medium.high <- get.plot.fig.3(
  data = fig.3.long.medium.high,
  cor = "medium", xaxis = "n"
)

gp.fig.3.long.high.high <- get.plot.fig.3(
  data = fig.3.long.high.high,
  cor = "high", xaxis = "n"
)

gp.fig.3.wide.ind.high <- get.plot.fig.3(
  data = fig.3.wide.ind.high,
  cor = "ind", xaxis = "p"
)
gp.fig.3.wide.medium.high <- get.plot.fig.3(
  data = fig.3.wide.medium.high,
  cor = "medium", xaxis = "p"
)
gp.fig.3.wide.high.high <- get.plot.fig.3(
  data = fig.3.wide.high.high,
  cor = "high", xaxis = "p"
)

fig.3.long.medium.p <- ggarrange(gp.fig.3.long.ind.medium,
  gp.fig.3.long.medium.medium,
  gp.fig.3.long.high.medium,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.3.long.medium <- annotate_figure(fig.3.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.3.long.high.p <- ggarrange(gp.fig.3.long.ind.high,
  gp.fig.3.long.medium.high,
  gp.fig.3.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.3.long.high <- annotate_figure(fig.3.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.3.wide.high.p <- ggarrange(gp.fig.3.wide.ind.high,
  gp.fig.3.wide.medium.high,
  gp.fig.3.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.3.wide.high <- annotate_figure(fig.3.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)


png("Figures/Figure 3.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(fig.3.long.medium,
  fig.3.long.high,
  fig.3.wide.high,
  ncol = 1
)
dev.off()

# -------------------------------------
# Figure 4: parameter estimation error
# -------------------------------------

gp.fig.4.long.ind.medium <- get.plot.fig.4(
  data = fig.4.long.ind.medium, cor = "ind", np = "n"
)

gp.fig.4.long.medium.medium <- get.plot.fig.4(
  data = fig.4.long.medium.medium, cor = "medium", np = "n"
)

gp.fig.4.long.high.medium <- get.plot.fig.4(
  data = fig.4.long.high.medium, cor = "high", np = "n"
)

gp.fig.4.long.ind.high <- get.plot.fig.4(
  data = fig.4.long.ind.high, cor = "ind", np = "n"
)

gp.fig.4.long.medium.high <- get.plot.fig.4(
  data = fig.4.long.medium.high, cor = "medium", np = "n"
)

gp.fig.4.long.high.high <- get.plot.fig.4(
  data = fig.4.long.high.high, cor = "high", np = "n"
)

gp.fig.4.wide.ind.high <- get.plot.fig.4(
  data = fig.4.wide.ind.high, cor = "ind", np = "p"
)

gp.fig.4.wide.medium.high <- get.plot.fig.4(
  data = fig.4.wide.medium.high, cor = "medium", np = "p"
)

gp.fig.4.wide.high.high <- get.plot.fig.4(
  data = fig.4.wide.high.high, cor = "high", np = "p"
)

fig.4.long.medium.p <- ggarrange(gp.fig.4.long.ind.medium,
  gp.fig.4.long.medium.medium,
  gp.fig.4.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

fig.4.long.medium <- annotate_figure(fig.4.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.4.long.high.p <- ggarrange(gp.fig.4.long.ind.high,
  gp.fig.4.long.medium.high,
  gp.fig.4.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.4.long.high <- annotate_figure(fig.4.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.4.wide.high.p <- ggarrange(gp.fig.4.wide.ind.high,
  gp.fig.4.wide.medium.high,
  gp.fig.4.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.4.wide.high <- annotate_figure(fig.4.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

png("Figures/Figure 4.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(fig.4.long.medium,
  fig.4.long.high,
  fig.4.wide.high,
  ncol = 1
)
dev.off()


# ---------------------------
# Figure 5: prediction error
# ---------------------------

gp.fig.5.long.ind.medium <- get.plot.fig.5(
  data = fig.5.long.ind.medium, cor = "ind", np = "n"
)

gp.fig.5.long.medium.medium <- get.plot.fig.5(
  data = fig.5.long.medium.medium, cor = "medium", np = "n"
)

gp.fig.5.long.high.medium <- get.plot.fig.5(
  data = fig.5.long.high.medium, cor = "high", np = "n"
)

gp.fig.5.long.ind.high <- get.plot.fig.5(
  data = fig.5.long.ind.high, cor = "ind", np = "n"
)

gp.fig.5.long.medium.high <- get.plot.fig.5(
  data = fig.5.long.medium.high, cor = "medium", np = "n"
)

gp.fig.5.long.high.high <- get.plot.fig.5(
  data = fig.5.long.high.high, cor = "high", np = "n"
)

gp.fig.5.wide.ind.high <- get.plot.fig.5(
  data = fig.5.wide.ind.high, cor = "ind", np = "p"
)

gp.fig.5.wide.medium.high <- get.plot.fig.5(
  data = fig.5.wide.medium.high, cor = "medium", np = "p"
)

gp.fig.5.wide.high.high <- get.plot.fig.5(
  data = fig.5.wide.high.high, cor = "high", np = "p"
)

fig.5.long.medium.p <- ggarrange(gp.fig.5.long.ind.medium,
  gp.fig.5.long.medium.medium,
  gp.fig.5.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

fig.5.long.medium <- annotate_figure(fig.5.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.5.long.high.p <- ggarrange(gp.fig.5.long.ind.high,
  gp.fig.5.long.medium.high,
  gp.fig.5.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.5.long.high <- annotate_figure(fig.5.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

fig.5.wide.high.p <- ggarrange(gp.fig.5.wide.ind.high,
  gp.fig.5.wide.medium.high,
  gp.fig.5.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

fig.5.wide.high <- annotate_figure(fig.5.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

png("Figures/Figure 5.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(fig.5.long.medium,
  fig.5.long.high,
  fig.5.wide.high,
  ncol = 1
)
dev.off()


# --------------------------------------------------
# supplementary figure 1: sensitivity in null bound
# --------------------------------------------------

gp.bound.supp.fig.1.long.ind.medium <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.ind.medium, cor = "ind"
)

gp.capture.supp.fig.1.long.ind.medium <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.ind.medium, cor = "ind"
)

gp.bound.supp.fig.1.long.medium.medium <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.medium.medium, cor = "medium"
)

gp.capture.supp.fig.1.long.medium.medium <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.medium.medium, cor = "medium"
)

gp.bound.supp.fig.1.long.high.medium <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.high.medium, cor = "high"
)

gp.capture.supp.fig.1.long.high.medium <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.high.medium, cor = "high"
)

gp.bound.supp.fig.1.long.ind.high <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.ind.high, cor = "ind"
)

gp.capture.supp.fig.1.long.ind.high <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.ind.high, cor = "ind"
)

gp.bound.supp.fig.1.long.medium.high <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.medium.high, cor = "medium"
)

gp.capture.supp.fig.1.long.medium.high <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.medium.high, cor = "medium"
)

gp.bound.supp.fig.1.long.high.high <- get.plot.supp.fig.1.bound(
  data = supp.fig.1.long.high.high, cor = "high"
)

gp.capture.supp.fig.1.long.high.high <- get.plot.supp.fig.1.capture(
  data = supp.fig.1.long.high.high, cor = "high"
)

bound.supp.fig.1.long.medium.p <- ggarrange(gp.bound.supp.fig.1.long.ind.medium,
  gp.bound.supp.fig.1.long.medium.medium,
  gp.bound.supp.fig.1.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

bound.supp.fig.1 <- annotate_figure(bound.supp.fig.1.long.medium.p,
  top = text_grob("Null bound size, medium SNR, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

capture.supp.fig.1.long.medium.p <- ggarrange(gp.capture.supp.fig.1.long.ind.medium,
  gp.capture.supp.fig.1.long.medium.medium,
  gp.capture.supp.fig.1.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

capture.supp.fig.1.long.medium <- annotate_figure(capture.supp.fig.1.long.medium.p,
  top = text_grob("Support recovery, medium SNR",
    size = 14, face = "bold"
  )
)

bound.supp.fig.1.long.high.p <- ggarrange(gp.bound.supp.fig.1.long.ind.high,
  gp.bound.supp.fig.1.long.medium.high,
  gp.bound.supp.fig.1.long.high.high,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

bound.high.supp.fig.1 <- annotate_figure(bound.supp.fig.1.long.high.p,
  top = text_grob("Null bound size, high SNR, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

capture.supp.fig.1.long.high.p <- ggarrange(gp.capture.supp.fig.1.long.ind.high,
  gp.capture.supp.fig.1.long.medium.high,
  gp.capture.supp.fig.1.long.high.high,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

capture.supp.fig.1.long.high <- annotate_figure(capture.supp.fig.1.long.high.p,
  top = text_grob("Support recovery, high SNR",
    size = 14, face = "bold"
  )
)


png("Figures/Supp Fig 1.png", width = 12, height = 12, units = "in", res = 300)
ggarrange(bound.supp.fig.1,
  capture.supp.fig.1.long.medium,
  bound.high.supp.fig.1,
  capture.supp.fig.1.long.high,
  ncol = 1
)
dev.off()


# ------------------------------------------------
# supplementary figure 2: one-stage and two-stage
# ------------------------------------------------

gp.supp.fig.2.long.ind.medium <- get.plot.supp.fig.2(
  data = supp.fig.2.long.ind.medium, cor = "ind"
)

gp.supp.fig.2.long.medium.medium <- get.plot.supp.fig.2(
  data = supp.fig.2.long.medium.medium, cor = "medium"
)

gp.supp.fig.2.long.high.medium <- get.plot.supp.fig.2(
  data = supp.fig.2.long.high.medium, cor = "high"
)

gp.supp.fig.2.long.ind.high <- get.plot.supp.fig.2(
  data = supp.fig.2.long.ind.high, cor = "ind"
)

gp.supp.fig.2.long.medium.high <- get.plot.supp.fig.2(
  data = supp.fig.2.long.medium.high, cor = "medium"
)

gp.supp.fig.2.long.high.high <- get.plot.supp.fig.2(
  data = supp.fig.2.long.high.high, cor = "high"
)

supp.fig.2.long.medium.p <- ggarrange(gp.supp.fig.2.long.ind.medium,
  gp.supp.fig.2.long.medium.medium,
  gp.supp.fig.2.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

supp.fig.2.long.medium <- annotate_figure(supp.fig.2.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.2.long.high.p <- ggarrange(gp.supp.fig.2.long.ind.high,
  gp.supp.fig.2.long.medium.high,
  gp.supp.fig.2.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.2.long.high <- annotate_figure(supp.fig.2.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

png("Figures/Supp Fig 2.png", width = 9, height = 6, units = "in", res = 300)
ggarrange(supp.fig.2.long.medium,
  supp.fig.2.long.high,
  ncol = 1
)
dev.off()

# ----------------------------------------------------
# supplementary figure 3: type I error rate and power
# ----------------------------------------------------

gp.supp.fig.3.long.ind.medium <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.ind.medium, xaxis = "n",
  cor = "ind", power.data = supp.fig.3.power.long.ind.medium
)

gp.supp.fig.3.long.medium.medium <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.medium.medium, xaxis = "n",
  cor = "medium", power.data = supp.fig.3.power.long.medium.medium
)

gp.supp.fig.3.long.high.medium <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.high.medium, xaxis = "n",
  cor = "high", power.data = supp.fig.3.power.long.high.medium
)

gp.supp.fig.3.long.ind.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.ind.high, xaxis = "n",
  cor = "ind", power.data = supp.fig.3.power.long.ind.high
)

gp.supp.fig.3.long.medium.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.medium.high, xaxis = "n",
  cor = "medium", power.data = supp.fig.3.power.long.medium.high
)

gp.supp.fig.3.long.high.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.long.high.high, xaxis = "n",
  cor = "high", power.data = supp.fig.3.power.long.high.high
)

gp.supp.fig.3.wide.ind.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.wide.ind.high, xaxis = "p",
  cor = "ind", power.data = supp.fig.3.power.wide.ind.high
)

gp.supp.fig.3.wide.medium.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.wide.medium.high, xaxis = "p",
  cor = "medium", power.data = supp.fig.3.power.wide.medium.high
)

gp.supp.fig.3.wide.high.high <- get.plot.supp.fig.3(
  t1.data = supp.fig.3.t1.wide.high.high, xaxis = "p",
  cor = "high", power.data = supp.fig.3.power.wide.high.high
)

supp.fig.3.long.medium.p <- ggarrange(gp.supp.fig.3.long.ind.medium,
  gp.supp.fig.3.long.medium.medium,
  gp.supp.fig.3.long.high.medium,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.3.long.medium <- annotate_figure(supp.fig.3.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.3.long.high.p <- ggarrange(gp.supp.fig.3.long.ind.high,
  gp.supp.fig.3.long.medium.high,
  gp.supp.fig.3.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.3.long.high <- annotate_figure(supp.fig.3.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.3.wide.high.p <- ggarrange(gp.supp.fig.3.wide.ind.high,
  gp.supp.fig.3.wide.medium.high,
  gp.supp.fig.3.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.3.wide.high <- annotate_figure(supp.fig.3.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)


png("Figures/Supp Fig 3.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(supp.fig.3.long.medium,
  supp.fig.3.long.high,
  supp.fig.3.wide.high,
  ncol = 1
)
dev.off()


# -------------------------------------
# supplementary figure 4: fdr and fndr
# -------------------------------------

gp.supp.fig.4.long.ind.medium <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.ind.medium, xaxis = "n",
  cor = "ind", fndr.data = supp.fig.4.fndr.long.ind.medium
)

gp.supp.fig.4.long.medium.medium <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.medium.medium, xaxis = "n",
  cor = "medium", fndr.data = supp.fig.4.fndr.long.medium.medium
)

gp.supp.fig.4.long.high.medium <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.high.medium, xaxis = "n",
  cor = "high", fndr.data = supp.fig.4.fndr.long.high.medium
)

gp.supp.fig.4.long.ind.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.ind.high, xaxis = "n",
  cor = "ind", fndr.data = supp.fig.4.fndr.long.ind.high
)

gp.supp.fig.4.long.medium.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.medium.high, xaxis = "n",
  cor = "medium", fndr.data = supp.fig.4.fndr.long.medium.high
)

gp.supp.fig.4.long.high.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.long.high.high, xaxis = "n",
  cor = "high", fndr.data = supp.fig.4.fndr.long.high.high
)

gp.supp.fig.4.wide.ind.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.wide.ind.high, xaxis = "p",
  cor = "ind", fndr.data = supp.fig.4.fndr.wide.ind.high
)

gp.supp.fig.4.wide.medium.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.wide.medium.high, xaxis = "p",
  cor = "medium", fndr.data = supp.fig.4.fndr.wide.medium.high
)

gp.supp.fig.4.wide.high.high <- get.plot.supp.fig.4(
  fdr.data = supp.fig.4.fdr.wide.high.high, xaxis = "p",
  cor = "high", fndr.data = supp.fig.4.fndr.wide.high.high
)

supp.fig.4.long.medium.p <- ggarrange(gp.supp.fig.4.long.ind.medium,
  gp.supp.fig.4.long.medium.medium,
  gp.supp.fig.4.long.high.medium,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.4.long.medium <- annotate_figure(supp.fig.4.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.4.long.high.p <- ggarrange(gp.supp.fig.4.long.ind.high,
  gp.supp.fig.4.long.medium.high,
  gp.supp.fig.4.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.4.long.high <- annotate_figure(supp.fig.4.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.4.wide.high.p <- ggarrange(gp.supp.fig.4.wide.ind.high,
  gp.supp.fig.4.wide.medium.high,
  gp.supp.fig.4.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.4.wide.high <- annotate_figure(supp.fig.4.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)


png("Figures/Supp Fig 4.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(supp.fig.4.long.medium,
  supp.fig.4.long.high,
  supp.fig.4.wide.high,
  ncol = 1
)
dev.off()


# -------------------------------------------
# supplementary figure 5: sensitivity of mc+
# -------------------------------------------

gp.supp.fig.5.ind.rate <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.ind.high, cor = "ind", outcome = "rate"
)

gp.supp.fig.5.ind.pe <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.ind.high, cor = "ind", outcome = "pe"
)

gp.supp.fig.5.ind.pr <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.ind.high, cor = "ind", outcome = "pr"
)

gp.supp.fig.5.medium.rate <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.medium.high, cor = "medium", outcome = "rate"
)

gp.supp.fig.5.medium.pe <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.medium.high, cor = "medium", outcome = "pe"
)

gp.supp.fig.5.medium.pr <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.medium.high, cor = "medium", outcome = "pr"
)

gp.supp.fig.5.high.rate <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.high.high, cor = "high", outcome = "rate"
)

gp.supp.fig.5.high.pe <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.high.high, cor = "high", outcome = "pe"
)

gp.supp.fig.5.high.pr <- get.plot.supp.fig.5(
  data = supp.fig.5.wide.high.high, cor = "high", outcome = "pr"
)

supp.fig.5.rate.p <- ggarrange(gp.supp.fig.5.ind.rate,
  gp.supp.fig.5.medium.rate,
  gp.supp.fig.5.high.rate,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

supp.fig.5.rate <- annotate_figure(supp.fig.5.rate.p,
  top = text_grob("Capture rate, p > n, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

supp.fig.5.pe.p <- ggarrange(gp.supp.fig.5.ind.pe,
  gp.supp.fig.5.medium.pe,
  gp.supp.fig.5.high.pe,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

supp.fig.5.pe <- annotate_figure(supp.fig.5.pe.p,
  top = text_grob("Parameter estimation, p > n, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

supp.fig.5.pr.p <- ggarrange(gp.supp.fig.5.ind.pr,
  gp.supp.fig.5.medium.pr,
  gp.supp.fig.5.high.pr,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

supp.fig.5.pr <- annotate_figure(supp.fig.5.pr.p,
  top = text_grob("Prediction accuracy, p > n, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

png("Figures/Supp Fig 5.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(supp.fig.5.rate,
  supp.fig.5.pe,
  supp.fig.5.pr,
  ncol = 1
)
dev.off()



# -------------------------------------
# supplementary figure 6: running time
# -------------------------------------

gp.supp.fig.6.long.ind.medium <- get.plot.supp.fig.6(
  data = supp.fig.6.long.ind.medium, cor = "ind", np = "n"
)

gp.supp.fig.6.long.medium.medium <- get.plot.supp.fig.6(
  data = supp.fig.6.long.medium.medium, cor = "medium", np = "n"
)

gp.supp.fig.6.long.high.medium <- get.plot.supp.fig.6(
  data = supp.fig.6.long.high.medium, cor = "high", np = "n"
)

gp.supp.fig.6.long.ind.high <- get.plot.supp.fig.6(
  data = supp.fig.6.long.ind.high, cor = "ind", np = "n"
)

gp.supp.fig.6.long.medium.high <- get.plot.supp.fig.6(
  data = supp.fig.6.long.medium.high, cor = "medium", np = "n"
)

gp.supp.fig.6.long.high.high <- get.plot.supp.fig.6(
  data = supp.fig.6.long.high.high, cor = "high", np = "n"
)

gp.supp.fig.6.wide.ind.high <- get.plot.supp.fig.6(
  data = supp.fig.6.wide.ind.high, cor = "ind", np = "p"
)

gp.supp.fig.6.wide.medium.high <- get.plot.supp.fig.6(
  data = supp.fig.6.wide.medium.high, cor = "medium", np = "p"
)

gp.supp.fig.6.wide.high.high <- get.plot.supp.fig.6(
  data = supp.fig.6.wide.high.high, cor = "high", np = "p"
)

supp.fig.6.long.medium.p <- ggarrange(gp.supp.fig.6.long.ind.medium,
  gp.supp.fig.6.long.medium.medium,
  gp.supp.fig.6.long.high.medium,
  ncol = 3, common.legend = T,
  legend = "bottom"
)

supp.fig.6.long.medium <- annotate_figure(supp.fig.6.long.medium.p,
  top = text_grob("Medium SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.6.long.high.p <- ggarrange(gp.supp.fig.6.long.ind.high,
  gp.supp.fig.6.long.medium.high,
  gp.supp.fig.6.long.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.6.long.high <- annotate_figure(supp.fig.6.long.high.p,
  top = text_grob("High SNR, n > p, p = 50, s = 10",
    size = 14, face = "bold"
  )
)

supp.fig.6.wide.high.p <- ggarrange(gp.supp.fig.6.wide.ind.high,
  gp.supp.fig.6.wide.medium.high,
  gp.supp.fig.6.wide.high.high,
  ncol = 3, common.legend = T, legend = "bottom"
)

supp.fig.6.wide.high <- annotate_figure(supp.fig.6.wide.high.p,
  top = text_grob("High SNR, n < p, n = 200, s = 4",
    size = 14, face = "bold"
  )
)

png("Figures/Supp Fig 6.png", width = 9, height = 9, units = "in", res = 300)
ggarrange(supp.fig.6.long.medium,
  supp.fig.6.long.high,
  supp.fig.6.wide.high,
  ncol = 1
)
dev.off()


# ------------------------------------
# supp fig 7: correlation and cluster
# ------------------------------------

png("Figures/Supp Fig 7.png",
  width = 6, height = 6, units = "in", res = 300
)
t.housing %>%
  correlate() %>%
  network_plot(min_cor = 0.1, colours = c("red", "white", "blue"))
dev.off()

# ------------------------------------
# supp fig 8: size of selected model
# ------------------------------------

png("Figures/Supp Fig 8.png",
  width = 8, height = 6, units = "in", res = 300
)
ggarrange(hist.full, hist.part,
  ncol = 1
)
dev.off()

# ------------------------------------
# supp fig 9: prediction accuracy
# ------------------------------------

png("Figures/Supp Fig 9.png",
    width = 8, height = 4, units = "in", res = 300
)
ggarrange(box.full, box.part,
          ncol = 2
)
dev.off()

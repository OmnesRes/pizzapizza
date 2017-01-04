library(rpsychi)

#Example code - compare with real ANOVA from raw data in SPSS
m.mat <- matrix(c(0.6006575, 0.6041635, 0.4552795, 0.3481225, 0.6484595, 0.579725), ncol=2)
sd.mat <- matrix(c(0.20422236, 0.37179451, 0.29778293, 0.23264826, 0.2919474, 0.27165736), ncol=2)
n.mat <- matrix(c(20, 20, 20, 20, 20, 20), ncol=2)
au <- ind.twoway.second(unbiased=FALSE, m = m.mat, sd = sd.mat, n = n.mat)
ab <- ind.twoway.second(m = m.mat, sd = sd.mat, n = n.mat)

m.vec <- c(4.77, 6.24)
sd.vec <- c(1.93, 2.65)
n.vec <- c(46, 39)
t <- ind.oneway.second(unbiased=FALSE, m = m.vec, sd = sd.vec, n = n.vec)

f_range <- function (m.p, sd.p, n.p, title=FALSE, u.p=TRUE, show.t=FALSE, labels=c()) {
  if (class(m.p) == "matrix") {
    func <- ind.twoway.second
    ways <- 2
  }
  else {
    m.p <- matrix(m.p)
    func <- ind.oneway.second
    ways <- 1
  }

# Correct for rounding in reported numbers by adding possible discrepancy (assume 2 DPs).
# For maximum F estimate, we subtract .005 from all SDs; for minimum F estimate, we add .005.
# For maximum F estimate, we have two methods to adjust the means:
# 1. Add .005 to the highest mean and subtract .005 from the lowest mean (assumes means are positive!)
# 2. Add .005 to all means above the midpoint and subtract .005 from all means below the midpoint
# Then we choose whichever gives the widest range. (For 2-condition items, they will be identical.)
# For minimum F estimate, we perform the opposite operations to the above.
  m.hi1 <- m.p
  m.lo1 <- m.p
  m.hi2 <- m.p
  m.lo2 <- m.p
  for (j in 1:ncol(m.p)) {
    w.min <- which.min(m.p[,j])
    w.max <- which.max(m.p[,j])
    m.hi1[w.min, j] <- m.p[w.min, j] - 0.005
    m.hi1[w.max, j] <- m.p[w.max, j] + 0.005
    m.lo1[w.min, j] <- m.p[w.min, j] + 0.005
    m.lo1[w.max, j] <- m.p[w.max, j] - 0.005

    w.mean <- mean(m.p[,j])
    for (i in 1:length(m.p[,j])) {
      cur <- m.p[i, j]
      delta <- if (cur > w.mean) 0.005 else -0.005
      m.hi2[i, j] <- cur + delta
      m.lo2[i, j] <- cur - delta
    }
  }

  sd.hi <- sd.p - 0.005
  sd.lo <- sd.p + 0.005

  if (ways == 1) {
    useF <- 1
    default_labels <- c("F")
    if (show.t) {
      default_labels <- c("t")
    }
  }
  else {
    useF <- c(3, 2, 4)
    default_labels <- c("col F", "row F", "inter F")
  }

  if (length(labels) == 0) {
    labels <- default_labels
  }

  if (title != FALSE) {
    cat(title, " ")
  }

  f_nom <- func(m=m.p, sd=sd.p, n=n.p, unbiased=u.p)$anova.table$F
  f_hi1 <- func(m=m.hi1, sd=sd.hi, n=n.p, unbiased=u.p)$anova.table$F
  f_lo1 <- func(m=m.lo1, sd=sd.lo, n=n.p, unbiased=u.p)$anova.table$F
  f_hi2 <- func(m=m.hi2, sd=sd.hi, n=n.p, unbiased=u.p)$anova.table$F
  f_lo2 <- func(m=m.lo2, sd=sd.lo, n=n.p, unbiased=u.p)$anova.table$F

  f_hi <- pmax(f_hi1, f_hi2)
  f_lo <- pmin(f_lo1, f_lo2)
  
  if (show.t) {
    f_nom <- sqrt(f_nom)
    f_hi <-  sqrt(f_hi)
    f_lo <-  sqrt(f_lo)
  }

  sp <- ""
  for (i in 1:length(useF)) {
    cat(sp)
    j <- useF[i]
    cat(labels[i], ": ", sprintf("%.3f", f_nom[j]),
                   " (min=", sprintf("%.3f", f_lo[j]),
                   ", max=", sprintf("%.3f", f_hi[j]), ")",
        sep="")
    sp <- "  "
  }
  cat("\n", sep="")
}

cat("Eating Heavily - Table 1\n")

n.eh.t1.men <- c(40, 20)
n.eh.t1.women <- c(35, 10)

m.eh.t1.l1.men <- c(44, 43)
sd.eh.t1.l1.men <- c(18.86, 11.19)
f_range(m = m.eh.t1.l1.men, sd = sd.eh.t1.l1.men, n = n.eh.t1.men, title="Line 1, men", show.t=TRUE)

m.eh.t1.l2.men <- c(178.02, 181.11)
sd.eh.t1.l2.men <- c(7.72, 7.32)
f_range(m = m.eh.t1.l2.men, sd = sd.eh.t1.l2.men, n = n.eh.t1.men, title="Line 2, men", show.t=TRUE)

m.eh.t1.l3.men <- c(86.35, 100.80)
sd.eh.t1.l3.men <- c(17.92, 21.33)
f_range(m = m.eh.t1.l3.men, sd = sd.eh.t1.l3.men, n = n.eh.t1.men, title="Line 3, men", show.t=TRUE)

m.eh.t1.l4.men <- c(27.20, 30.96)
sd.eh.t1.l4.men <- c(5.13, 6.62)
f_range(m = m.eh.t1.l4.men, sd = sd.eh.t1.l4.men, n = n.eh.t1.men, title="Line 4, men", show.t=TRUE)

m.eh.t1.l1.women <- c(44.52, 48.18)
sd.eh.t1.l1.women <- c(17.09, 16.49)
f_range(m = m.eh.t1.l1.women, sd = sd.eh.t1.l1.women, n = n.eh.t1.women, title="Line 1, women", show.t=TRUE)

m.eh.t1.l2.women <- c(165.83, 164.82)
sd.eh.t1.l2.women <- c(7.71, 5.88)
f_range(m = m.eh.t1.l2.women, sd = sd.eh.t1.l2.women, n = n.eh.t1.women, title="Line 2, women", show.t=TRUE)

m.eh.t1.l3.women <- c(64.63, 75.54)
sd.eh.t1.l3.women <- c(10.95, 12.42)
f_range(m = m.eh.t1.l3.women, sd = sd.eh.t1.l3.women, n = n.eh.t1.women, title="Line 3, women", show.t=TRUE)

m.eh.t1.l4.women <- c(23.46, 27.77)
sd.eh.t1.l4.women <- c(3.53, 3.68)
f_range(m = m.eh.t1.l4.women, sd = sd.eh.t1.l4.women, n = n.eh.t1.women, title="Line 4, women", show.t=TRUE)

cat("\n")
cat("Eating Heavily - Table 2\n")

lab.eh.t2 <- c("gender", "group", "gender x group")
n.eh.t2 <- matrix(c(40, 20, 35, 10), ncol=2)

m.eh.t2.l1 <- matrix(c(5.00, 2.69, 4.83, 5.54), ncol=2)
sd.eh.t2.l1 <- matrix(c(2.99, 2.57, 2.71, 1.84), ncol=2)
f_range(m = m.eh.t2.l1, sd = sd.eh.t2.l1, n = n.eh.t2, title="Line 1", labels=lab.eh.t2)

m.eh.t2.l2 <- matrix(c(2.99, 1.55, 1.33, 1.05), ncol=2)
sd.eh.t2.l2 <- matrix(c(1.75, 1.07, 0.83, 1.38), ncol=2)
f_range(m = m.eh.t2.l2, sd = sd.eh.t2.l2, n = n.eh.t2, title="Line 2", labels=lab.eh.t2)

m.eh.t2.l3 <- matrix(c(2.67, 2.76, 2.73, 1.00), ncol=2)
sd.eh.t2.l3 <- matrix(c(2.04, 2.18, 2.16, 0.00), ncol=2)
f_range(m = m.eh.t2.l3, sd = sd.eh.t2.l3, n = n.eh.t2, title="Line 3", labels=lab.eh.t2)

m.eh.t2.l4 <- matrix(c(1.46, 1.90, 2.29, 1.18), ncol=2)
sd.eh.t2.l4 <- matrix(c(1.07, 1.48, 2.28, 0.40), ncol=2)
f_range(m = m.eh.t2.l4, sd = sd.eh.t2.l4, n = n.eh.t2, title="Line 4", labels=lab.eh.t2)

m.eh.t2.l5 <- matrix(c(478.75, 397.5, 463.61, 111.71), ncol=2)
sd.eh.t2.l5 <- matrix(c(290.67, 191.37, 264.25, 109.57), ncol=2)
f_range(m = m.eh.t2.l5, sd = sd.eh.t2.l5, n = n.eh.t2, title="Line 5", labels=lab.eh.t2)

m.eh.t2.l6 <- matrix(c(2.11, 2.27, 2.20, 1.91), ncol=2)
sd.eh.t2.l6 <- matrix(c(1.54, 1.75, 1.71, 2.12), ncol=2)
f_range(m = m.eh.t2.l6, sd = sd.eh.t2.l6, n = n.eh.t2, title="Line 6", labels=lab.eh.t2)

cat("\n")
cat("Eating Heavily - Table 3\n")

n.eh.t3 <- c(20, 21, 19)

m.eh.t3.l1 <- c(2.69, 5.55, 4.33)
sd.eh.t3.l1 <- c(2.57, 2.66, 3.31)
f_range(m = m.eh.t3.l1, sd = sd.eh.t3.l1, n = n.eh.t3, title="Line 1")

m.eh.t3.l2 <- c(1.55, 2.79, 3.13)
sd.eh.t3.l2 <- c(1.07, 1.54, 2.18)
f_range(m = m.eh.t3.l2, sd = sd.eh.t3.l2, n = n.eh.t3, title="Line 2")

m.eh.t3.l3 <- c(2.76, 2.92, 2.53)
sd.eh.t3.l3 <- c(2.19, 2.3, 1.81)
f_range(m = m.eh.t3.l3, sd = sd.eh.t3.l3, n = n.eh.t3, title="Line 3")

m.eh.t3.l4 <- c(1.9, 1.65, 1.47)
sd.eh.t3.l4 <- c(1.48, 1.34, 1.23)
f_range(m = m.eh.t3.l4, sd = sd.eh.t3.l4, n = n.eh.t3, title="Line 4")

m.eh.t3.l5 <- c(397.5, 409.52, 555.26)
sd.eh.t3.l5 <- c(191.38, 246.87, 321.84)
f_range(m = m.eh.t3.l5, sd = sd.eh.t3.l5, n = n.eh.t3, title="Line 5")

m.eh.t3.l6 <- c(2.27, 2.32, 1.95)
sd.eh.t3.l6 <- c(1.75, 1.77, 1.24)
f_range(m = m.eh.t3.l6, sd = sd.eh.t3.l6, n = n.eh.t3, title="Line 6")

cat("\n")
cat("Low Prices and High Regret - Table 2\n")

lab.lp.t2 <- c("price", "pieces", "price x pieces")
n.lp.t2 <- matrix(c(18, 18, 7, 17, 19, 10), ncol=2)

m.lp.t2.l1 <- matrix(c(2.63, 4.82, 6.00, 1.76, 3.53, 4.40), ncol=2)
sd.lp.t2.l1 <- matrix(c(2.06, 2.55, 2.00, 1.82, 2.39, 3.24), ncol=2)
f_range(m = m.lp.t2.l1, sd = sd.lp.t2.l1, n = n.lp.t2, title="Line 1", labels=lab.lp.t2)

m.lp.t2.l2 <- matrix(c(2.39, 3.44, 3.71, 2.26, 1.68, 2.90), ncol=2)
sd.lp.t2.l2 <- matrix(c(1.94, 2.47, 1.49, 1.79, 1.42, 2.08), ncol=2)
f_range(m = m.lp.t2.l2, sd = sd.lp.t2.l2, n = n.lp.t2, title="Line 2", labels=lab.lp.t2)

m.lp.t2.l3 <- matrix(c(2.17, 2.94, 2.43, 1.97, 1.45, 2.25), ncol=2)
sd.lp.t2.l3 <- matrix(c(1.88, 2.12, 1.51, 1.68, 0.94, 1.81), ncol=2)
f_range(m = m.lp.t2.l3, sd = sd.lp.t2.l3, n = n.lp.t2, title="Line 3", labels=lab.lp.t2)

m.lp.t2.l4 <- matrix(c(2.11, 3.89, 3.71, 1.67, 1.67, 3.50), ncol=2)
sd.lp.t2.l4 <- matrix(c(1.81, 2.59, 1.79, 1.28, 1.24, 2.74), ncol=2)
f_range(m = m.lp.t2.l4, sd = sd.lp.t2.l4, n = n.lp.t2, title="Line 4", labels=lab.lp.t2)

m.lp.t2.l5 <- matrix(c(2.50, 4.28, 4.57, 2.00, 2.14, 3.92), ncol=2)
sd.lp.t2.l5 <- matrix(c(2.20, 2.44, 2.22, 1.45, 1.77, 2.81), ncol=2)
f_range(m = m.lp.t2.l5, sd = sd.lp.t2.l5, n = n.lp.t2, title="Line 5", labels=lab.lp.t2)

cat("\n")
cat("Low Prices and High Regret - Table 3\n")

n.lp.t3.1p <- c(18, 19)
n.lp.t3.2p <- c(18, 21)
n.lp.t3.3p <- c(7, 12)

m.lp.t3.l1.1p <- c(2.63, 1.76)
sd.lp.t3.l1.1p <- c(2.06, 1.82)
f_range(m = m.lp.t3.l1.1p, sd = sd.lp.t3.l1.1p, n = n.lp.t3.1p, title="Line 1, 1 piece")

m.lp.t3.l1.2p <- c(4.82, 3.53)
sd.lp.t3.l1.2p <- c(2.55, 2.39)
f_range(m = m.lp.t3.l1.2p, sd = sd.lp.t3.l1.2p, n = n.lp.t3.2p, title="Line 1, 2 pieces")

m.lp.t3.l1.3p <- c(6.00, 4.40)
sd.lp.t3.l1.3p <- c(2.00, 3.24)
f_range(m = m.lp.t3.l1.3p, sd = sd.lp.t3.l1.3p, n = n.lp.t3.3p, title="Line 1, 3 pieces")

m.lp.t3.l2.1p <- c(2.39, 2.26)
sd.lp.t3.l2.1p <- c(1.94, 1.79)
f_range(m = m.lp.t3.l2.1p, sd = sd.lp.t3.l2.1p, n = n.lp.t3.1p, title="Line 2, 1 piece")

m.lp.t3.l2.2p <- c(3.44, 1.68)
sd.lp.t3.l2.2p <- c(2.48, 1.42)
f_range(m = m.lp.t3.l2.2p, sd = sd.lp.t3.l2.2p, n = n.lp.t3.2p, title="Line 2, 2 pieces")

m.lp.t3.l2.3p <- c(3.71, 2.90)
sd.lp.t3.l2.3p <- c(1.50, 2.08)
f_range(m = m.lp.t3.l2.3p, sd = sd.lp.t3.l2.3p, n = n.lp.t3.3p, title="Line 2, 3 pieces")

m.lp.t3.l3.1p <- c(2.17, 1.955)
sd.lp.t3.l3.1p <- c(1.89, 1.68)
f_range(m = m.lp.t3.l3.1p, sd = sd.lp.t3.l3.1p, n = n.lp.t3.1p, title="Line 3, 1 piece")

m.lp.t3.l3.2p <- c(2.94, 1.28)
sd.lp.t3.l3.2p <- c(2.13, 0.46)
f_range(m = m.lp.t3.l3.2p, sd = sd.lp.t3.l3.2p, n = n.lp.t3.2p, title="Line 3, 2 pieces")

m.lp.t3.l3.3p <- c(2.43, 2.10)
sd.lp.t3.l3.3p <- c(1.51, 1.91)
f_range(m = m.lp.t3.l3.3p, sd = sd.lp.t3.l3.3p, n = n.lp.t3.3p, title="Line 3, 3 pieces")

m.lp.t3.l4.1p <- c(2.11, 1.67)
sd.lp.t3.l4.1p <- c(1.81, 1.28)
f_range(m = m.lp.t3.l4.1p, sd = sd.lp.t3.l4.1p, n = n.lp.t3.1p, title="Line 4, 1 piece")

m.lp.t3.l4.2p <- c(3.89, 1.53)
sd.lp.t3.l4.2p <- c(2.59, 1.02)
f_range(m = m.lp.t3.l4.2p, sd = sd.lp.t3.l4.2p, n = n.lp.t3.2p, title="Line 4, 2 pieces")

m.lp.t3.l4.3p <- c(3.71, 3.50)
sd.lp.t3.l4.3p <- c(1.79, 2.95)
f_range(m = m.lp.t3.l4.3p, sd = sd.lp.t3.l4.3p, n = n.lp.t3.3p, title="Line 4, 3 pieces")

m.lp.t3.l5.1p <- c(2.50, 2.00)
sd.lp.t3.l5.1p <- c(2.20, 1.45)
f_range(m = m.lp.t3.l5.1p, sd = sd.lp.t3.l5.1p, n = n.lp.t3.1p, title="Line 5, 1 piece")

m.lp.t3.l5.2p <- c(4.28, 2.05)
sd.lp.t3.l5.2p <- c(2.44, 1.72)
f_range(m = m.lp.t3.l5.2p, sd = sd.lp.t3.l5.2p, n = n.lp.t3.2p, title="Line 5, 2 pieces")

m.lp.t3.l5.3p <- c(4.57, 4.00)
sd.lp.t3.l5.3p <- c(2.23, 3.02)
f_range(m = m.lp.t3.l5.3p, sd = sd.lp.t3.l5.3p, n = n.lp.t3.3p, title="Line 5, 3 pieces")

cat("\n")
cat("Lower Buffet Prices - Table 1\n")

n.lbp.t1 <- c(62, 60)

m.lbp.t1.l1 <- c(44.16, 46.08)
sd.lbp.t1.l1 <- c(18.99, 14.46)
f_range(m = m.lbp.t1.l1, sd = sd.lbp.t1.l1, n = n.lbp.t1, title="Age")

m.lbp.t1.l3 <- c(68.52, 67.91)
sd.lbp.t1.l3 <- c(3.95, 3.93)
f_range(m = m.lbp.t1.l3, sd = sd.lbp.t1.l3, n = n.lbp.t1, title="Height")

m.lbp.t1.l4 <- c(180.84, 182.31)
sd.lbp.t1.l4 <- c(48.37, 48.41)
f_range(m = m.lbp.t1.l4, sd = sd.lbp.t1.l4, n = n.lbp.t1, title="Weight")

m.lbp.t1.l5 <- c(3.00, 3.28)
sd.lbp.t1.l5 <- c(1.55, 1.29)
f_range(m = m.lbp.t1.l5, sd = sd.lbp.t1.l5, n = n.lbp.t1, title="Group size")

# Next line gives an F too small for rpsychi to calculate
#m.lbp.t1.l6 <- c(6.62, 6.64)
#sd.lbp.t1.l6 <- c(1.85, 2.06)
#f_range(m = m.lbp.t1.l6, sd = sd.lbp.t1.l6, n = n.lbp.t1, title="Hungry then")

m.lbp.t1.l7 <- c(1.88, 1.85)
sd.lbp.t1.l7 <- c(1.34, 1.75)
f_range(m = m.lbp.t1.l7, sd = sd.lbp.t1.l7, n = n.lbp.t1, title="Hungry now")

cat("\n")
cat("Lower Buffet Prices - Table 2\n")

n.lbp.t2 <- c(62, 60)

m.lbp.t2.l1 <- c(6.89, 7.44)
sd.lbp.t2.l1 <- c(1.39, 1.60)
f_range(m = m.lbp.t2.l1, sd = sd.lbp.t2.l1, n = n.lbp.t2, title="Overall taste")

m.lbp.t2.l2 <- c(7.08, 7.45)
sd.lbp.t2.l2 <- c(1.30, 1.60)
f_range(m = m.lbp.t2.l2, sd = sd.lbp.t2.l2, n = n.lbp.t2, title="Piece 1, taste")

m.lbp.t2.l3 <- c(7.08, 7.34)
sd.lbp.t2.l3 <- c(1.37, 1.70)
f_range(m = m.lbp.t2.l3, sd = sd.lbp.t2.l3, n = n.lbp.t2, title="Piece 1, satisfying")

m.lbp.t2.l4 <- c(7.05, 7.47)
sd.lbp.t2.l4 <- c(1.40, 1.55)
f_range(m = m.lbp.t2.l4, sd = sd.lbp.t2.l4, n = n.lbp.t2, title="Piece 1, enjoyable")

m.lbp.t2.l5 <- c(6.68, 7.97)
sd.lbp.t2.l5 <- c(1.49, 1.21)
f_range(m = m.lbp.t2.l5, sd = sd.lbp.t2.l5, n = n.lbp.t2, title="Piece 2, taste")

m.lbp.t2.l6 <- c(6.68, 7.97)
sd.lbp.t2.l6 <- c(1.49, 1.21)
f_range(m = m.lbp.t2.l6, sd = sd.lbp.t2.l6, n = n.lbp.t2, title="Piece 2, satisfying")

m.lbp.t2.l7 <- c(6.64, 7.81)
sd.lbp.t2.l7 <- c(1.48, 1.22)
f_range(m = m.lbp.t2.l7, sd = sd.lbp.t2.l7, n = n.lbp.t2, title="Piece 2, enjoyable")

m.lbp.t2.l8 <- c(6.15, 7.58)
sd.lbp.t2.l8 <- c(1.89, 1.39)
f_range(m = m.lbp.t2.l8, sd = sd.lbp.t2.l8, n = n.lbp.t2, title="Piece 3, taste")

m.lbp.t2.l9 <- c(6.16, 7.41)
sd.lbp.t2.l9 <- c(1.87, 1.55)
f_range(m = m.lbp.t2.l9, sd = sd.lbp.t2.l9, n = n.lbp.t2, title="Piece 3, satisfying")

m.lbp.t2.l10 <- c(5.98, 7.45)
sd.lbp.t2.l10 <- c(1.86, 1.52)
f_range(m = m.lbp.t2.l10, sd = sd.lbp.t2.l10, n = n.lbp.t2, title="Piece 3, enjoyable")

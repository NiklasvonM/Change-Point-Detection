library(data.table)
library(ggplot2)
library(latex2exp)
library(ggExtra)

n <- 1000
t <- 1
steps <- seq(0, t, length = n + 1)
#plot(steps,bm,type="l")

myFun <- function(t, s) {
  abs(bm[t] - bm[s] - (steps[t] - steps[s]) * bm[n + 1])
}

#values <- NULL
values <- fread("Verteilung von T unter H0 samples.csv")$value
dtST <- fread("Verteilung s t.csv")

#K <- 10000
i <- 1
ss <- dtST$s #NULL
ts <- dtST$t #NULL
while (TRUE) {
  if (i %% 100 == 0)
    print(i)
  maxCur <- 0
  sCur <- NA_real_
  tCur <- NA_real_
  bm <- c(0, cumsum(rnorm(n, 0, sqrt(t / n))))
  for (stepT in 1:(n + 1)) {
    for (stepS in 1:(n + 1)) {
      if (stepS >= stepT)
        next
        valCur <- myFun(stepT, stepS)
      if (valCur > maxCur) {
        maxCur <- valCur
        sCur <- stepS
        tCur <- stepT
      }

    }
  }
  values <- c(values, maxCur)
  ss <- c(ss, sCur)
  ts <- c(ts, tCur)
  i <- i + 1
}

dtST <- data.table(s = ss, t = ts)
fwrite(dtST, "Verteilung s t.csv")
dtPlotST <- dtST[, .(N = .N), by = .(s, t)]


ggplot(dtPlotST, aes(s, t, size = N)) +
  geom_point()
ggplot(dtPlotST, aes(s / (n + 1), t / (n + 1))) +
  geom_jitter() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

p <- ggplot(dtPlotST, aes(s / (n + 1), t / (n + 1))) +
  geom_point(alpha = 0) +
  geom_bin2d(bins = 70) + #geom_bin2d # geom_hex
scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  xlab("s") +
  ylab("t") +
  theme(legend.position = "none", axis.title.y = element_text(angle = 0, vjust = 0.6))


ggMarginal(p, type = "densigram")

#ggMarginal(data = dtPlotST, x = "s", y = "t")





fwrite(data.table(value = values), "Verteilung von T unter H0 samples.csv")

histogram <- hist(values, breaks = "FD") #FD #Scott
dtPlot <- data.table(
  x = histogram$mids,
  y = histogram$counts
)
dtPlot[, y := y / sum(y)]

cdf <- ecdf(values)
dtCDF <- data.table(x = seq(min(dtPlot$x), max(dtPlot$x), by = 0.01))
dtCDF[, y := cdf(x)]
dtCDF[, yScaled := y * max(dtPlot$y)]

ggplot() +
  geom_bar(aes(x, y), dtPlot, stat = "identity") +
  geom_line(aes(x, yScaled), dtCDF) +
  theme_minimal() +
  xlab(TeX("")) +
#ylab("Density") +
scale_x_continuous(labels = TeX(paste0(seq(0, 3, by = 0.5), "")), breaks = seq(0, 3, by = 0.5)) + #$\\sigma$
scale_y_continuous(name = "pdf", sec.axis = sec_axis(~. / max(dtPlot$y), name = "cdf")) +
  ggtitle(TeX("Estimation of the test statistic $T$"))




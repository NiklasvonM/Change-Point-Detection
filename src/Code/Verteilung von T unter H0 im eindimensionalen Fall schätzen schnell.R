library(data.table)
library(ggplot2)
library(latex2exp)
library(ggExtra)

n <- 1000
t <- 1

values <- fread("Verteilung von T unter H0 samples.csv")$value
i <- 1
while (TRUE) {
  if (i %% 100 == 0) print(i)
  bm <- c(0, cumsum(rnorm(n, 0, sqrt(t / n))))
  bb <- bm - seq(0, 1, length.out = n + 1) * bm[n + 1]
  valuePairs <- as.data.table(expand.grid(bbs = bb, bbt = bb))
  valuePairs[, statisticValue := abs(bbt - bbs)]
  T_sample_cur <- max(valuePairs)
  values <- c(values, T_sample_cur)
  i <- i + 1
}

fwrite(data.table(value = values), "Verteilung von T unter H0 samples.csv")

histogram <- hist(values, breaks = "FD") #FD #Scott

dtPlot <- data.table(
  x = histogram$mids,
  y = histogram$density
)
#dtPlot[, y := y / sum(y)]

cdf <- ecdf(values)
dtCDF <- data.table(x = seq(min(dtPlot$x), max(dtPlot$x), length.out = 100000))
dtCDF[, y := cdf(x)]
dtCDF[, yScaled := y * max(dtPlot$y)]

p <- ggplot() +
  geom_bar(aes(x, y), dtPlot, stat = "identity", fill = rgb(0.2, 0.4, 0.6, 0.7)) + #, fill = "brown4"
geom_step(aes(x, yScaled), dtCDF, size = 1.5, color = "#5D4A66") +
  theme_minimal() +
  xlab(TeX("")) +
#ylab("Density") +
scale_x_continuous(
    labels = TeX(paste0(ifelse(seq(0, 3, by = 0.5) == 1, "", seq(0, 3, by = 0.5)), "$\\sigma$")),
    breaks = seq(0, 3, by = 0.5)) + #
scale_y_continuous(
    breaks = seq(0, 1.5, by = 0.5),
    labels = TeX(paste0("$", ifelse(seq(0, 1.5, by = 0.5) == 1, "", seq(0, 1.5, by = 0.5)), ifelse(seq(0, 1.5, by = 0.5) == 0, "", "\\sigma^{-1}$"))),
    name = "pdf",
    sec.axis = sec_axis(~. / max(dtPlot$y), name = "cdf")
  ) +
  ggtitle(TeX("Estimation of the test statistic $T$"))
p

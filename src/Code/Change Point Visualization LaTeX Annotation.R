library(ggplot2)
library(latex2exp)
library(data.table)

n <- 1000
alpha <- 0.3
beta <- 0.3
Delta <- 1.5
l <- floor(n * alpha)
u <- floor((1 - beta) * n)
dt <- data.table(j = seq_len(n))
dt[, TildeX_j := rnorm(n, sd = 0.5)]
dt[, X_j := TildeX_j + ifelse(l < j & j <= u, Delta, 0)]

labelLeftAlignBy <- 100
bottomValue <- min(dt$X_j)
size <- 5

ggplot(dt, aes(j, X_j)) +
  geom_point(color = "brown4", size = 2) +
  xlab(TeX("j")) +
  ylab(TeX("${}_{n}X_j$")) +
  annotate("text", x = -labelLeftAlignBy, y = 0, label = TeX("\\mu", output = "character"), parse = TRUE, size = size) +
  annotate("text", x = -labelLeftAlignBy, y = Delta, label = TeX("\\mu+\\Delta", output = "character"), parse = TRUE, size = size) +
  geom_line(aes(x, y), data = data.frame(x = c(-labelLeftAlignBy / 2, n), y = c(Delta, Delta)), linetype = "dashed") +
  geom_line(aes(x, y), data = data.frame(x = c(-labelLeftAlignBy / 2, n), y = c(0, 0)), linetype = "dashed") +
  geom_segment(aes(x = l, y = bottomValue, yend = Inf, xend = l)) +
  geom_segment(aes(x = u, y = bottomValue, yend = Inf, xend = u)) +
  annotate("text", x = l, y = bottomValue - 0.5, label = TeX("$l_n$", output = "character"), parse = TRUE, size = size) +
  annotate("text", x = u, y = bottomValue - 0.5, label = TeX("$u_n$", output = "character"), parse = TRUE, size = size) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 20, face = "bold", angle = 0, vjust = 0.5),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.text = element_blank()
  )

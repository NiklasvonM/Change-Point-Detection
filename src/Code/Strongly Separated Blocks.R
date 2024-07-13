library(ggplot2)
library(data.table)
library(latex2exp)


a_1 <- c(0, 0.3, 0.9)
b_1 <- c(0.2, 0.7, 1)
a_2 <- c(0, 0.3, 0.6, 0.8)
b_2 <- c(0.2, 0.5, 0.7, 1)

dt <- merge(
  data.table(xmin = a_1, xmax = b_1, by = 1),
  data.table(ymin = a_2, ymax = b_2, by = 1),
  by = "by",
  all = TRUE,
  allow.cartesian = TRUE
)

zip <- function(v, w) {
  stopifnot(length(v) == length(w))
  as.vector(sapply(seq_len(length(v)), function(i) c(v[i], w[i])))
}

ggplot(dt, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = "black") +
  theme_minimal() +
  annotate(
    "text",
    x = zip(a_1, b_1),
    y = -0.05,
    label = zip(
      TeX(paste0("$a_1^{(", seq_len(length(a_1)), ")}$"), output = "character"),
      TeX(paste0("$b_1^{(", seq_len(length(b_1)), ")}$"), output = "character")
    ),
    parse = TRUE
  ) +
  annotate(
    "text",
    y = zip(a_2, b_2),
    x = -0.05,
    label = zip(
      TeX(paste0("$a_2^{(", seq_len(length(a_2)), ")}$"), output = "character"),
      TeX(paste0("$b_2^{(", seq_len(length(b_2)), ")}$"), output = "character")
    ),
    parse = TRUE
  ) +
  annotate(
    "text",
    x = rep((a_1 + b_1) / 2, times = length(a_2)),
    y = rep((a_2 + b_2) / 2, times = length(a_1)),
    label = c(1, 5, 9, 10, 2, 6, 7, 11, 3, 4, 8, 12), #seq_len(length(a_1) * length(a_2)),
    color = "white"
  ) +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
  scale_x_continuous(limits = c(-0.05, 1.05)) +
  scale_y_continuous(limits = c(-0.05, 1.05))



# Obere und unterer Index vertauscht
ggplot(dt, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = "black") +
  theme_minimal() +
  annotate(
    "text",
    x = zip(a_1, b_1),
    y = -0.05,
    label = zip(
      TeX(paste0("$a^1_{", seq_len(length(a_1)), "}$"), output = "character"),
      TeX(paste0("$b^1_{", seq_len(length(b_1)), "}$"), output = "character")
    ),
    parse = TRUE
  ) +
  annotate(
    "text",
    y = zip(a_2, b_2),
    x = -0.05,
    label = zip(
      TeX(paste0("$a^2_{", seq_len(length(a_2)), "}$"), output = "character"),
      TeX(paste0("$b^2_{", seq_len(length(b_2)), "}$"), output = "character")
    ),
    parse = TRUE
  ) +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
  scale_x_continuous(limits = c(-0.05, 1.05)) +
  scale_y_continuous(limits = c(-0.05, 1.05))



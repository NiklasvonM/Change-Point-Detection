# Note that this script may take a while to finish...

library(geoR)
library(data.table)
library(ggplot2)
sim1 <- grf(10000, cov.pars = c(1, .25), grid="reg")
sim2 <- grf(1000, cov.pars = c(1, .25), grid="reg")


dt <- as.data.table(cbind(sim1[[1]], sim1$data))

dtPlot <- dt[, .(value = mean(V3)), by = .(x = floor(x * 100) / 100, y = floor(y * 100) / 100)]

dtAdd <- as.data.table(cbind(sim2[[1]], sim2$data))
dtAdd2 <- dtAdd[, .(valueAdd = mean(V3)), by = .(x = floor(x * 10) / 100, y = floor(y * 10) / 100)]
dtAdd2[, x := x + 0.3]
dtAdd2[, y := y + 0.4]

dtPlot <- merge(dtPlot, dtAdd2, by = c("x", "y"), all.x = TRUE)
dtPlot[!is.na(valueAdd), valueWithChangePoint := value + valueAdd * 3]
dtPlot[is.na(valueAdd), valueWithChangePoint := value]

ggplot(dtPlot[x < 0.9 & y < 0.9], aes(x, y, fill = valueWithChangePoint)) +
  geom_tile() +
  theme_minimal() + 
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank())

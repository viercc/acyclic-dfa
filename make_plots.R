library(ggplot2)

srcfile <- commandArgs(TRUE)[[1]]

data <- read.csv(srcfile)

dataCombine <- data[data$Category=='combine',]
dataQuery <- data[data$Category=='query',]

plt <- ggplot(dataCombine, aes(x=Method, y=Mean, color=Name)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchCombine.png", plot=plt, dpi=120, width=6, height=4)

plt <- ggplot(dataQuery, aes(x=Method, y=Mean, color=Name)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchQuery.png", plot=plt, dpi=120, width=6, height=6)

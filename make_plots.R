library(ggplot2)

srcfile <- commandArgs(TRUE)[[1]]

data <- read.csv(srcfile)

dataCombineDict <- data[data$Category=='combine' & data$DataSet=='dict',]
dataQueryDict <- data[data$Category=='query' & data$DataSet=='dict',]
dataCombineURI <- data[data$Category=='combine' & data$DataSet=='URI',]
dataQueryURI <- data[data$Category=='query' & data$DataSet=='URI',]

plt <- ggplot(dataCombineDict, aes(x=Method, y=Mean, color=Name, shape=DataSet)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchCombineDict.png", plot=plt, dpi=320, width=8, height=4)

plt <- ggplot(dataCombineURI, aes(x=Method, y=Mean, color=Name, shape=DataSet)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchCombineURI.png", plot=plt, dpi=320, width=8, height=4)

plt <- ggplot(dataQueryDict, aes(x=Method, y=Mean, color=Name, shape=DataSet)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchQueryDict.png", plot=plt, dpi=320, width=6, height=6)

plt <- ggplot(dataQueryURI, aes(x=Method, y=Mean, color=Name, shape=DataSet)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_point()
ggsave(file="doc/benchQueryURI.png", plot=plt, dpi=320, width=6, height=6)


library(readr)
PKY <- read_delim("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

dim(PKY)
str(PKY)
summary(PKY)

library(ggplot2)
library(GGally)

#data is quite big, so quick summaries aren't very informative, looking first at factor correlation

correlations <- vapply(
  PKY[, c(-60)],
  function(x)
  {
    cor(PKY[, 59], x)
  },
  numeric(1)
)
plot(correlations[-59])
cor_PNY_max <- correlations[which.max(abs(correlations[-59]))]
cor_PNY_min <- correlations[which.min(correlations[-59])]
cor_PNY <- correlations[-59]



#Now plotting screened parameters for closer look

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

#In continous scale for visual purpouses, discrete would be probably correct
g = ggpairs(PKY,columns = c(4,14,15,56,58,59), lower = list(continuous = my_fn))
g


# Some ways to find big or small values
maxN <- function(x, N=5){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}
cor_PNY <- correlations[-59]
maxN(cor_PNY)

n <- length(cor_PNY)
sort(cor_PNY,partial=n-1)[n-1]

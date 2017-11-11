#Unorganized ideas
#Chapter 2
######################################################################
#Cool but futile

#Running correlations to all
correlations2 <- vapply(
  PKY[, c(-60)],
  function(x)
  {
    cor(PKY[, 59], x)
  },
  numeric(1)
)
plot(abs(correlations[-59]))
cor_PNY_max <- correlations[which.max(abs(correlations[-59]))]
cor_PNY_min <- correlations[which.min(correlations[-59])]
cor_PNY <- correlations[-59]
#

#To test correlation selection algorithm
R1 <- structure(c(1, 0.86, 0.56, 0.32, 0.85, 0.86, 1, 0.01, 0.74, 0.32, 
                  0.56, 0.01, 1, 0.65, 0.91, 0.32, 0.74, 0.65, 1, 0.36,
                  0.85, 0.32, 0.91, 0.36, 1), 
                .Dim = c(5L, 5L))
colnames(R1) <- rownames(R1) <- paste0("x", 1:ncol(R1))
R1

findCorrelation(R1, cutoff = .6, exact = FALSE)
R2 <- diag(rep(1, 5))
R2[2, 3] <- R2[3, 2] <- .7
R2[5, 3] <- R2[3, 5] <- -.7
R2[4, 1] <- R2[1, 4] <- -.67

corrDF <- expand.grid(row = 1:5, col = 1:5)
corrDF$correlation <- as.vector(R2)
levelplot(correlation ~ row + col, corrDF)

findCorrelation(R2, cutoff = .65, verbose = TRUE)

findCorrelation(R2, cutoff = .99, verbose = TRUE)


#Why not just run it with the colums found out here?
teest <-  matrix((correlations[correlations.selected,]), ncol=length(correlations[correlations.selected]), byrow=TRUE)
colnames(teest) <- colnames(correlations)[correlations.selected]


## Okay, new try.
correlations.selected <- findCorrelation(correlations, cutoff = .2, exact = FALSE)

#Not used
corr <- round(cor(PKY[-60]), 2)
hist(abs(corr))

#other
correlations2 <- cor(PKY[,c(1:59)], use="pairwise", method="spearman")

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


x <- readr::read_rds('paids-x-fees.Rds')


qq <- function(x, distribution, ...) {
  qdist_name <- paste0('q', distribution)
  qdist_func <- get(qdist_name)
  n <- length(x)
  r <- rank(x)
  p <- ppoints(n)[r]
  x_theoretical <- qdist_func(p = p, ...)
  
}
distribution <- 'gamma'



y <- c(
  622138, 
   18500, 
  302954, 
   88977, 
  440271, 
   84400, 
   50000, 
   12631, 
  100000, 
   52371)

order(y)
order(order(y))

y[order(y)]



n <- length(y)


ppoints(n)



x <- qnorm(ppoints(n))[order(order(y))]




yqnorm <- qqnorm(y, plot.it = FALSE)
O <- order(y)
yqnorm$x[O]
yqnorm$y[O]


ysorted <- sort(y)
qnorm(ppoints(n))



qqnorm.default <- function (
  y, ylim, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
    ylab = "Sample Quantiles", plot.it = TRUE, datax = FALSE, 
    ...) {
  
    if (has.na <- any(ina <- is.na(y))) {
        yN <- y
        y <- y[!ina]
    }
  
    if (0 == (n <- length(y))) 
        stop("y is empty or has only NAs")
  
    if (plot.it && missing(ylim)) {
      ylim <- range(y)
    }
      
    x <- qnorm(ppoints(n))[order(order(y))]
    
    if (has.na) {
        y <- x
        x <- yN
        x[!ina] <- y
        y <- yN
    }
    
    if (plot.it) 
        if (datax) 
            plot(y, x, main = main, xlab = ylab, ylab = xlab, 
                xlim = ylim, ...)
        else plot(x, y, main = main, xlab = xlab, ylab = ylab, 
            ylim = ylim, ...)
    invisible(if (datax) list(x = y, y = x) else list(x = x, 
        y = y))
}




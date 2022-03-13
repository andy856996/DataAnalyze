# ---------
#  30-3
# ---------
# ---------------------------------------------------------- #

simple.ex <- function(x, y)
{
   return(x * y)
}

# ---------------------------------------------------------- #
 
#' @title simple.ex
#' @description Simple Example
#' @details This is a simple example of a function
#' @aliases simple.ex
#' @author Jared P. Lander
#' @export simple.ex
#' @param x A numeric
#' @param y A second numeric
#' @return x times y
#' @examples
#' simple.ex(5, 3)
simple.ex <- function(x, y)
{
   return(x * y)
}

# ---------------------------------------------------------- #

#' @title print.myClass
#' @aliases print.myClass
#' @method print myClass
#' @S3method print myClass
#' @export print.myClass
#' @param x Simple object
#' @param ... Further arguments to be passed on
#' @return The top 5 rows of x
print.myClass <- function(x, ...)
{
   class(x) <- "list"
   x <- as.data.frame(x)
   print.data.frame(head(x, 5))
}

# ---------------------------------------------------------- #

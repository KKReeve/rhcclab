\name{f.rollingGaussMode}
\alias{f.rollingGaussMode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TODO%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f.rollingGaussMode(x, n, by.column = TRUE, weights = rep(1, n), normalize = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{by.column}{
%%     ~~Describe \code{by.column} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, n, by.column = TRUE, weights = rep(1, n), normalize = FALSE) 
{
    force(outFile)
    if (length(weights) != n) {
        stop("length of weights must equal n")
    }
    if (normalize) {
        weights <- weights * length(weights)/sum(weights)
    }
    if (is.matrix(x)) {
        if (n > nrow(x)) {
            stop("n cannot be greater than nrow(x)")
        }
        call <- call(paste(sep = "", name, "_matrix"), x, as.integer(n), 
            as.logical(by.column), as.numeric(weights))
        return(eval(call, envir = cpp_env))
    }
    if (is.vector(x)) {
        if (n > length(x)) {
            stop("n cannot be greater than length(x)")
        }
        call <- call(paste(sep = "", name, "_numeric"), x, as.integer(n), 
            as.numeric(weights))
        return(as.numeric(eval(call, envir = cpp_env)))
    }
    stop("the x supplied is neither a vector nor a matrix")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

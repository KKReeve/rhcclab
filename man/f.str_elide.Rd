\name{f.str_elide}
\alias{f.str_elide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TODO%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f.str_elide(s, length = 20, elideText = "...")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{length}{
%%     ~~Describe \code{length} here~~
}
  \item{elideText}{
%%     ~~Describe \code{elideText} here~~
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
function (s, length = 20, elideText = "...") 
{
    el = str_length(elideText)
    l = (length\%/\%2) - (el\%/\%2)
    s1 = str_sub(s, 1, l)
    s2 = str_sub(s, str_length(s) - (length - el - l) + 1, str_length(s))
    s12 = paste0(s1, elideText, s2)
    ifelse(str_length(s) > length, s12, s)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

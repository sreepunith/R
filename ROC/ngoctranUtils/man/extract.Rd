\name{extract}
\alias{extract}
sdfdsfdsfdsfdsfdsf
\title{
sdfdsfdsfdf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract(fit = NULL, field = NULL, field_idx = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{field}{
%%     ~~Describe \code{field} here~~
}
  \item{field_idx}{
%%     ~~Describe \code{field_idx} here~~
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
function (fit = NULL, field = NULL, field_idx = NULL) 
{
    filtered_output <- fit$pred[fit$pred[field] == field_idx, 
        ]
    roc_dt <- list()
    k <- 1
    for (resample in unique(filtered_output$Resample)) {
        roc_dt$predictions[[k]] <- apply(filtered_output[filtered_output$Resample == 
            resample, 3:4], 1, max)
        roc_dt$labels[[k]] <- filtered_output$obs[filtered_output$Resample == 
            resample]
        k <- k + 1
    }
    return(roc_dt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

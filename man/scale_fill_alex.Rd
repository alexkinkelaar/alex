\name{scale_fill_alex}
\alias{scale_fill_alex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill scale constructor
}
\description{
Fill scale constructor for custom color palette
}
\usage{
scale_fill_alex(palette = NULL, discrete = TRUE, reverse = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{palette}{
    Character name of custom color palette: "dark", "light", or "main". Defaults to "dark" for discrete scales (readable on the theme_alex() background) and "main" for continuous scales. Use "light" on white backgrounds.
  }
  \item{discrete}{
    Boolean input as to whether color palette should be discrete or not
  }
  \item{reverse}{
    Boolean input to reverse the ordering of the returned color palette
  }
  \item{...}{
    Additional arguments passed to the underlying ggplot2 scale
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
library(ggplot2)
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  geom_bar() +
  scale_fill_alex() +
  theme_alex() +
  xlab("Cylinders") +
  ylab("Count") +
  labs(title = "Cars by Cylinder Count")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

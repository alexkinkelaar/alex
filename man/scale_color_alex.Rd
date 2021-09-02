\name{scale_color_alex}
\alias{scale_color_alex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Color scale constructor
}
\description{
Color scale constructor for custom color palette.
}
\usage{
scale_color_alex(palette = "main", discrete = TRUE, reverse = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{palette}{
    Character name of custom color palette
  }
  \item{discrete}{
    Boolean input as to whether color palette should be discrete or not
  }
  \item{reverse}{
    Boolean input to reverse the ordering of the returned color palette
  }
  \item{...}{
    Additional arguments to pass to palette_alex()
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
mtcars \%>\% ggplot(aes(x = hp, y = mpg, color = as.factor(cyl))) +
   geom_line(size = 1.5, alpha = 0.5) +
   scale_color_alex() +
   theme_alex() +
   xlab("Horsepower") +
   ylab("Miles per Gallon") +
   labs(
       title = "Miles per Gallon vs Horsepower"
   )
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

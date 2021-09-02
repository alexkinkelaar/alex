\name{theme_alex}
\alias{theme_alex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Custom theme for ggplot
}
\description{
Dark theme built off of theme_void.
}
\usage{
theme_alex()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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

Will format the plot with a customized theme.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\docType{methods}
\name{summary-method}
\alias{summary,ConfirmatoryClass-method}
\alias{summary,ExploratoryClass-method}
\alias{summary,MixedClass-method}
\alias{summary,MultipleGroupClass-method}
\alias{summary-method}
\title{Summary of model object}
\usage{
\S4method{summary}{ExploratoryClass}(object, rotate = "", Target = NULL,
  suppress = 0, digits = 3, printCI = FALSE, verbose = TRUE, ...)
}
\arguments{
  \item{object}{an object of class \code{ExploratoryClass},
  \code{ConfirmatoryClass}, \code{MultipleGroupClass}, or
  \code{MixedClass}}

  \item{rotate}{see \code{\link{mirt}} for details}

  \item{Target}{a dummy variable matrix indicting a target
  rotation pattern}

  \item{suppress}{a numeric value indicating which
  (possibly rotated) factor loadings should be suppressed.
  Typical values are around .3 in most statistical
  software. Default is 0 for no suppression}

  \item{printCI}{print a confidence interval for
  standardized loadings (e.g., \code{printCI = .95} gives a
  95\% confidence interval)}

  \item{digits}{number of significant digits to be rounded}

  \item{verbose}{logical; allow information to be printed
  to the console?}

  \item{...}{additional arguments to be passed}
}
\description{
Transforms coefficients into a standardized factor
loading's metric. For \code{MixedClass} objects, the fixed
and random coefficients are printed.
}
\examples{
\dontrun{
x <- mirt(Science, 2)
summary(x)
summary(x, rotate = 'varimax')

#print confidence interval (requires computed information matrix)
x2 <- mirt(Science, 1, SE=TRUE)
summary(x2, printCI=.95)
}
}
\seealso{
\code{\link{coef-method}}
}


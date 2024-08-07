% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability.R
\name{calculate_stability}
\alias{calculate_stability}
\alias{calculate_stability.2d_Isingland}
\alias{calculate_stability.2d_Isingland_matrix}
\title{Calculate the stability metrics for Ising landscapes}
\usage{
calculate_stability(l, ...)

\method{calculate_stability}{`2d_Isingland`}(l, split_value = 0.5 * l$Nvar, ...)

\method{calculate_stability}{`2d_Isingland_matrix`}(l, split_value = 0.5 * l$Nvar, ...)
}
\arguments{
\item{l}{An \code{Isingland} object constructed with \code{\link[=make_2d_Isingland]{make_2d_Isingland()}} or \code{\link[=make_2d_Isingland_matrix]{make_2d_Isingland_matrix()}}.}

\item{...}{Not in use.}

\item{split_value}{An integer to specify the number of active nodes used to split two stability ranges. Default is half of the number of nodes.}
}
\value{
\describe{
\item{\code{\link[=calculate_stability.2d_Isingland]{calculate_stability.2d_Isingland()}}}{Returns a \code{calculate_stability.2d_Isingland} project, which contains the following elements:
\describe{
\item{dist}{The distribution tibble which is the same as in the input \code{l}.}
\item{effective_minindex1,effective_maxindex1,effective_minindex2,effective_maxindex2}{The (row)indices in \code{dist} that were used as the positions of the local minimums and maximums in two parts.}
\item{stability1,stability2,stability_diff}{The stability measures for the first (left) part, the second part (right), and their difference.}
}
}
\item{\code{\link[=calculate_stability.2d_Isingland_matrix]{calculate_stability.2d_Isingland_matrix()}}}{Returns a \code{stability_2d_Isingland_matrix} object, which is a tibble containing columns of the varying parameters and a column \code{stability} of the \code{calculate_stability.2d_Isingland} objects for each landscape.}
}

When \code{print()}ed, a verbal description of the stability metrics is shown. Use the \code{summary()} method for a tidy version of the outputs.
}
\description{
The stability is calculated based on the shape of the potential landscape and the prior knowledge about the qualitatively different parts of the system. Two stability indicators are calculated separately, and their difference is used to represent a general stability of the system in favor of the first phase. Within each phase, the potential difference between the local maximum and the local minimum (if multiple minimums exist, use the one that is further from the other phase; and the local maximum should always be on the side to the other phase) is used to represent the stability of this phase.
}

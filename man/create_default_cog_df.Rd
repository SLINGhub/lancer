% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cognostics.R
\name{create_default_cog_df}
\alias{create_default_cog_df}
\title{Create Default Cognostics Data Frame}
\usage{
create_default_cog_df()
}
\value{
A dataframe of default cognostics information
}
\description{
Create default cognostics data frame to be used to
convert columns in \code{curve_summary} to class cognostics
}
\details{
Internal function to create a dataframe of default cognostics
information to be used by the \code{trelliscopejs::cog}.
}
\examples{
cog_df <- create_default_cog_df()
cog_df
}

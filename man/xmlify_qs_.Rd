% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manifesto_to_xml.R
\name{xmlify_qs_}
\alias{xmlify_qs_}
\title{XML-ify manifesto quasi-sentences bloc}
\usage{
xmlify_qs_(x)
}
\arguments{
\item{x}{rows of \code{manifesto.df} with role 'qs'}
}
\value{
A character vector, elements are XML-formatted
    Out-most and only tag named 'quasi-sentence' (w/ attribute 'nr' and 'cmp-code' if CMP code is not \code{NA}).
}
\description{
\code{manifesto_df_to_xml} helper
}
